namespace bfcfs

module Optimizer =
    let private (|OptimizeOffsetAdd|_|) = function
        | PointerAdd (pInc) :: DataAdd (dInc) :: PointerSub (pDec) :: _ when pInc = pDec ->
           Some (pInc, dInc)
        | PointerSub (pDec) :: DataAdd (dInc) :: PointerAdd (pInc) :: _ when pInc = pDec ->
            Some (-pDec, dInc)
        | _ -> None

    let private (|OptimizeOffsetDec|_|) = function
        | PointerAdd (pInc) :: DataSub (dDec) :: PointerSub (pDec) :: _ when pInc = pDec ->
           Some (pInc, dDec)
        | PointerSub (pDec) :: DataSub (dDec) :: PointerAdd (pInc) :: _ when pInc = pDec ->
            Some (-pDec, dDec)
        | _ -> None

    let private (|OptimizeOffsetRead|_|) = function
        | PointerAdd (pInc) :: ReadChar :: PointerSub (pDec) :: _ when pInc = pDec ->
           Some (pInc)
        | PointerSub (pDec) :: ReadChar :: PointerAdd (pInc) :: _ when pInc = pDec ->
            Some (-pDec)
        | _ -> None

    let private (|OptimizeOffsetWrite|_|) = function
        | PointerAdd (pInc) :: WriteChar :: PointerSub (pDec) :: _ when pInc = pDec ->
           Some (pInc)
        | PointerSub (pDec) :: WriteChar :: PointerAdd (pInc) :: _ when pInc = pDec ->
            Some (-pDec)
        | _ -> None

    let private (|OptimizeZero|_|) = function
        | LoopStart :: DataSub (1) :: LoopEnd :: _
        | LoopStart :: DataSub (1) :: LoopEnd :: _ -> Some ()
        | _ -> None

    let private (|OptimizeOffsetZero|_|) = function
        | PointerAdd (pInc) :: LoopStart :: DataSub (1) :: LoopEnd :: PointerSub (pDec) :: _
        | PointerSub (pDec) :: LoopStart :: DataSub (1) :: LoopEnd :: PointerAdd (pInc) :: _
        | PointerAdd (pInc) :: LoopStart :: DataAdd (1) :: LoopEnd :: PointerSub (pDec) :: _
        | PointerSub (pDec) :: LoopStart :: DataAdd (1) :: LoopEnd :: PointerAdd (pInc) :: _
            when pInc = pDec -> Some (pInc)
        | _ -> None

    let private (|OptimizeAssign|_|) = function
        | LoopStart :: DataSub (1) :: LoopEnd :: DataAdd (n) :: _
        | LoopStart :: DataAdd (1) :: LoopEnd :: DataAdd (n) :: _ -> Some (n)
        | LoopStart :: DataSub (1) :: LoopEnd :: DataSub (n) :: _
        | LoopStart :: DataAdd (1) :: LoopEnd :: DataSub (n) :: _ -> Some (-n)
        | _ -> None

    let private (|OptimizeOffsetAssign|_|) = function
        | PointerAdd (pInc) :: LoopStart :: DataSub (1) :: LoopEnd :: DataAdd (n) :: PointerSub (pDec) :: _
           when pInc = pDec -> Some (pInc, n)
        | PointerSub (pDec) :: LoopStart :: DataAdd (1) :: LoopEnd :: DataAdd (n) :: PointerAdd (pInc) :: _
           when pInc = pDec -> Some (-pDec, n)
        | PointerAdd (pInc) :: LoopStart :: DataSub (1) :: LoopEnd :: DataSub (n) :: PointerSub (pDec) :: _
           when pInc = pDec -> Some (pInc, n)
        | PointerSub (pDec) :: LoopStart :: DataAdd (1) :: LoopEnd :: DataSub (n) :: PointerAdd (pInc) :: _
            when pInc = pDec -> Some (-pDec, -n)
        | _ -> None

    type private PointerOpsUtil = Operation of Token | PointerOffset of int

    // Here be dragons.
    // This optimization eliminates unnecessary pointer operations by delaying
    // pointer movement operations as much as possible by converting normal
    // operations into ones with offset. It has a pretty major impact; for 
    // example, it makes the mandelbrot benchmark over 2x faster.
    let private (|EliminatePointerOps|_|) (tokens : Token list) =
        let takePredicate = function
            | PointerAdd _ | PointerSub _
            | DataAdd _    | DataSub _
            | DataZero     | Assign _
            | OffsetAdd _  | OffsetSub _ -> true
            | _ -> false

        if List.isEmpty tokens || (takePredicate >> not) (List.head tokens) then None else

        let rec f inputTokens outputTokens pointerOffset =
            if List.isEmpty inputTokens then (List.rev outputTokens, pointerOffset) else
                let result =
                    match List.head inputTokens with
                    | DataAdd n -> Operation <| if pointerOffset = 0 then DataAdd n else OffsetAdd    (pointerOffset, n)
                    | DataSub n -> Operation <| if pointerOffset = 0 then DataSub n else OffsetSub    (pointerOffset, n)
                    | DataZero  -> Operation <| if pointerOffset = 0 then DataZero  else OffsetZero   (pointerOffset)
                    | Assign n  -> Operation <| if pointerOffset = 0 then Assign n  else OffsetAssign (pointerOffset, n)
                    | OffsetAdd (o, n) -> Operation <| OffsetAdd (pointerOffset + o, n)
                    | OffsetSub (o, n) -> Operation <| OffsetSub (pointerOffset + o, n)
                    | PointerAdd n -> PointerOffset n
                    | PointerSub n -> PointerOffset -n
                    | head -> failwith <| sprintf "Invalid token in EliminatePointerOps: %A" head
                match result with
                | Operation newToken   -> f (List.tail inputTokens) (newToken :: outputTokens) (pointerOffset)
                | PointerOffset offset -> f (List.tail inputTokens) (outputTokens) (pointerOffset + offset)

        let optimizable = List.takeWhile takePredicate tokens
        let results = f optimizable List.empty 0
        let consumedTokens = List.length optimizable

        match results with
        | (tokens, offset) when List.length tokens < 2 -> None
        | (tokens, 0)                                  -> Some (Many tokens, consumedTokens)
        | (tokens, offset)                             -> Some (Many (tokens @ [PointerAdd offset]), consumedTokens)

    // DetectRegionZero merges multiple Zeros into a RegionZero, which is
    // compiled into an Array.Clear call.
    // However, in all benchmarks I've done so far Array.Clear has had too much
    // overhead to actually improve the performance.
    // Therefore, this optimization is not currently enabled.
    let private (|DetectRegionZero|_|) (tokens : Token list) =
        match tokens with
        | OffsetZero a :: OffsetZero b :: _ when b = a + 1 ->
            let count =
                Seq.skip 2 tokens
                |> Seq.zip (Seq.skip 1 tokens)
                |> Seq.takeWhile (function (OffsetZero prev, OffsetZero next) -> next = prev + 1 | _ -> false)
                |> Seq.length
                |> ((+) 2)
            if count > 2 then Some (a, count) else None
        | _ -> None

    let private optimizerPass pass = function
        // Merge adds/subs + NOP zero adds/subs
        | DataAdd a :: DataAdd b :: _     -> Some (DataAdd (a + b), 2)
        | DataSub a :: DataSub b :: _     -> Some (DataSub (a + b), 2)
        | DataAdd add :: DataSub sub :: _
        | DataSub sub :: DataAdd add :: _ -> Some(DataAdd (add - sub), 2)
        | DataAdd n :: _
        | DataSub n :: _ when n = 0       -> Some(Nop, 1)

        // Same optimization for pointers
        | PointerAdd a :: PointerAdd b :: _     -> Some (PointerAdd (a + b), 2)
        | PointerSub a :: PointerSub b :: _     -> Some (PointerSub (a + b), 2)
        | PointerAdd add :: PointerSub sub :: _
        | PointerSub sub :: PointerAdd add :: _ -> Some(PointerAdd (add - sub), 2)
        | PointerAdd n :: _
        | PointerSub n :: _ when n = 0          -> Some(Nop, 1)

        | OffsetAdd (offsetA, incA) :: OffsetAdd (offsetB, incB) :: _
            when offsetA = offsetB        -> Some(OffsetAdd (offsetA, incA + incB), 2)
        | OffsetSub (offsetA, decA) :: OffsetSub (offsetB, decB) :: _
            when offsetA = offsetB        -> Some(OffsetSub (offsetA, decA + decB), 2)

        | EliminatePointerOps (tokens, skip) -> Some (tokens, skip)
        | OptimizeOffsetAdd (offset, inc) -> Some(OffsetAdd (offset, inc), 3)
        | OptimizeOffsetDec (offset, dec) -> Some(OffsetSub (offset, dec), 3)
        | OptimizeOffsetRead (offset)     -> Some(OffsetRead offset, 3)
        | OptimizeOffsetWrite (offset)    -> Some(OffsetWrite offset, 3)
        | OptimizeOffsetAssign (offset, n)-> Some(OffsetAssign (offset, n), 6)
        | OptimizeOffsetZero (offset)     -> Some(OffsetZero offset, 5)
        | OptimizeAssign n                -> Some(Assign (n), 4)
        | OptimizeZero                    -> Some(DataZero, 3)
        | (token :: _)                    -> Some (token, 1)
        | []                              -> None

    let rec private optimize pass tokens =
        let result = optimizerPass pass tokens

        match result with
        | None                    -> List.empty
        | Some (Nop, 1)           -> optimize pass (List.skip 1 tokens)
        | Some (Many newTokens, consumedN) -> newTokens @ optimize pass (List.skip consumedN tokens)
        | Some (token, consumedN) -> token :: optimize pass (List.skip consumedN tokens)

    let rec private fullOptimize pass tokens =
        let optimized = optimize pass tokens

        if tokens = optimized then
            (tokens, pass)
        else
            fullOptimize (pass + 1) optimized

    let rec FullOptimize = fullOptimize 1