namespace bfcfs

open System.IO

module Parser =
    type StringReader with
        member this.PeekChar = char(this.Peek ())
        member this.HasNext = this.Peek () <> -1
        member this.NextChar = char(this.Read ())
        member this.TakeWhile f = seq {while this.HasNext && f this.PeekChar do yield this.NextChar} |> Seq.toList

    let Parse mergeTokens (source : string) : Token seq =
        let reader = new StringReader (source)
        let mergeOf c = if mergeTokens then 1 + (List.length <| reader.TakeWhile ((=) c)) else 1

        seq {
            while reader.HasNext do
                match reader.NextChar with
                | '+' -> yield DataAdd    <| mergeOf '+'
                | '-' -> yield DataSub    <| mergeOf '-'
                | '>' -> yield PointerAdd <| mergeOf '>'
                | '<' -> yield PointerSub <| mergeOf '<'
                | '.' -> yield WriteChar
                | ',' -> yield ReadChar
                | '[' -> yield LoopStart
                | ']' -> yield LoopEnd
                | ';' -> ignore <| reader.TakeWhile (fun c -> c <> '\n' && c <> '\r')
                | _   -> ()
            reader.Dispose ()
        }