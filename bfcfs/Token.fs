namespace bfcfs

type Token =
      PointerAdd of int
    | PointerSub of int
    | DataAdd of int
    | DataSub of int
    | WriteChar
    | ReadChar
    | LoopStart
    | LoopEnd
    | DataZero
    | Assign of int
    | OffsetAssign of (int * int)
    | OffsetAdd of (int * int)
    | OffsetSub of (int * int)
    | OffsetRead of int
    | OffsetWrite of int
    | OffsetZero of int
    | RegionZero of (int * int)
    | Many of Token list
    | Nop
