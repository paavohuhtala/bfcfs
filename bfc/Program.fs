open System
open System.IO
open bfcfs

type private OptimizeSetting = Disabled | ParseTime | MultiPass
type private Arguments = {source: string; verbose: bool; optimize: OptimizeSetting; arraySize: int; saveIr: bool; output: string option}
let private defaultArgs = {source = ""; verbose = false; optimize = MultiPass; arraySize = 30000; saveIr = false; output = None}

type private ParseResults = Success of Arguments | Failure of string

let rec private parseArguments options = function
    | [] -> Failure "No source file supplied."
    | source :: [] ->
        let sourcePath = FileInfo source
        if not sourcePath.Exists then
            Failure <| sprintf "Source file '%s' doesn't exist." sourcePath.FullName
        else
            Success <| {options with source = sourcePath.FullName}
    | "-v" :: rest | "--verbose"  :: rest -> parseArguments {options with verbose = true} rest
    | "--optimize" :: level :: rest ->
        let optimizeLevel =
            match level with
            | "none"    -> Some Disabled
            | "some"    -> Some ParseTime
            | "full"    -> Some MultiPass
            | unknown   -> None
        if optimizeLevel.IsNone then
            Failure <| sprintf "Unknown --optimize level: %s." level
        else
            parseArguments {options with optimize = optimizeLevel.Value} rest
    | "-O0" :: rest -> parseArguments {options with optimize = Disabled}  rest
    | "-O1" :: rest -> parseArguments {options with optimize = ParseTime} rest
    | "-O2" :: rest -> parseArguments {options with optimize = MultiPass} rest
    | "-o" :: output :: rest | "--output" :: output :: rest ->
        let outputPath = FileInfo output
        let outputDirectory = outputPath.Directory
        if not outputDirectory.Exists then outputDirectory.Create ()
        parseArguments {options with output = Some outputPath.FullName} rest
    | "-ir" :: rest | "--save-ir" :: rest -> parseArguments {options with saveIr = true} rest
    | "-as" :: size :: rest | "--array-size" :: size :: rest ->
        let (|Int|_|) i =
            let valid, result = Int32.TryParse i
            if valid then Some result else None

        match size with
        | Int i -> parseArguments {options with arraySize = i} rest
        | _     -> Failure <| sprintf "Array size must be an integer (it was %s)." size
    | unknown :: rest -> Failure <| sprintf "Unknown compiler argument: %s." unknown

let private printUsage () =
    let linesOfLength maxLength (message : string) =
        let words = message.Split [|' '|] |> Array.toList

        let rec takeLine (words : String list) current length =
            match words with
            | [] ->
                (String.concat " " <| List.rev current, [])
            | word :: rest when (length + word.Length) <= maxLength ->
                takeLine rest (word :: current) (length + word.Length + 1)
            | word :: rest ->
                 (String.concat " " <| List.rev current, words)

        let rec splitLines (words : String list) lines =
            if List.isEmpty words then lines else
                let (line, rest) = takeLine words [] 0
                splitLines rest (lines @ [line])

        splitLines words []


    let outputDescription =
        "Set output file path. If the directory of the file doesn't exist, the compiler \
        attemps to create it. If output path isn't specified, the compiler saves \
        the compiled assembly to the directory of the source file as (source-file).exe."

    let saveIrDescription =
        "Save the optimized immediate representation of the program as (output-file).bfir."

    let arraySizeDescription =
        sprintf "Set the data array size of the generated assembly. The default is %i." defaultArgs.arraySize

    let printNested command = printfn "%4s%-20s%2s%-53s" "" command ""

    let options = (["-v";"--verbose"], "Print more stuff", []) ::
                  (["--optimize (level)"], "Set optimization level: ",
                   [("none (-O0)", "Disable all optimizations.")
                    ("some (-O1)", "Enable parse-time optimization.")
                    ("full (-O2)", "Enable multi-pass optimization. (default)")]) ::
                  (["-o";"--output (path)"], outputDescription, []) ::
                  (["-ir";"--save-ir"], saveIrDescription, []) ::
                  (["-as";"--array-size"], arraySizeDescription, []) :: []

    printfn "bfc 0.1"
    printfn "An optimizing Brainfuck to CLR compiler."
    printfn "bfc [options] (source-file)"
    printfn "Options:"

    for (commands, description, subCommands) in options do
        let parts = linesOfLength 55 description
        printfn "%2s%-20s%2s%-55s" "" (String.concat ", " commands) "" (List.head parts)
        for part in (List.tail parts) do
            printfn "%24s%-54s" "" part
        for (subCommand, subDescription) in subCommands do
            printfn "%4s%-20s%2s%-53s" "" subCommand "" subDescription

[<EntryPoint>]
let main argv =
    if (Array.isEmpty argv || Array.contains "--help" argv || Array.contains "-h" argv) then
        printUsage ()
        0
    else

    let arguments = Array.toList argv |> parseArguments defaultArgs

    match arguments with
    | Failure message ->
        printf "Parameter error: %s\n" message
        1
    | Success args ->
        let verbosePrint0     = if args.verbose then printfn     else ignore
        let verbosePrint1 a   = if args.verbose then printfn a   else ignore
        let verbosePrint2 a b = if args.verbose then printfn a b else ignore

        let timer = System.Diagnostics.Stopwatch ()
        let sourceFile = args.source

        timer.Start ()
        printfn "Reading %s..." sourceFile

        let source = File.ReadAllText sourceFile
        timer.Stop ()
        verbosePrint1 "Read in %i ms." timer.ElapsedMilliseconds

        let output =
            match args.output with
            | Some file -> file
            | None      -> [|Path.GetDirectoryName sourceFile; Path.GetFileNameWithoutExtension sourceFile|]
                           |> Path.Combine
        let outputDir = Path.GetDirectoryName output
        let outputName = Path.GetFileNameWithoutExtension output

        match args.optimize with
        | Disabled  -> verbosePrint0 "Optimization is disabled."
        | ParseTime -> verbosePrint0 "Using parse-time optimization."
        | MultiPass -> verbosePrint0 "Using multi-pass optimization."

        let parse = Parser.Parse <| match args.optimize with
                                    | Disabled -> false
                                    | _        -> true

        let optimize =
            match args.optimize with
            | MultiPass -> Optimizer.FullOptimize
            | _         -> fun tokens -> (tokens, -1)

        let parsed = parse source |> Seq.toList
        timer.Stop ()
        verbosePrint1 "Parsed in %i ms." timer.ElapsedMilliseconds

        let printOptimize = (function MultiPass -> true | _ -> false) args.optimize

        timer.Restart ()
        let (optimized, passes) = optimize parsed
        timer.Stop ()
        if printOptimize then
            verbosePrint1 "Optimized in %i ms." timer.ElapsedMilliseconds

        match args.optimize with
        | MultiPass ->
            let reduction = 1.0 - float(List.length optimized) / float(List.length parsed) |> (*) 100.0
            verbosePrint2 "Optimizer reduced tokens by %.2f%% in %i passes." reduction passes
        | _ -> ()

        let pathBase = Path.Combine [|outputDir; outputName|]

        if args.saveIr then
            let irPath = sprintf "%s.bfir" <| pathBase
            File.WriteAllLines (irPath, [for token in optimized do yield sprintf "%A" token])
            printfn "Saved IR to %s." irPath

        timer.Restart ()
        let compiled = Compiler.CompileAssembly args.arraySize outputName optimized
        timer.Stop ()
        verbosePrint1 "Compiled in %i ms." timer.ElapsedMilliseconds

        // AssemblyBuilder.Save doesn't take a path, only a file name.
        // This is a stupid workaround.
        let oldCurrentDir = Environment.CurrentDirectory
        Environment.CurrentDirectory <- outputDir
        compiled.Save <| sprintf "%s.exe" outputName
        Environment.CurrentDirectory <- oldCurrentDir

        printfn "Saved to %s.exe." <| pathBase
        0
