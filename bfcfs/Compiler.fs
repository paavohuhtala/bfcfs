namespace bfcfs

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit

module Compiler =
    let private compile (arraySize : int) (gen : ILGenerator) tokens =
        // Define data array
        let dataArray = gen.DeclareLocal typeof<byte array>
        gen.Emit (OpCodes.Ldc_I4, arraySize)
        gen.Emit (OpCodes.Newarr, typeof<byte>)
        gen.Emit (OpCodes.Stloc, dataArray)

        // Define data pointer
        let dataPointer = gen.DeclareLocal typeof<int>
        gen.Emit OpCodes.Ldc_I4_0
        gen.Emit (OpCodes.Stloc, dataPointer)

        let loopStarts = Stack<Label> ()
        let loopEnds = Stack<Label> ()

        let loadArray    () = gen.Emit (OpCodes.Ldloc, dataArray)
        let loadPointer  () = gen.Emit (OpCodes.Ldloc, dataPointer)
        let storePointer () = gen.Emit (OpCodes.Stloc, dataPointer)

        let loadAtPointer () =
            loadArray ()
            loadPointer ()
            gen.Emit OpCodes.Ldelem_U1

        let writeChar = typeof<Console>.GetMethod ("Write", [|typeof<char>|])
        let readChar  = typeof<Console>.GetMethod "Read"
        let clear     = typeof<Array>.GetMethod "Clear"

        // We don't cache (pointer + offset) here, since computing it twice
        // gives better performance (for some reason)
        let offsetOp (offset : int) (n : int) op =
            loadArray ()
            loadPointer ()
            gen.Emit (OpCodes.Ldc_I4, offset)
            gen.Emit OpCodes.Add
            loadArray ()
            loadPointer ()
            gen.Emit (OpCodes.Ldc_I4, offset)
            gen.Emit OpCodes.Add
            gen.Emit OpCodes.Ldelem_U1
            gen.Emit (OpCodes.Ldc_I4, n)
            gen.Emit op
            gen.Emit (OpCodes.Stelem, typeof<byte>)

        let compileToken = function
            | OffsetAdd (offset, n) ->
                offsetOp offset n OpCodes.Add
            | OffsetSub (offset, n) ->
                offsetOp offset n OpCodes.Sub
            | PointerAdd n ->
                loadPointer ()
                gen.Emit (OpCodes.Ldc_I4, n)
                gen.Emit OpCodes.Add
                storePointer ()
            | PointerSub n ->
                loadPointer ()
                gen.Emit (OpCodes.Ldc_I4, n)
                gen.Emit OpCodes.Sub
                storePointer ()
            | DataAdd n ->
                loadArray ()
                loadPointer ()
                loadAtPointer ()
                gen.Emit (OpCodes.Ldc_I4, n)
                gen.Emit OpCodes.Add
                gen.Emit (OpCodes.Stelem, typeof<byte>)
            | DataSub n ->
                loadArray ()
                loadPointer ()
                loadAtPointer ()
                gen.Emit (OpCodes.Ldc_I4, n)
                gen.Emit OpCodes.Sub
                gen.Emit (OpCodes.Stelem, typeof<byte>)
            | OffsetAssign (offset, n) ->
                loadArray ()
                loadPointer ()
                gen.Emit (OpCodes.Ldc_I4, offset)
                gen.Emit OpCodes.Add
                gen.Emit (OpCodes.Ldc_I4, n)
                gen.Emit (OpCodes.Stelem, typeof<byte>)
            | Assign n ->
                loadArray ()
                loadPointer ()
                gen.Emit (OpCodes.Ldc_I4, n)
                gen.Emit (OpCodes.Stelem, typeof<byte>)
            | OffsetZero offset ->
                loadArray ()
                loadPointer ()
                gen.Emit (OpCodes.Ldc_I4, offset)
                gen.Emit OpCodes.Add
                gen.Emit OpCodes.Ldc_I4_0
                gen.Emit (OpCodes.Stelem, typeof<byte>)
            | RegionZero (offset, n) ->
                loadArray ()
                loadPointer ()
                gen.Emit (OpCodes.Ldc_I4, offset)
                gen.Emit OpCodes.Add
                gen.Emit (OpCodes.Ldc_I4, n)
                gen.Emit (OpCodes.Call, clear)
            | DataZero ->
                loadArray ()
                loadPointer ()
                gen.Emit OpCodes.Ldc_I4_0
                gen.Emit (OpCodes.Stelem, typeof<byte>)
            | LoopStart ->
                loopStarts.Push <| gen.DefineLabel ()
                loopEnds.Push   <| gen.DefineLabel ()
                loadAtPointer ()
                gen.Emit (OpCodes.Brfalse, loopEnds.Peek ())
                gen.MarkLabel <| loopStarts.Peek ()
            | LoopEnd ->
                loadAtPointer ()
                gen.Emit (OpCodes.Brtrue, loopStarts.Pop ())
                gen.MarkLabel <| loopEnds.Pop ()
            | OffsetWrite offset ->
                loadArray ()
                loadPointer ()
                gen.Emit (OpCodes.Ldc_I4, offset)
                gen.Emit OpCodes.Add
                gen.Emit OpCodes.Ldelem_U1
                gen.Emit (OpCodes.Call, writeChar)
            | OffsetRead offset ->
                loadArray ()
                loadPointer ()
                gen.Emit (OpCodes.Ldc_I4, offset)
                gen.Emit OpCodes.Add
                loadArray ()
                gen.Emit (OpCodes.Ldc_I4, offset)
                gen.Emit OpCodes.Add
                gen.Emit (OpCodes.Call, readChar)
                gen.Emit OpCodes.Conv_U1
                gen.Emit (OpCodes.Stelem, typeof<byte>)
            | WriteChar ->
                loadAtPointer ()
                gen.Emit (OpCodes.Call, writeChar)
            | ReadChar ->
                loadArray ()
                loadPointer ()
                gen.Emit (OpCodes.Call, readChar)
                gen.Emit OpCodes.Conv_U1
                gen.Emit (OpCodes.Stelem, typeof<byte>)

        Seq.map compileToken tokens |> Seq.toList |> ignore

        gen.Emit OpCodes.Ret

    let CompileAssembly arraySize name tokens : AssemblyBuilder =
        let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly (AssemblyName (name), AssemblyBuilderAccess.RunAndSave)
        let moduleBuilder = assemblyBuilder.DefineDynamicModule (name, name + ".exe")
        let typeBuilder = moduleBuilder.DefineType (name + ".Program", (TypeAttributes.Class ||| TypeAttributes.Public))
        let methodBuilder = typeBuilder.DefineMethod ("Main", MethodAttributes.Public ||| MethodAttributes.Static, typeof<Void>, [|typeof<string array>|])
        let gen = methodBuilder.GetILGenerator ()

        compile arraySize gen tokens

        typeBuilder.CreateType () |> ignore
        assemblyBuilder.SetEntryPoint (methodBuilder, PEFileKinds.ConsoleApplication)
        assemblyBuilder

    type BrainfuckDelegate = delegate of unit -> unit

    let CompileMethod arraySize tokens : BrainfuckDelegate =
        let dynamicMethod = new DynamicMethod ("Evaluate", typeof<Void>, Array.empty)
        let gen = dynamicMethod.GetILGenerator ()

        compile arraySize gen tokens

        dynamicMethod.CreateDelegate (typeof<BrainfuckDelegate>) :?> BrainfuckDelegate
