# bfcfs

**bfcfs** is an optimizing compiler for the [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) programming language, targeting the [Common Language Runtime](https://en.wikipedia.org/wiki/Common_Language_Runtime).
It utilizes multiple optimizations techniques, including most of the ones detailed in [these](http://www.nayuki.io/page/optimizing-brainfuck-compiler) [two](http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html) articles.
The compiler produces dependency-free (apart from mscorlib), PEVerify-able .NET assemblies, which can be run on .NET and Mono.

The compiler is written in [F#](https://en.wikipedia.org/wiki/F_Sharp_%28programming_language%29) 4.0, and it has no third-party dependencies.
However, it utilizes System.Reflection.Emit for bytecode generation, which is not currently (2015-06-06) available on .NET Core.
It's split into two modules: **bfcfs**, the compiler, and **bfc**, the command line interface.
bfcfs has a public API, and it can be used separately.

The implemented version of Brainfuck is fairly standard. The parser ignores all non-command characters, but it also  supports the use of a semicolon as a comment marker; all of the characters between a ; and the next newline are ignored.
The size of the data array is fixed, but configurable at compile time (30,000 by default). The cells are 8-bit unsigned integers.

## --help

```
$ ./bfc --help
bfc 0.1
An optimizing Brainfuck to CLR compiler.
bfc [options] (source-file)
Options:
  -v, --verbose         Print more stuff
  --optimize (level)    Set optimization level:
    none (-O0)            Disable all optimizations.
    some (-O1)            Enable parse-time optimization.
    full (-O2)            Enable multi-pass optimization. (default)
  -o, --output (path)   Set output file path. If the directory of the file
                        doesn't exist, the compiler attemps to create it. If
                        output path isn't specified, the compiler saves the
                        compiled assembly to the directory of the source file
                        as (source-file).exe.
  -ir, --save-ir        Save the optimized immediate representation of the
                        program as (output-file).bfir.
  -as, --array-size     Set the data array size of the generated assembly. The
                        default is 30000.

```

## Thanks

This repository contains a mandelbrot viewer written by **[Erik Bosman](http://esoteric.sange.fi/brainfuck/utils/mandelbrot/)**.
It has been really useful during development, especially for benchmarking.

## License

All of my code in this project is licensed under the **[MIT License](http://opensource.org/licenses/MIT)**.
