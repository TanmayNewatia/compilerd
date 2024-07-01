export const languageOptions = [
  {
    "id": 63,
    "name": "JavaScript (Node.js 12.14.0)",
    "label": "JavaScript (Node.js 12.14.0)",
    "value": "javascript",
    "defaultCode": "console.log('Hello, World!');"
  },
  {
    "id": 45,
    "name": "Assembly (NASM 2.14.02)",
    "label": "Assembly (NASM 2.14.02)",
    "value": "assembly",
    "defaultCode": "; Assembly (NASM 2.14.02)\nsection .data\n    msg db 'Hello, World!', 0\n\nsection .text\n    global _start\n\n_start:\n    ; write our string to stdout\n    mov eax, 4\n    mov ebx, 1\n    mov ecx, msg\n    mov edx, 13\n    int 0x80\n\n    ; exit\n    mov eax, 1\n    xor ebx, ebx\n    int 0x80"
  },
  {
    "id": 46,
    "name": "Bash (5.0.0)",
    "label": "Bash (5.0.0)",
    "value": "bash",
    "defaultCode": "echo \"Hello, World!\""
  },
  {
    "id": 47,
    "name": "Basic (FBC 1.07.1)",
    "label": "Basic (FBC 1.07.1)",
    "value": "basic",
    "defaultCode": "PRINT \"Hello, World!\""
  },
  {
    "id": 50,
    "name": "C (GCC 9.2.0)",
    "label": "C (GCC 9.2.0)",
    "value": "c",
    "defaultCode": "#include <stdio.h>\n\nint main() {\n    printf(\"Hello, World!\\n\");\n    return 0;\n}"
  },
  {
    "id": 54,
    "name": "C++ (GCC 9.2.0)",
    "label": "C++ (GCC 9.2.0)",
    "value": "cpp",
    "defaultCode": "#include <iostream>\n\nint main() {\n    std::cout << \"Hello, World!\" << std::endl;\n    return 0;\n}"
  },
  {
    "id": 86,
    "name": "Clojure (1.10.1)",
    "label": "Clojure (1.10.1)",
    "value": "clojure",
    "defaultCode": "(println \"Hello, World!\")"
  },
  {
    "id": 51,
    "name": "C# (Mono 6.6.0.161)",
    "label": "C# (Mono 6.6.0.161)",
    "value": "csharp",
    "defaultCode": "using System;\n\nclass Program {\n    static void Main() {\n        Console.WriteLine(\"Hello, World!\");\n    }\n}"
  },
  {
    "id": 77,
    "name": "COBOL (GnuCOBOL 2.2)",
    "label": "COBOL (GnuCOBOL 2.2)",
    "value": "cobol",
    "defaultCode": "IDENTIFICATION DIVISION.\nPROGRAM-ID. HelloWorld.\nPROCEDURE DIVISION.\n    DISPLAY 'Hello, World!'.\n    STOP RUN."
  },
  {
    "id": 56,
    "name": "D (DMD 2.089.1)",
    "label": "D (DMD 2.089.1)",
    "value": "d",
    "defaultCode": "import std.stdio;\n\nvoid main() {\n    writeln(\"Hello, World!\");\n}"
  },
  {
    "id": 57,
    "name": "Elixir (1.9.4)",
    "label": "Elixir (1.9.4)",
    "value": "elixir",
    "defaultCode": "IO.puts \"Hello, World!\""
  },
  {
    "id": 58,
    "name": "Erlang (OTP 22.2)",
    "label": "Erlang (OTP 22.2)",
    "value": "erlang",
    "defaultCode": "-module(hello).\n-export([world/0]).\n\nworld() -> io:format(\"Hello, World!~n\")."
  },
  {
    "id": 87,
    "name": "F# (.NET Core SDK 3.1.202)",
    "label": "F# (.NET Core SDK 3.1.202)",
    "value": "fsharp",
    "defaultCode": "printfn \"Hello, World!\""
  },
  {
    "id": 59,
    "name": "Fortran (GFortran 9.2.0)",
    "label": "Fortran (GFortran 9.2.0)",
    "value": "fortran",
    "defaultCode": "program hello\n    print *, \"Hello, World!\"\nend program hello"
  },
  {
    "id": 60,
    "name": "Go (1.13.5)",
    "label": "Go (1.13.5)",
    "value": "go",
    "defaultCode": "package main\n\nimport \"fmt\"\n\nfunc main() {\n    fmt.Println(\"Hello, World!\")\n}"
  },
  {
    "id": 88,
    "name": "Groovy (3.0.3)",
    "label": "Groovy (3.0.3)",
    "value": "groovy",
    "defaultCode": "println 'Hello, World!'"
  },
  {
    "id": 61,
    "name": "Haskell (GHC 8.8.1)",
    "label": "Haskell (GHC 8.8.1)",
    "value": "haskell",
    "defaultCode": "main = putStrLn \"Hello, World!\""
  },
  {
    "id": 62,
    "name": "Java (OpenJDK 13.0.1)",
    "label": "Java (OpenJDK 13.0.1)",
    "value": "java",
    "defaultCode": "public class HelloWorld {\n    public static void main(String[] args) {\n        System.out.println(\"Hello, World!\");\n    }\n}"
  },
  {
    "id": 78,
    "name": "Kotlin (1.3.70)",
    "label": "Kotlin (1.3.70)",
    "value": "kotlin",
    "defaultCode": "fun main() {\n    println(\"Hello, World!\")\n}"
  },
  {
    "id": 64,
    "name": "Lua (5.3.5)",
    "label": "Lua (5.3.5)",
    "value": "lua",
    "defaultCode": "print(\"Hello, World!\")"
  },
  {
    "id": 65,
    "name": "OCaml (4.09.0)",
    "label": "OCaml (4.09.0)",
    "value": "ocaml",
    "defaultCode": "print_endline \"Hello, World!\""
  },
  {
    "id": 66,
    "name": "Octave (5.1.0)",
    "label": "Octave (5.1.0)",
    "value": "octave",
    "defaultCode": "disp('Hello, World!')"
  },
  {
    "id": 67,
    "name": "Pascal (FPC 3.0.4)",
    "label": "Pascal (FPC 3.0.4)",
    "value": "pascal",
    "defaultCode": "program HelloWorld;\nbegin\n  writeln('Hello, World!');\nend."
  },
  {
    "id": 85,
    "name": "Perl (5.28.1)",
    "label": "Perl (5.28.1)",
    "value": "perl",
    "defaultCode": "print \"Hello, World!\\n\";"
  },
  {
    "id": 68,
    "name": "PHP (7.4.1)",
    "label": "PHP (7.4.1)",
    "value": "php",
    "defaultCode": "<?php\necho \"Hello, World!\";\n?>"
  },
  {
    "id": 43,
    "label": "Plain Text",
    "name": "Plain Text",
    "value": "text",
    "defaultCode": "Hello, World!"
  },
  {
    "id": 69,
    "name": "Prolog (GNU Prolog 1.4.5)",
    "label": "Prolog (GNU Prolog 1.4.5)",
    "value": "prolog",
    "defaultCode": ":- initialization(main).\n\nmain :- write('Hello, World!'), nl."
  },
  {
    "id": 71,
    "name": "Python (3.8.1)",
    "label": "Python (3.8.1)",
    "value": "python",
    "defaultCode": "print(\"Hello, World!\")"
  },
  {
    "id": 80,
    "name": "R (4.0.0)",
    "label": "R (4.0.0)",
    "value": "r",
    "defaultCode": "cat(\"Hello, World!\\n\")"
  },
  {
    "id": 72,
    "name": "Ruby (2.7.0)",
    "label": "Ruby (2.7.0)",
    "value": "ruby",
    "defaultCode": "puts \"Hello, World!\""
  },
  {
    "id": 73,
    "name": "Rust (1.40.0)",
    "label": "Rust (1.40.0)",
    "value": "rust",
    "defaultCode": "fn main() {\n    println!(\"Hello, World!\");\n}"
  },
  {
    "id": 81,
    "name": "Scala (2.13.2)",
    "label": "Scala (2.13.2)",
    "value": "scala",
    "defaultCode": "object HelloWorld {\n    def main(args: Array[String]): Unit = {\n        println(\"Hello, World!\")\n    }\n}"
  },
  {
    "id": 82,
    "name": "SQL (SQLite 3.27.2)",
    "label": "SQL (SQLite 3.27.2)",
    "value": "sql",
    "defaultCode": "SELECT 'Hello, World!';"
  },
  {
    "id": 83,
    "name": "Swift (5.2.3)",
    "label": "Swift (5.2.3)",
    "value": "swift",
    "defaultCode": "print(\"Hello, World!\")"
  },
  {
    "id": 74,
    "name": "TypeScript (3.7.4)",
    "label": "TypeScript (3.7.4)",
    "value": "typescript",
    "defaultCode": "console.log(\"Hello, World!\");"
  },
]