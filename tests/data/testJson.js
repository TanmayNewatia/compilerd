const testCases = [
    {
        name: 'cpp : hello world',
        reqObject: {
            language: 'cpp',
            script:
                '#include<bits/stdc++.h>\n' +
                'using namespace std;\n' +
                'int main(){\n' +
                '    cout << "hello world";\n' +
                'return 0;\n' +
                '}\n',
        },
        expectedResponse: {
            val: 'hello world',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'cpp : print stdin',
        reqObject: {
            language: 'cpp',
            script:
                '#include<bits/stdc++.h>\n\n' +
                'using namespace std;\n' +
                'int main(){\n\n' +
                '    int a;\n' +
                '    while(cin >> a){\n' +
                '        cout << a << endl;\n' +
                '    }\n' +
                '    return 0;\n\n' +
                '}\n',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1\n2\n3\n',
            status: 200,
            error: 0,
        },

    },
    {
        name: 'nodejs : hello world',
        reqObject: {
            language: 'nodejs',
            script: 'console.log(\'hello world\')',
        },
        expectedResponse: {
            val: 'hello world\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'nodejs : print stdin',
        reqObject: {
            language: 'nodejs',
            script:
                'process.stdin.setEncoding(\'utf8\'); \n ' +
                'process.stdin.on(\'data\', (input) => { \n ' +
                '  console.log(input); \n ' +
                ' \n ' +
                '}); \n ',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'python : hello world',
        reqObject: {
            language: 'python',
            script: 'print(\'hello world\')',
        },
        expectedResponse: {
            val: 'hello world\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'python : print stdin',
        reqObject: {
            language: 'python',
            script:
                'try:\n' +
                '    while(True):\n' +
                '        line = input()\n' +
                '        if not line:\n' +
                '            break\n' +
                '        print(line)\n' +
                'except EOFError:\n' +
                '    pass',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'c : hello world',
        reqObject: {
            language: 'c',
            script:
                '#include<stdio.h>\n\n' +
                'int main(){\n\n' +
                '    printf("hello world");\n' +
                '    return 0;\n' +
                '}\n',
        },
        expectedResponse: {
            val: 'hello world',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'c : print stdin',
        reqObject: {
            language: 'c',
            script:
                '#include <stdio.h>\n' +
                'int main() {\n' +
                '    int number;\n' +
                '    while (scanf("%d", &number) == 1) {\n' +
                '        printf("%d\\n", number);\n' +
                '    } \n' +
                '    return 0;\n' +
                '}',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1\n2\n3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'java : print stdin',
        reqObject: {
            language: 'java',
            script:
                'import java.util.Scanner;\n' +
                'public class Solution {\n' +
                '    public static void main(String[] args) {\n' +
                '        System.out.println("hello world");\n' +
                '    }\n' +
                '}\n',
        },
        expectedResponse: {
            val: 'hello world\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'java : print stdin',
        reqObject: {
            language: 'java',
            script:
                'import java.util.Scanner;\n' +
                'public class Solution {\n' +
                '    public static void main(String[] args) {\n' +
                '        Scanner scanner = new Scanner(System.in);\n' +
                '        while (scanner.hasNextInt()) {\n' +
                '            int number = scanner.nextInt();\n' +
                '            System.out.println(number);\n' +
                '        } \n' +
                '        scanner.close();\n' +
                '    }\n' +
                '}\n',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1\n2\n3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'ruby : print hello world',
        reqObject: {
            language: 'ruby',
            script:
                'print "hello world"'
        },
        expectedResponse: {
            val: 'hello world',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'ruby : print stdin',
        reqObject: {
            language: 'ruby',
            script:
                'user_input = gets.chomp\n' +
                'puts user_input',
            stdin: '10\n'
        },
        expectedResponse: {
            val: '10\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'TLE test',
        reqObject: {
            language: 'nodejs',
            script: 'for(let i=0 ; ; ){i++}',
        },
        expectedResponse: {
            val: 'Time limit exceeded',
            status: 200,
            error: 1,
        },
    },
    {
        name: 'MLE test',
        reqObject: {
            language: 'python',
            script: 'one_gb_data = bytearray(1000 * 1024 * 1024)',
        },
        expectedResponse: {
            val: 'Memory limit exceeded',
            status: 200,
            error: 1,
        },
    },
    {
        name: 'MLE test 2',
        reqObject: {
            language: 'python',
            script:
                'import time\n' +
                'def consume_memory(target_mb, duration_sec):\n' +
                '    float_size = 8\n' +
                '    floats_per_mb = (1024 * 1024) // float_size\n' +
                '    total_floats = target_mb * floats_per_mb\n' +
                '    iterations = int(duration_sec / 0.1)\n' +
                '    floats_per_iteration = total_floats // iterations\n' +
                '    memory_hog = []\n' +
                '    for _ in range(iterations):\n' +
                '        memory_hog.extend([0.0] * floats_per_iteration)\n' +
                '        time.sleep(0.1)\n' +
                'consume_memory(1000, 1)\n',
        },
        expectedResponse: {
            val: 'Memory limit exceeded',
            status: 200,
            error: 1,
        },
    },
    {
        name: 'MLE test 3',
        reqObject: {
            language: 'python',
            script:
                'a = [100]\n' +
                'for i in a:\n' +
                '    a.append(i)\n',
        },
        expectedResponse: {
            val: 'Memory limit exceeded',
            status: 200,
            error: 1,
        },
    },
    {
        name: 'OPEN AI test promptv1',
        reqObject: {
            language: 'promptv1',
            prompt: 'The question is what is 2 plus 2. The answer given is 4.',
        },
        expectedResponse: {
            val: {},
            status: 200,
            error: 0,
        },
    },
    {
        name: 'OPEN AI test promptv2',
        reqObject: {
            language: 'promptv2',
            prompt: 'The question is what is 2 plus 2. The answer given is 4.',
        },
        expectedResponse: {
            val: {},
            status: 200,
            error: 0,
        },
    },
    {
        name: 'typescript : hello world',
        reqObject: {
            language: 'typescript',
            script: 'console.log("hello world");',
        },
        expectedResponse: {
            val: 'hello world\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'typescript : print stdin',
        reqObject: {
            language: 'typescript',
            script:
                'process.stdin.setEncoding("utf8"); \n' +
                'process.stdin.on("data", (input) => { \n' +
                '  console.log(input.trim().split(" ").join("\\n")); \n' +
                '}); \n',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1\n2\n3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'go : hello world',
        reqObject: {
            language: 'go',
            script: 'package main\n\nimport "fmt"\n\nfunc main() {\n    fmt.Println("hello world")\n}',
        },
        expectedResponse: {
            val: 'hello world\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'go : print stdin',
        reqObject: {
            language: 'go',
            script:
                'package main\n\nimport (\n    "bufio"\n    "fmt"\n    "os"\n)\n\nfunc main() {\n    scanner := bufio.NewScanner(os.Stdin)\n    for scanner.Scan() {\n        fmt.Println(scanner.Text())\n    }\n}',
            stdin: '1\n2\n3\n',
        },
        expectedResponse: {
            val: '1\n2\n3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'swift : hello world',
        reqObject: {
            language: 'swift',
            script: 'print("hello world")',
        },
        expectedResponse: {
            val: 'hello world\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'swift : print stdin',
        reqObject: {
            language: 'swift',
            script:
                'import Foundation\n' +
                'let input = readLine()\n' +
                'if let input = input {\n' +
                '    print(input)\n' +
                '}',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'php : hello world',
        reqObject: {
            language: 'php',
            script: '<?php echo "hello world"; ?>',
        },
        expectedResponse: {
            val: 'hello world',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'php : print stdin',
        reqObject: {
            language: 'php',
            script:
                '<?php\n' +
                '$input = file_get_contents("php://stdin");\n' +
                '$numbers = explode(" ", trim($input));\n' +
                'foreach ($numbers as $number) {\n' +
                '    echo $number . "\\n";\n' +
                '}',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1\n2\n3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Assembly : hello world',
        reqObject: {
            language: 'assembly',
            script:
                'section .data\n' +
                '    msg db "Hello, World!", 0\n\n' +
                'section .text\n' +
                '    global _start\n\n' +
                '_start:\n' +
                '    mov eax, 4\n' +
                '    mov ebx, 1\n' +
                '    mov ecx, msg\n' +
                '    mov edx, 13\n' +
                '    int 0x80\n\n' +
                '    mov eax, 1\n' +
                '    xor ebx, ebx\n' +
                '    int 0x80\n',
        },
        expectedResponse: {
            val: 'Hello, World!',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Bash : hello world',
        reqObject: {
            language: 'bash',
            script: 'echo "Hello, World!"',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Bash : print stdin',
        reqObject: {
            language: 'bash',
            script: 'read input\n echo "$input"',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Basic : hello world',
        reqObject: {
            language: 'basic',
            script: 'PRINT "Hello, World!"',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Basic : print stdin',
        reqObject: {
            language: 'basic',
            script: 'INPUT "", input$\nPRINT input$',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Clojure : hello world',
        reqObject: {
            language: 'clojure',
            script: '(println "Hello, World!")',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Clojure : print stdin',
        reqObject: {
            language: 'clojure',
            script: '(println (read-line))',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'C# : hello world',
        reqObject: {
            language: 'csharp',
            script: 'using System;\n\nclass Program {\n    static void Main() {\n        Console.WriteLine("Hello, World!");\n    }\n}',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'C# : print stdin',
        reqObject: {
            language: 'csharp',
            script: 'using System;\n\nclass Program {\n    static void Main() {\n        string input = Console.ReadLine();\n        Console.WriteLine(input);\n    }\n}',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'COBOL : hello world',
        reqObject: {
            language: 'cobol',
            script: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. HelloWorld.\nPROCEDURE DIVISION.\n    DISPLAY \'Hello, World!\'.\n    STOP RUN.',
        },
        expectedResponse: {
            val: 'Hello, World!',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'COBOL : print stdin',
        reqObject: {
            language: 'cobol',
            script: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. PrintStdin.\nDATA DIVISION.\nWORKING-STORAGE SECTION.\n01 userInput PIC X(100).\nPROCEDURE DIVISION.\n    ACCEPT userInput\n    DISPLAY userInput\n    STOP RUN.',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'D : hello world',
        reqObject: {
            language: 'd',
            script: 'import std.stdio;\n\nvoid main() {\n    writeln("Hello, World!");\n}',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'D : print stdin',
        reqObject: {
            language: 'd',
            script: 'import std.stdio;\n\nvoid main() {\n    string input = stdin.readLine();\n    writeln(input);\n}',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Erlang : hello world',
        reqObject: {
            language: 'erlang',
            script: 'io:format("Hello, World!~n").',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Erlang : print stdin',
        reqObject: {
            language: 'erlang',
            script: 'main() ->\n    {ok, Input} = io:get_line(\'\'),\n    io:format(Input).\n',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Elixir : hello world',
        reqObject: {
            language: 'elixir',
            script: 'IO.puts "Hello, World!"',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Elixir : print stdin',
        reqObject: {
            language: 'elixir',
            script: 'input = IO.gets("")\nIO.puts input',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Fortran : hello world',
        reqObject: {
            language: 'fortran',
            script: 'program hello\n  print *, "Hello, World!"\nend program hello',
        },
        expectedResponse: {
            val: ' Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Fortran : print stdin',
        reqObject: {
            language: 'fortran',
            script: 'program print_stdin\n  character(len=100) :: input\n  read(*,*) input\n  print *, trim(input)\nend program print_stdin',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: ' 1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Groovy : hello world',
        reqObject: {
            language: 'groovy',
            script: 'println "Hello, World!"',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Groovy : print stdin',
        reqObject: {
            language: 'groovy',
            script: 'def input = System.console().readLine()\nprintln input',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Haskell : hello world',
        reqObject: {
            language: 'haskell',
            script: 'main = putStrLn "Hello, World!"',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Haskell : print stdin',
        reqObject: {
            language: 'haskell',
            script: 'main = getLine >>= putStrLn',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Kotlin : hello world',
        reqObject: {
            language: 'kotlin',
            script: 'fun main() {\n    println("Hello, World!")\n}',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Kotlin : print stdin',
        reqObject: {
            language: 'kotlin',
            script: 'fun main() {\n    val input = readLine()\n    println(input)\n}',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Lua : hello world',
        reqObject: {
            language: 'lua',
            script: 'print("Hello, World!")',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Lua : print stdin',
        reqObject: {
            language: 'lua',
            script: 'input = io.read()\nprint(input)',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'OCaml : hello world',
        reqObject: {
            language: 'ocaml',
            script: 'print_endline "Hello, World!";;',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'OCaml : print stdin',
        reqObject: {
            language: 'ocaml',
            script: 'let () =\n  let input = read_line () in\n  print_endline input',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Octave : hello world',
        reqObject: {
            language: 'octave',
            script: 'disp("Hello, World!")',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Octave : print stdin',
        reqObject: {
            language: 'octave',
            script: 'input = input("", "s");\ndisp(input)',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Pascal : hello world',
        reqObject: {
            language: 'pascal',
            script: 'program HelloWorld;\nbegin\n  writeln(\'Hello, World!\');\nend.',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Pascal : print stdin',
        reqObject: {
            language: 'pascal',
            script: 'program PrintStdin;\nvar\n  input: string;\nbegin\n  readln(input);\n  writeln(input);\nend.',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Perl : hello world',
        reqObject: {
            language: 'perl',
            script: 'print "Hello, World!\\n";',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Perl : print stdin',
        reqObject: {
            language: 'perl',
            script: 'my $input = <STDIN>;\nprint $input;',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Prolog : hello world',
        reqObject: {
            language: 'prolog',
            script: ':- initialization(main).\nmain :- write(\'Hello, World!\'), nl.',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Prolog : print stdin',
        reqObject: {
            language: 'prolog',
            script: ':- initialization(main).\nmain :- read_line_to_string(user_input, Input), write(Input), nl.',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'R : hello world',
        reqObject: {
            language: 'r',
            script: 'cat("Hello, World!\\n")',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'R : print stdin',
        reqObject: {
            language: 'r',
            script: 'input <- readLines("stdin", n=1)\ncat(input, "\\n")',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Rust : hello world',
        reqObject: {
            language: 'rust',
            script: 'fn main() {\n    println!("Hello, World!");\n}',
        },
        expectedResponse: {
            val: 'Hello, World!\n',
            status: 200,
            error: 0,
        },
    },
    {
        name: 'Rust : print stdin',
        reqObject: {
            language: 'rust',
            script: 'use std::io::{self, BufRead};\n\nfn main() {\n    let stdin = io::stdin();\n    let input = stdin.lock().lines().next().unwrap().unwrap();\n    println!("{}", input);\n}',
            stdin: '1 2 3',
        },
        expectedResponse: {
            val: '1 2 3\n',
            status: 200,
            error: 0,
        },
    },
]

module.exports = { testCases }
