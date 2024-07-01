const {
    CPP, C, PYTHON, JAVA, NODEJS, RUBY, PROMPTV1, PROMPTV2, SWIFT, HASKELL, RUST, SCALA, TYPESCRIPT,
    KOTLIN, GO, LUA, PERL, PHP, R, OCAML, OCTAVE, PASCAL, PROLOG, GROOVY, FORTRAN, ELIXIR, ERLANG, D, COBOL
} = require('../enums/supportedLanguages');

const ONE_MB = 1024; // ulimit uses Kilobyte as base units
const ALLOWED_RAM = process.env.ALLOWED_RAM || 512;

const LANGUAGES_CONFIG = {
    [C]: {
        compile: 'gcc -o a.out solution.c',
        run: './a.out',
        timeout: 2,
        filename: 'solution.c',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [CPP]: {
        compile: 'g++ -o a.out -pthread -O0 solution.cpp',
        run: './a.out',
        timeout: 2,
        filename: 'solution.cpp',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [PYTHON]: {
        compile: 'python -m compileall -q solution.py',
        run: 'python solution.py',
        timeout: 10,
        filename: 'solution.py',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [JAVA]: {
        compile: 'javac Solution.java',
        run: 'java Solution',
        timeout: 4,
        filename: 'Solution.java',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [NODEJS]: {
        compile: 'node --check solution.js',
        run: 'node solution.js',
        timeout: 10,
        filename: 'solution.js',
        memory: 786432, // Node.js v20 requires more initial memory, so initialize it to around 780MB (1.5 * 512MB). This value is higher than the previous 512MB but below 1GB to ensure ulimit catches excessive memory use without the GCR container being killed.
    },
    [RUBY]: {
        compile: 'ruby -c solution.rb',
        run: 'ruby solution.rb',
        timeout: 10,
        filename: 'solution.rb',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [SWIFT]: {
        compile: 'swiftc -o a.out solution.swift',
        run: './a.out',
        timeout: 4,
        filename: 'solution.swift',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [HASKELL]: {
        compile: 'ghc -o a.out solution.hs',
        run: './a.out',
        timeout: 4,
        filename: 'solution.hs',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [RUST]: {
        compile: 'rustc -o a.out solution.rs',
        run: './a.out',
        timeout: 4,
        filename: 'solution.rs',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [SCALA]: {
        compile: 'scalac solution.scala',
        run: 'scala Solution',
        timeout: 4,
        filename: 'Solution.scala',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [TYPESCRIPT]: {
        compile: 'tsc solution.ts',
        run: 'node solution.js',
        timeout: 10,
        filename: 'solution.ts',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [KOTLIN]: {
        compile: 'kotlinc solution.kt -include-runtime -d solution.jar',
        run: 'java -jar solution.jar',
        timeout: 4,
        filename: 'solution.kt',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [GO]: {
        compile: 'go build -o a.out solution.go',
        run: './a.out',
        timeout: 4,
        filename: 'solution.go',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [LUA]: {
        compile: null,
        run: 'lua solution.lua',
        timeout: 4,
        filename: 'solution.lua',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [PERL]: {
        compile: 'perl -c solution.pl',
        run: 'perl solution.pl',
        timeout: 4,
        filename: 'solution.pl',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [PHP]: {
        compile: 'php -l solution.php',
        run: 'php solution.php',
        timeout: 4,
        filename: 'solution.php',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [R]: {
        compile: null,
        run: 'Rscript solution.R',
        timeout: 4,
        filename: 'solution.R',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [OCAML]: {
        compile: 'ocamlc -o a.out solution.ml',
        run: './a.out',
        timeout: 4,
        filename: 'solution.ml',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [OCTAVE]: {
        compile: null,
        run: 'octave --silent solution.m',
        timeout: 4,
        filename: 'solution.m',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [PASCAL]: {
        compile: 'fpc solution.pas',
        run: './solution',
        timeout: 4,
        filename: 'solution.pas',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [PROLOG]: {
        compile: null,
        run: 'swipl -q -f solution.pl -t main',
        timeout: 4,
        filename: 'solution.pl',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [GROOVY]: {
        compile: 'groovyc solution.groovy',
        run: 'groovy solution',
        timeout: 4,
        filename: 'solution.groovy',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [FORTRAN]: {
        compile: 'gfortran -o a.out solution.f90',
        run: './a.out',
        timeout: 4,
        filename: 'solution.f90',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [ELIXIR]: {
        compile: 'elixirc solution.ex',
        run: 'elixir solution.exs',
        timeout: 4,
        filename: 'solution.ex',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [ERLANG]: {
        compile: 'erlc solution.erl',
        run: 'erl -noshell -s solution start -s init stop',
        timeout: 4,
        filename: 'solution.erl',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [D]: {
        compile: 'dmd -ofa.out solution.d',
        run: './a.out',
        timeout: 4,
        filename: 'solution.d',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [COBOL]: {
        compile: 'cobc -x -o a.out solution.cob',
        run: './a.out',
        timeout: 4,
        filename: 'solution.cob',
        memory: ALLOWED_RAM * ONE_MB,
    },
    [PROMPTV1]: {
        model: 'gpt-4-1106-preview',
    },
    [PROMPTV2]: {
        model: 'gpt-3.5-turbo-1106',
    },
};

module.exports = { LANGUAGES_CONFIG };
