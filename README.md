# Slop

Slop is a small programming-language prototype designed around one question: can a model generate a useful algorithm in a compact, statically checked language?

The compiler is dependency-free Rust. It lexes and parses `.slop` source, checks explicit types, lowers functions and control flow to stack bytecode, then runs that bytecode on a VM. The VM has integer and boolean values, mutable integer arrays, recursive functions, conditionals, loops, and structured JSON diagnostics. Arrays are shared by reference across function calls, which supports in-place backtracking.

## Run the example

```sh
cargo run --release -- examples/sudoku.slop
```

The Sudoku solver returns its 81 cells as a JSON array. A zero in the input means an empty cell. Invalid source prints a JSON error object to stderr with a stable code and a location field. Lexer errors use byte offsets; parser and type errors currently use coarse token locations.

## Declarative experiment

There is also a small finite-domain constraint language. Instead of implementing the solver, source declares an integer domain, fixed clues, and `all_different` constraints:

```text
var cell[81] in 1..10
given cell[0] = 5
all_different cell[0..9]
```

Domain and range ends are exclusive. Run its Sudoku model with:

```sh
cargo run --release --bin slop-csp -- examples/sudoku.csp
```

The backend uses constraint propagation and minimum-remaining-values backtracking. This experiment makes Sudoku's rules short to state and removes the need for generated search code. It is deliberately a narrow CSP language: it cannot express arbitrary algorithms, and its built-in solver does much of the work. So it is a useful test of declarative problem modeling, not evidence that this syntax improves general code generation.

## Language sketch

```text
fn sum_to(n: Int) -> Int {
    let total: Int = 0;
    let i: Int = 1;
    while i <= n {
        total = total + i;
        i = i + 1;
    }
    return total;
}

fn main() -> Int {
    return sum_to(100);
}
```

Types are `Int`, `Bool`, `Array`, and `Unit`. Local variables require explicit types. Arrays contain integers and are created with `[1, 2, 3]` or `fill(length, value)`. `len(array)` returns the array length. Functions require parameter and return types. `main` is the entry point and its result is printed as JSON.

## Prototype limits

The goal is to exercise code generation and algorithmic state, not to provide a production language. There is no I/O, strings, modules, generics, garbage collector, optimizer, or debug info yet. Bytecode is compiled in memory and executed directly; there is no bytecode file format. Runtime and type errors use stable codes. Array values alias on assignment and function calls. Arithmetic is signed 64-bit integer arithmetic. The separate CSP experiment currently supports integer arrays, givens, and all-different constraints only.
