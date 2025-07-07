# Brainfuck Interpreter

A fast and robust Brainfuck interpreter written in Haskell with comprehensive diagnostic features and error handling.

## Features

- **Full Brainfuck Language Support**: All 8 standard Brainfuck commands
- **8-bit Cell Wrapping**: Cells wrap from 0-255 automatically
- **Robust Loop Handling**: Supports nested loops and proper bracket matching
- **Diagnostic Mode**: Step-by-step execution tracing (default)
- **Silent Mode**: Clean output without diagnostic information
- **Error Handling**: Graceful handling of file errors and invalid input
- **Comprehensive Testing**: Extensive test suite with HUnit
- **Input Support**: Interactive character input with the `,` command

## Quick Start

### Prerequisites

- GHC (Glasgow Haskell Compiler) 8.6 or later
- Cabal 3.0 or later

### Building

```bash

# Build the project
cabal build

# Run tests
cabal test
```

### Basic Usage

```bash
# Run with diagnostic output (default)
cabal run bf-interpreter -- examples/hello_world.bf

# Run silently (clean output only)
cabal run bf-interpreter -- --silent examples/hello_world.bf

# Show help
cabal run bf-interpreter -- --help

# Show version
cabal run bf-interpreter -- --version
```

## Command Line Options

- `--silent`: Run without diagnostic output
- `--help`, `-h`: Show help message
- `--version`: Show version information

## Examples

The `examples/` directory contains several Brainfuck programs:

### Hello World
```bash
cabal run bf-interpreter -- examples/hello_world.bf
```
Prints "Hello World!" followed by a newline.

### Simple Programs
```bash
# Print 'A' three times
cabal run bf-interpreter -- examples/loop_A_x3.bf

# Demonstrate nested loops and cell clearing
cabal run bf-interpreter -- examples/nested_loop_zero_cell.bf

# Simple newline example
cabal run bf-interpreter -- examples/simple.bf
```

## Language Reference

| Command | Description |
|---------|-------------|
| `>`     | Move data pointer right |
| `<`     | Move data pointer left |
| `+`     | Increment byte at data pointer |
| `-`     | Decrement byte at data pointer |
| `.`     | Output byte at data pointer as ASCII |
| `,`     | Input byte and store at data pointer |
| `[`     | Jump past matching `]` if byte is zero |
| `]`     | Jump back to matching `[` if byte is non-zero |

## Diagnostic Output

In default mode, the interpreter provides detailed execution traces:

```
[instr: +] (src: ++++++++[>++++[>++>+++>+++>+<<<<-) L: [0,0,0,0,0] | C: 0 | R: [0,0,0,0,0]
[instr: +] (src: +++++++[>++++[>++>+++>+++>+<<<<-]) L: [0,0,0,0,0] | C: 1 | R: [0,0,0,0,0]
...
==== Interpreted result ============
Hello World!

[Finished program]
```

## Testing

Run the comprehensive test suite:

```bash
cabal test
```

Tests cover:
- Basic tape operations (move, increment, decrement)
- Cell wrapping behavior
- Loop execution (including nested loops)
- Input/output operations
- Edge cases and error conditions

## Project Structure

```
BF-Haskell-Interpreter/
├── app/
│   ├── Main.hs          # Command-line interface and main entry point
│   ├── Interpreter.hs   # Core Brainfuck interpreter logic
│   └── Tape.hs          # Tape data structure and operations
├── examples/            # Sample Brainfuck programs
│   ├── hello_world.bf
│   ├── loop_A_x3.bf
│   ├── nested_loop_zero_cell.bf
│   └── simple.bf
├── test/
│   └── InterpreterTest.hs # HUnit test suite
├── bf-interpreter.cabal  # Cabal build configuration
├── README.md
└── LICENSE
```

## Implementation Details

### Tape Model
The interpreter uses an infinite tape model implemented as a zipper:
- `([Int], Int, [Int])` representing (left cells, current cell, right cells)
- Cells are 8-bit values (0-255) with automatic wrapping
- Infinite tape extends in both directions

### Loop Handling
Bracket matching uses a robust algorithm that:
- Correctly handles nested loops
- Provides proper error handling for unmatched brackets
- Efficiently extracts loop bodies without full parsing

### Error Handling
- File reading errors are caught and reported
- Input errors default to value 0
- Invalid characters are filtered out automatically
- Graceful handling of edge cases

## Performance

The interpreter is designed for clarity and correctness rather than maximum performance, but includes several optimizations:
- Efficient tape representation using Haskell's lazy evaluation
- Direct character filtering of input files
- Minimal memory allocation during execution

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass: `cabal test`
6. Submit a pull request

## License

This project is licensed under the BSD 3-Clause License - see the LICENSE file for details.

## Acknowledgments

- The Brainfuck language was created by Urban Müller
- Inspired by the elegance of functional programming and Haskell's type system
