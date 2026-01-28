# Programming Language Benchmark Suite

A comprehensive, cross-language performance benchmarking framework comparing execution speed across **16+ programming languages** with multiple compilers, assemblers, and computational workloads.

[![GitHub](https://img.shields.io/badge/GitHub-killerdevildog%2FProgramming__Benchmark-blue?logo=github)](https://github.com/killerdevildog/Programming_Benchmark)
[![Codeberg](https://img.shields.io/badge/Codeberg-killerdevildog%2FProgramming__Benchmark-blue?logo=codeberg)](https://codeberg.org/killerdevildog/Programming_Benchmark)
[![Radicle](https://img.shields.io/badge/Radicle-z4RRJ6vMp6Ysa1WSRW7opPzQSDTfX-purple?logo=data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMjQiIGhlaWdodD0iMjQiIHZpZXdCb3g9IjAgMCAyNCAyNCIgZmlsbD0ibm9uZSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj4KPHBhdGggZD0iTTEyIDJDNi40OCAyIDIgNi40OCAyIDEyczQuNDggMTAgMTAgMTAgMTAtNC40OCAxMC0xMFMxNy41MiAyIDEyIDJ6IiBmaWxsPSJ3aGl0ZSIvPgo8L3N2Zz4=)](https://app.radicle.xyz/nodes/seed.radicle.xyz/rad:z4RRJ6vMp6Ysa1WSRW7opPzQSDTfX)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## 🎯 Project Overview

This project provides **apples-to-apples performance comparisons** of compiled executables across diverse programming languages, compilers, and optimization strategies. Unlike interpreter-based benchmarks, all implementations are compiled to native executables, providing true performance insights into language efficiency and compiler optimization capabilities.

### Why This Matters

- **Real-world Performance**: Measures actual executable performance, not interpreter overhead
- **Compiler Comparison**: Tests the same language with different compilers (GCC vs Clang)
- **Assembler Variants**: Compares NASM, YASM, and GAS (GNU Assembler) for assembly code
- **Fair Evaluation**: Identical algorithms across all languages ensure fair comparisons
- **Reproducible Results**: Automated benchmarking with hyperfine ensures statistical validity

## ✨ Key Features

- **48 Hello World Implementations**: Minimal overhead testing across languages
- **32 Computational Benchmarks**: Prime number calculation (first 1000 primes)
- **Multi-Compiler Support**: GCC and Clang for compatible languages
- **Assembly Variants**: NASM (Intel syntax), YASM, and GAS (AT&T syntax)
- **Embedded Language Support**: R via embedded API, Python via Cython
- **Automated Benchmark Tool**: Custom wrapper around [hyperfine](https://github.com/sharkdp/hyperfine)
- **Multiple Output Formats**: Markdown tables and JSON for further analysis

## 📊 Supported Languages

### System Programming Languages
- **C** - Both GCC and Clang
- **C++** - 6 different implementations (iostreams, printf variants)
- **Rust** - Using rustc
- **Zig** - Latest stable
- **Go** - Using gccgo and standard go compiler
- **D** - Using gdc/ldc
- **Nim** - Compiled via C backend

### Assembly Languages
- **NASM** (Intel syntax)
- **YASM** (NASM-compatible)
- **GAS** (GNU Assembler with AT&T syntax)

### High-Level/Managed Languages
- **Ada** - Using GNAT
- **Haskell** - GHC compiler
- **C#** - Mono runtime
- **Pascal** - Free Pascal Compiler

### Legacy Languages
- **Fortran 90**
- **COBOL** - GnuCOBOL

### Scripting Languages (Compiled)
- **Python** - Via Cython to native executable
- **R** - Embedded R API with C wrapper

## 🏗️ Project Structure

```
Programming_Benchmark/
├── test_src/              # Source code for all languages
│   ├── c/
│   │   ├── hello_world/001/hello.c
│   │   └── first1000primes/001/primes.c
│   ├── asm/
│   │   ├── hello_world/
│   │   │   ├── 001/hello.asm       (NASM)
│   │   │   ├── 001/hello_yasm.asm  (YASM)
│   │   │   └── 001/hello_gas.s     (GAS)
│   ├── r/
│   │   ├── hello_world/001/
│   │   │   ├── hello.R
│   │   │   └── hello_wrapper.c
│   └── [other languages]/
├── bin/
│   ├── tests/             # Compiled executables
│   │   ├── gcc/
│   │   │   ├── c/hello_world/001/hello
│   │   │   ├── asm_nasm/hello_world/001/hello
│   │   │   └── [...]
│   │   └── clang/
│   ├── benchmark          # Benchmark automation tool
│   └── hyperfine          # Hyperfine binary
├── CMakeLists.txt         # Build configuration
├── results_hello_world.md # Benchmark results (Markdown)
├── results_first1000primes.md
└── README.md
```

## 🚀 Getting Started

### Prerequisites

Install the required compilers and tools for the languages you want to benchmark:

```bash
# Essential build tools
sudo apt install cmake build-essential

# Language compilers
sudo apt install gcc clang gfortran gnat gccgo gdc mono-mcs ghc fpc cobol

# Assembly tools
sudo apt install nasm yasm

# Modern languages (install via respective package managers)
# Rust: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# Zig: https://ziglang.org/download/
# Nim: https://nim-lang.org/install.html

# Python (Cython)
pip install cython

# R development files
sudo apt install r-base r-base-dev

# Optional: Hyperfine (for benchmarking)
# Will be built automatically if not present
cargo install hyperfine
```

### Building the Project

```bash
# Clone the repository
git clone https://github.com/killerdevildog/Programming_Benchmark.git
cd Programming_Benchmark

# Create build directory
mkdir -p build && cd build

# Configure with CMake
cmake ..

# Build all implementations
cmake --build .

# Or build with multiple cores
cmake --build . -j$(nproc)
```

The build process will:
1. Detect available compilers
2. Build all language implementations
3. Create executables in `bin/tests/`
4. Build the benchmark tool

## 🔬 Running Benchmarks

### Quick Start

```bash
# Run hello_world benchmark (500 runs)
./bin/benchmark hello_world --runs 500

# Run first1000primes benchmark (100 runs)
./bin/benchmark first1000primes --runs 100
```

### Benchmark Series

Two benchmark series are available:

1. **hello_world**: Minimal I/O overhead testing
   - Tests: Basic executable startup + output
   - Implementations: 48 variants
   - Typical runtime: 73µs (ASM) to 178ms (R)

2. **first1000primes**: Computational workload
   - Tests: Prime number calculation (find 1000th prime = 7919)
   - Algorithm: Trial division with optimizations
   - Implementations: 32 variants
   - Typical runtime: 216µs (ASM) to 213ms (R)

### Understanding Results

Benchmark results are saved in two formats:

- **Markdown** (`results_*.md`): Human-readable tables
- **JSON** (`results_*.json`): Machine-parseable data

Example output:
```
Summary
  gcc_asm_001 ran
    1.03 ± 0.41 times faster than gcc_zig_001
    2.00 ± 0.59 times faster than gcc_c_001
    988.00 ± 273.66 times faster than gcc_r_001
```

## 📈 Performance Insights

### Hello World Results (500 runs)

| Category | Fastest | Slowest | Range |
|----------|---------|---------|-------|
| **Assembly** | 73µs (GCC/Zig) | 146µs (Clang ASM) | ~2x |
| **System Languages** | 317µs (C) | 898µs (Ada) | ~3x |
| **Managed** | 1.5ms (D) | 12ms (C#) | ~8x |
| **Legacy** | 1.7ms (COBOL) | 11.3ms (Haskell) | ~7x |
| **Embedded** | 16ms (Python/Cython) | 178ms (R) | ~11x |

**Key Takeaway**: R's embedded initialization overhead is ~2400x slower than raw assembly, but provides full statistical computing capabilities.

### First 1000 Primes Results (100 runs)

| Category | Fastest | Slowest | Notes |
|----------|---------|---------|-------|
| **Low-level** | 216µs (ASM) | 243µs (Zig) | Highly optimized |
| **System** | 432µs (C) | 974µs (Ada) | GCC optimizations effective |
| **Functional** | 11.2ms (Haskell) | N/A | Lazy evaluation overhead |
| **Legacy** | 23.8ms (COBOL) | N/A | Surprising competitiveness |
| **Embedded** | 17.5ms (Python) | 213ms (R) | Cython provides good performance |

**Key Insight**: Algorithm matters more than language for compute-heavy tasks. Well-optimized C/Zig/ASM are within 2x of each other.

## 🔧 Technical Highlights

### Assembly Variants

Three assemblers tested with identical functionality:
- **NASM**: Intel syntax (`mov rax, 1`)
- **YASM**: NASM-compatible, different optimizer
- **GAS**: AT&T syntax (`mov $1, %rax`), uses `#` for comments

Both GCC and Clang linkers tested with each assembler.

### R Language Integration

R uses embedded API approach:
```c
#include <R.h>
#include <Rinternals.h>
#include <Rembedded.h>

Rf_initEmbeddedR(4, r_argv);  // Initialize R runtime
R_ParseVector(code_str, -1, &status, R_NilValue);  // Parse R code
Rf_endEmbeddedR(0);  // Cleanup
```

This demonstrates the overhead of embedding interpreted languages in compiled executables.

### Python via Cython

Python code compiled to C and then to native executable:
```bash
cython --embed -o hello.c hello.py
gcc hello.c $(python3-config --cflags --ldflags) -o hello
```

Significantly faster than interpreted Python, but slower than pure C.

## 🧪 Adding New Benchmarks

### Adding a New Language

1. **Create source directory**:
   ```bash
   mkdir -p test_src/newlang/hello_world/001
   mkdir -p test_src/newlang/first1000primes/001
   ```

2. **Implement the benchmarks**:
   - `hello_world`: Print "Hello, World!\n" to stdout
   - `first1000primes`: Calculate and print the 1000th prime (7919)

3. **Update CMakeLists.txt**:
   ```cmake
   find_program(NEWLANG_COMPILER newlangc)
   if(NEWLANG_COMPILER)
       # Add build rules
   endif()
   ```

4. **Rebuild**: `cmake --build build`

### Adding a New Benchmark Series

1. Create source files in `test_src/[lang]/newseries/001/`
2. Update CMakeLists.txt with build rules
3. Executables will be auto-discovered by `./bin/benchmark newseries`

## 🤝 Contributing

Contributions welcome! Areas of interest:

- **New Languages**: Julia, OCaml, Erlang, Elixir, Kotlin Native, Swift
- **New Benchmarks**: Sorting algorithms, matrix operations, string processing
- **Optimizations**: Language-specific optimizations while maintaining algorithm parity
- **Documentation**: Detailed analysis of compiler optimizations

## 📜 License

MIT License - see LICENSE file for details.

## 🙏 Acknowledgments

- [Hyperfine](https://github.com/sharkdp/hyperfine) - Excellent benchmarking tool
- The compiler teams behind GCC, Clang, and all language-specific compilers
- Open source language communities

## 📚 References

- [Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/)
- [Programming Language Comparison](https://programming-language-benchmarks.vercel.app/)
- [Compiler Explorer](https://godbolt.org/) - Analyze compiler output

---

**Note**: Benchmark results are system-dependent. Your mileage may vary based on CPU architecture, OS, compiler versions, and system load. All results in this README are from a Linux system with 16 cores.
