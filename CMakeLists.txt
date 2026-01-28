cmake_minimum_required(VERSION 3.15)
project(ProgrammingBenchmark)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Set output directory for binaries
set(BIN_DIR ${CMAKE_SOURCE_DIR}/bin)
file(MAKE_DIRECTORY ${BIN_DIR})

# Hyperfine build target
set(HYPERFINE_SOURCE_DIR ${CMAKE_SOURCE_DIR}/thirdparty/hyperfine)
set(HYPERFINE_BINARY ${HYPERFINE_SOURCE_DIR}/target/release/hyperfine)
set(HYPERFINE_OUTPUT ${BIN_DIR}/hyperfine)

# Check for cargo
find_program(CARGO_EXECUTABLE cargo)
if(NOT CARGO_EXECUTABLE)
    message(FATAL_ERROR "Cargo not found. Install Rust from https://rustup.rs/")
endif()

# Custom command to build hyperfine
add_custom_command(
    OUTPUT ${HYPERFINE_BINARY}
    COMMAND ${CARGO_EXECUTABLE} build --release
    WORKING_DIRECTORY ${HYPERFINE_SOURCE_DIR}
    COMMENT "Building hyperfine with cargo..."
    VERBATIM
)

# Custom command to copy hyperfine to bin directory
add_custom_command(
    OUTPUT ${HYPERFINE_OUTPUT}
    COMMAND ${CMAKE_COMMAND} -E copy ${HYPERFINE_BINARY} ${HYPERFINE_OUTPUT}
    DEPENDS ${HYPERFINE_BINARY}
    COMMENT "Copying hyperfine to ${BIN_DIR}..."
    VERBATIM
)

# Custom target that depends on the copied binary
add_custom_target(hyperfine ALL
    DEPENDS ${HYPERFINE_OUTPUT}
)

# Print build information
message(STATUS "Hyperfine source directory: ${HYPERFINE_SOURCE_DIR}")
message(STATUS "Binary output directory: ${BIN_DIR}")

# ======================================================================
# Build test programs with multiple compilers
# ======================================================================

set(TEST_SRC_DIR ${CMAKE_SOURCE_DIR}/test_src)
set(TEST_BIN_DIR ${CMAKE_SOURCE_DIR}/bin/tests)

# Detect available compilers
find_program(GCC_EXECUTABLE g++)
find_program(CLANG_EXECUTABLE clang++)
find_program(GCC_C_EXECUTABLE gcc)
find_program(CLANG_C_EXECUTABLE clang)
find_program(NASM_EXECUTABLE nasm)
find_program(YASM_EXECUTABLE yasm)
find_program(GAS_EXECUTABLE as)
find_program(LD_EXECUTABLE ld)
find_program(RUSTC_EXECUTABLE rustc)
find_program(ZIG_EXECUTABLE zig)
find_program(GO_EXECUTABLE go)
find_program(DMD_EXECUTABLE dmd)
find_program(LDC2_EXECUTABLE ldc2)
find_program(GDC_EXECUTABLE gdc)
find_program(NIM_EXECUTABLE nim)
find_program(ODIN_EXECUTABLE odin)
find_program(V_EXECUTABLE v)
find_program(GNAT_EXECUTABLE gnatmake)
find_program(GHC_EXECUTABLE ghc)
find_program(GFORTRAN_EXECUTABLE gfortran)
find_program(FPC_EXECUTABLE fpc)
find_program(COBC_EXECUTABLE cobc)
find_program(CSC_EXECUTABLE csc)
find_program(MCS_EXECUTABLE mcs)
find_program(PYTHON_EXECUTABLE python3)
find_program(CYTHON_EXECUTABLE cython)
find_program(R_EXECUTABLE R)

# List of compilers to use
set(COMPILERS "")
if(GCC_EXECUTABLE)
    list(APPEND COMPILERS "gcc")
    message(STATUS "GCC found: ${GCC_EXECUTABLE}")
endif()
if(CLANG_EXECUTABLE)
    list(APPEND COMPILERS "clang")
    message(STATUS "Clang found: ${CLANG_EXECUTABLE}")
endif()

if(NOT COMPILERS)
    message(FATAL_ERROR "No C++ compilers found (need g++ or clang++)")
endif()

# C++ Hello World implementations - build with each compiler
set(CPP_IMPLEMENTATIONS 001 002 003 004 005 006)
foreach(COMPILER ${COMPILERS})
    # Set compiler executable
    if(COMPILER STREQUAL "gcc")
        set(COMPILER_EXEC ${GCC_EXECUTABLE})
    elseif(COMPILER STREQUAL "clang")
        set(COMPILER_EXEC ${CLANG_EXECUTABLE})
    endif()
    
    foreach(IMPL ${CPP_IMPLEMENTATIONS})
        set(CPP_SRC ${TEST_SRC_DIR}/cpp/hello_world/${IMPL}/hello.cpp)
        set(CPP_BIN ${TEST_BIN_DIR}/${COMPILER}/cpp/hello_world/${IMPL}/hello)
        
        if(EXISTS ${CPP_SRC})
            file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/cpp/hello_world/${IMPL})
            
            add_custom_command(
                OUTPUT ${CPP_BIN}
                COMMAND ${COMPILER_EXEC} ${CPP_SRC} -o ${CPP_BIN} -O3 -std=c++17
                DEPENDS ${CPP_SRC}
                COMMENT "Building C++ hello_world/${IMPL} with ${COMPILER}..."
                VERBATIM
            )
            
            add_custom_target(${COMPILER}_cpp_hello_${IMPL} ALL DEPENDS ${CPP_BIN})
        endif()
    endforeach()
endforeach()

# ASM Hello World 001 - Support multiple assemblers
set(ASSEMBLERS "")
if(NASM_EXECUTABLE)
    list(APPEND ASSEMBLERS "nasm")
endif()
if(YASM_EXECUTABLE)
    list(APPEND ASSEMBLERS "yasm")
endif()
if(GAS_EXECUTABLE)
    list(APPEND ASSEMBLERS "gas")
endif()

if(ASSEMBLERS AND LD_EXECUTABLE)
    foreach(ASSEMBLER ${ASSEMBLERS})
        foreach(COMPILER ${COMPILERS})
            if(ASSEMBLER STREQUAL "nasm")
                set(ASM_001_SRC ${TEST_SRC_DIR}/asm/hello_world/001/hello.asm)
                set(ASM_001_OBJ ${TEST_BIN_DIR}/${COMPILER}/asm_${ASSEMBLER}/hello_world/001/hello.o)
                set(ASM_001_BIN ${TEST_BIN_DIR}/${COMPILER}/asm_${ASSEMBLER}/hello_world/001/hello)
                file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/asm_${ASSEMBLER}/hello_world/001)
                
                add_custom_command(
                    OUTPUT ${ASM_001_OBJ}
                    COMMAND ${NASM_EXECUTABLE} -f elf64 ${ASM_001_SRC} -o ${ASM_001_OBJ}
                    DEPENDS ${ASM_001_SRC}
                    COMMENT "Assembling ASM hello_world/001 with NASM for ${COMPILER}..."
                    VERBATIM
                )
                
                add_custom_command(
                    OUTPUT ${ASM_001_BIN}
                    COMMAND ${LD_EXECUTABLE} ${ASM_001_OBJ} -o ${ASM_001_BIN}
                    DEPENDS ${ASM_001_OBJ}
                    COMMENT "Linking ASM hello_world/001 (NASM) for ${COMPILER}..."
                    VERBATIM
                )
                
                add_custom_target(${COMPILER}_asm_${ASSEMBLER}_hello_001 ALL DEPENDS ${ASM_001_BIN})
                
            elseif(ASSEMBLER STREQUAL "yasm")
                set(ASM_001_SRC ${TEST_SRC_DIR}/asm/hello_world/001/hello_yasm.asm)
                set(ASM_001_OBJ ${TEST_BIN_DIR}/${COMPILER}/asm_${ASSEMBLER}/hello_world/001/hello.o)
                set(ASM_001_BIN ${TEST_BIN_DIR}/${COMPILER}/asm_${ASSEMBLER}/hello_world/001/hello)
                file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/asm_${ASSEMBLER}/hello_world/001)
                
                add_custom_command(
                    OUTPUT ${ASM_001_OBJ}
                    COMMAND ${YASM_EXECUTABLE} -f elf64 ${ASM_001_SRC} -o ${ASM_001_OBJ}
                    DEPENDS ${ASM_001_SRC}
                    COMMENT "Assembling ASM hello_world/001 with YASM for ${COMPILER}..."
                    VERBATIM
                )
                
                add_custom_command(
                    OUTPUT ${ASM_001_BIN}
                    COMMAND ${LD_EXECUTABLE} ${ASM_001_OBJ} -o ${ASM_001_BIN}
                    DEPENDS ${ASM_001_OBJ}
                    COMMENT "Linking ASM hello_world/001 (YASM) for ${COMPILER}..."
                    VERBATIM
                )
                
                add_custom_target(${COMPILER}_asm_${ASSEMBLER}_hello_001 ALL DEPENDS ${ASM_001_BIN})
                
            elseif(ASSEMBLER STREQUAL "gas")
                set(ASM_001_SRC ${TEST_SRC_DIR}/asm/hello_world/001/hello_gas.s)
                set(ASM_001_OBJ ${TEST_BIN_DIR}/${COMPILER}/asm_${ASSEMBLER}/hello_world/001/hello.o)
                set(ASM_001_BIN ${TEST_BIN_DIR}/${COMPILER}/asm_${ASSEMBLER}/hello_world/001/hello)
                file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/asm_${ASSEMBLER}/hello_world/001)
                
                add_custom_command(
                    OUTPUT ${ASM_001_OBJ}
                    COMMAND ${GAS_EXECUTABLE} ${ASM_001_SRC} -o ${ASM_001_OBJ}
                    DEPENDS ${ASM_001_SRC}
                    COMMENT "Assembling ASM hello_world/001 with GAS for ${COMPILER}..."
                    VERBATIM
                )
                
                add_custom_command(
                    OUTPUT ${ASM_001_BIN}
                    COMMAND ${LD_EXECUTABLE} ${ASM_001_OBJ} -o ${ASM_001_BIN}
                    DEPENDS ${ASM_001_OBJ}
                    COMMENT "Linking ASM hello_world/001 (GAS) for ${COMPILER}..."
                    VERBATIM
                )
                
                add_custom_target(${COMPILER}_asm_${ASSEMBLER}_hello_001 ALL DEPENDS ${ASM_001_BIN})
            endif()
        endforeach()
    endforeach()
    message(STATUS "ASM build enabled with assemblers: ${ASSEMBLERS}")
else()
    message(WARNING "No assembler or linker found, skipping ASM builds")
endif()

# Rust Hello World 001 - build with different linkers
if(RUSTC_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(RUST_001_SRC ${TEST_SRC_DIR}/rust/hello_world/001/main.rs)
        set(RUST_001_BIN ${TEST_BIN_DIR}/${COMPILER}/rust/hello_world/001/hello)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/rust/hello_world/001)
        
        # Use different linker based on compiler
        if(COMPILER STREQUAL "gcc")
            set(RUST_LINKER "gcc")
        elseif(COMPILER STREQUAL "clang")
            set(RUST_LINKER "clang")
        endif()
        
        add_custom_command(
            OUTPUT ${RUST_001_BIN}
            COMMAND ${RUSTC_EXECUTABLE} ${RUST_001_SRC} -o ${RUST_001_BIN} -C opt-level=3 -C linker=${RUST_LINKER}
            DEPENDS ${RUST_001_SRC}
            COMMENT "Building Rust hello_world/001 with ${COMPILER} linker..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_rust_hello_001 ALL DEPENDS ${RUST_001_BIN})
    endforeach()
    message(STATUS "Rust build enabled")
else()
    message(WARNING "rustc not found, skipping Rust builds")
endif()

# Zig Hello World 001 - Zig has its own compiler, build for each configuration
if(ZIG_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(ZIG_001_SRC ${TEST_SRC_DIR}/zig/hello_world/001/main.zig)
        set(ZIG_001_BIN ${TEST_BIN_DIR}/${COMPILER}/zig/hello_world/001/hello)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/zig/hello_world/001)
        
        add_custom_command(
            OUTPUT ${ZIG_001_BIN}
            COMMAND ${ZIG_EXECUTABLE} build-exe ${ZIG_001_SRC} -O ReleaseFast -femit-bin=${ZIG_001_BIN}
            DEPENDS ${ZIG_001_SRC}
            COMMENT "Building Zig hello_world/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_zig_hello_001 ALL DEPENDS ${ZIG_001_BIN})
    endforeach()
    message(STATUS "Zig build enabled")
else()
    message(WARNING "zig not found, skipping Zig builds")
endif()

# C Hello World 001 - build with gcc and clang
if(GCC_C_EXECUTABLE OR CLANG_C_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        # Set C compiler executable
        if(COMPILER STREQUAL "gcc" AND GCC_C_EXECUTABLE)
            set(C_COMPILER_EXEC ${GCC_C_EXECUTABLE})
        elseif(COMPILER STREQUAL "clang" AND CLANG_C_EXECUTABLE)
            set(C_COMPILER_EXEC ${CLANG_C_EXECUTABLE})
        else()
            continue()
        endif()
        
        set(C_001_SRC ${TEST_SRC_DIR}/c/hello_world/001/hello.c)
        set(C_001_BIN ${TEST_BIN_DIR}/${COMPILER}/c/hello_world/001/hello)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/c/hello_world/001)
        
        add_custom_command(
            OUTPUT ${C_001_BIN}
            COMMAND ${C_COMPILER_EXEC} ${C_001_SRC} -o ${C_001_BIN} -O3
            DEPENDS ${C_001_SRC}
            COMMENT "Building C hello_world/001 with ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_c_hello_001 ALL DEPENDS ${C_001_BIN})
    endforeach()
    message(STATUS "C build enabled")
else()
    message(WARNING "gcc or clang not found, skipping C builds")
endif()

# Go Hello World 001 - Go compiler is independent
if(GO_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(GO_001_SRC ${TEST_SRC_DIR}/go/hello_world/001/main.go)
        set(GO_001_BIN ${TEST_BIN_DIR}/${COMPILER}/go/hello_world/001/hello)
        set(GO_001_DIR ${TEST_SRC_DIR}/go/hello_world/001)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/go/hello_world/001)
        
        add_custom_command(
            OUTPUT ${GO_001_BIN}
            COMMAND ${GO_EXECUTABLE} build -o ${GO_001_BIN} -ldflags=-s\ -w ${GO_001_SRC}
            WORKING_DIRECTORY ${GO_001_DIR}
            DEPENDS ${GO_001_SRC}
            COMMENT "Building Go hello_world/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_go_hello_001 ALL DEPENDS ${GO_001_BIN})
    endforeach()
    message(STATUS "Go build enabled")
else()
    message(WARNING "go not found, skipping Go builds")
endif()

# D Hello World 001 - try multiple D compilers
set(D_COMPILER "")
if(LDC2_EXECUTABLE)
    set(D_COMPILER ${LDC2_EXECUTABLE})
    set(D_COMPILER_NAME "ldc2")
    set(D_FLAGS "-O3;-release")
    message(STATUS "D compiler: ldc2 (LLVM-based)")
elseif(DMD_EXECUTABLE)
    set(D_COMPILER ${DMD_EXECUTABLE})
    set(D_COMPILER_NAME "dmd")
    set(D_FLAGS "-O;-release")
    message(STATUS "D compiler: dmd (reference)")
elseif(GDC_EXECUTABLE)
    set(D_COMPILER ${GDC_EXECUTABLE})
    set(D_COMPILER_NAME "gdc")
    set(D_FLAGS "-O3;-frelease")
    message(STATUS "D compiler: gdc (GCC-based)")
endif()

if(D_COMPILER)
    foreach(COMPILER ${COMPILERS})
        set(D_001_SRC ${TEST_SRC_DIR}/d/hello_world/001/hello.d)
        set(D_001_BIN ${TEST_BIN_DIR}/${COMPILER}/d/hello_world/001/hello)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/d/hello_world/001)
        
        add_custom_command(
            OUTPUT ${D_001_BIN}
            COMMAND ${D_COMPILER} ${D_001_SRC} -of=${D_001_BIN} ${D_FLAGS}
            DEPENDS ${D_001_SRC}
            COMMENT "Building D hello_world/001 with ${D_COMPILER_NAME} for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_d_hello_001 ALL DEPENDS ${D_001_BIN})
    endforeach()
    message(STATUS "D build enabled")
else()
    message(WARNING "No D compiler found (dmd, ldc2, or gdc), skipping D builds")
endif()

# Nim Hello World 001
if(NIM_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(NIM_001_SRC ${TEST_SRC_DIR}/nim/hello_world/001/hello.nim)
        set(NIM_001_BIN ${TEST_BIN_DIR}/${COMPILER}/nim/hello_world/001/hello)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/nim/hello_world/001)
        
        add_custom_command(
            OUTPUT ${NIM_001_BIN}
            COMMAND ${NIM_EXECUTABLE} c -d:release --opt:speed -o:${NIM_001_BIN} ${NIM_001_SRC}
            DEPENDS ${NIM_001_SRC}
            COMMENT "Building Nim hello_world/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_nim_hello_001 ALL DEPENDS ${NIM_001_BIN})
    endforeach()
    message(STATUS "Nim build enabled")
else()
    message(WARNING "nim not found, skipping Nim builds")
endif()

# Odin Hello World 001
if(ODIN_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(ODIN_001_SRC ${TEST_SRC_DIR}/odin/hello_world/001/hello.odin)
        set(ODIN_001_BIN ${TEST_BIN_DIR}/${COMPILER}/odin/hello_world/001/hello)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/odin/hello_world/001)
        
        add_custom_command(
            OUTPUT ${ODIN_001_BIN}
            COMMAND ${ODIN_EXECUTABLE} build ${ODIN_001_SRC} -file -o:speed -out:${ODIN_001_BIN}
            DEPENDS ${ODIN_001_SRC}
            COMMENT "Building Odin hello_world/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_odin_hello_001 ALL DEPENDS ${ODIN_001_BIN})
    endforeach()
    message(STATUS "Odin build enabled")
else()
    message(WARNING "odin not found, skipping Odin builds")
endif()

# V Hello World 001
if(V_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(V_001_SRC ${TEST_SRC_DIR}/v/hello_world/001/hello.v)
        set(V_001_BIN ${TEST_BIN_DIR}/${COMPILER}/v/hello_world/001/hello)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/v/hello_world/001)
        
        add_custom_command(
            OUTPUT ${V_001_BIN}
            COMMAND ${V_EXECUTABLE} -prod -o ${V_001_BIN} ${V_001_SRC}
            DEPENDS ${V_001_SRC}
            COMMENT "Building V hello_world/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_v_hello_001 ALL DEPENDS ${V_001_BIN})
    endforeach()
    message(STATUS "V build enabled")
else()
    message(WARNING "v not found, skipping V builds")
endif()

# Ada Hello World 001
if(GNAT_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(ADA_001_SRC ${TEST_SRC_DIR}/ada/hello_world/001/hello.adb)
        set(ADA_001_BIN ${TEST_BIN_DIR}/${COMPILER}/ada/hello_world/001/hello)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/ada/hello_world/001)
        
        add_custom_command(
            OUTPUT ${ADA_001_BIN}
            COMMAND ${GNAT_EXECUTABLE} ${ADA_001_SRC} -O3 -o ${ADA_001_BIN}
            DEPENDS ${ADA_001_SRC}
            COMMENT "Building Ada hello_world/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_ada_hello_001 ALL DEPENDS ${ADA_001_BIN})
    endforeach()
    message(STATUS "Ada build enabled")
else()
    message(WARNING "gnatmake not found, skipping Ada builds")
endif()

# Haskell Hello World 001
if(GHC_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(HASKELL_001_SRC ${TEST_SRC_DIR}/haskell/hello_world/001/hello.hs)
        set(HASKELL_001_BIN ${TEST_BIN_DIR}/${COMPILER}/haskell/hello_world/001/hello)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/haskell/hello_world/001)
        
        add_custom_command(
            OUTPUT ${HASKELL_001_BIN}
            COMMAND ${GHC_EXECUTABLE} -O2 -outputdir ${TEST_BIN_DIR}/${COMPILER}/haskell/hello_world/001 -o ${HASKELL_001_BIN} ${HASKELL_001_SRC}
            DEPENDS ${HASKELL_001_SRC}
            COMMENT "Building Haskell hello_world/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_haskell_hello_001 ALL DEPENDS ${HASKELL_001_BIN})
    endforeach()
    message(STATUS "Haskell build enabled")
else()
    message(WARNING "ghc not found, skipping Haskell builds")
endif()

# Fortran Hello World 001
if(GFORTRAN_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(FORTRAN_001_SRC ${TEST_SRC_DIR}/fortran/hello_world/001/hello.f90)
        set(FORTRAN_001_BIN ${TEST_BIN_DIR}/${COMPILER}/fortran/hello_world/001/hello)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/fortran/hello_world/001)
        
        add_custom_command(
            OUTPUT ${FORTRAN_001_BIN}
            COMMAND ${GFORTRAN_EXECUTABLE} ${FORTRAN_001_SRC} -O3 -o ${FORTRAN_001_BIN}
            DEPENDS ${FORTRAN_001_SRC}
            COMMENT "Building Fortran hello_world/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_fortran_hello_001 ALL DEPENDS ${FORTRAN_001_BIN})
    endforeach()
    message(STATUS "Fortran build enabled")
else()
    message(WARNING "gfortran not found, skipping Fortran builds")
endif()

# Pascal Hello World 001
if(FPC_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(PASCAL_001_SRC ${TEST_SRC_DIR}/pascal/hello_world/001/hello.pas)
        set(PASCAL_001_BIN ${TEST_BIN_DIR}/${COMPILER}/pascal/hello_world/001/hello)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/pascal/hello_world/001)
        
        add_custom_command(
            OUTPUT ${PASCAL_001_BIN}
            COMMAND ${FPC_EXECUTABLE} ${PASCAL_001_SRC} -O3 -o${PASCAL_001_BIN}
            DEPENDS ${PASCAL_001_SRC}
            COMMENT "Building Pascal hello_world/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_pascal_hello_001 ALL DEPENDS ${PASCAL_001_BIN})
    endforeach()
    message(STATUS "Pascal build enabled")
else()
    message(WARNING "fpc not found, skipping Pascal builds")
endif()

# COBOL Hello World 001
if(COBC_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(COBOL_001_SRC ${TEST_SRC_DIR}/cobol/hello_world/001/hello.cob)
        set(COBOL_001_BIN ${TEST_BIN_DIR}/${COMPILER}/cobol/hello_world/001/hello)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/cobol/hello_world/001)
        
        add_custom_command(
            OUTPUT ${COBOL_001_BIN}
            COMMAND ${COBC_EXECUTABLE} -x -O3 -o ${COBOL_001_BIN} ${COBOL_001_SRC}
            DEPENDS ${COBOL_001_SRC}
            COMMENT "Building COBOL hello_world/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_cobol_hello_001 ALL DEPENDS ${COBOL_001_BIN})
    endforeach()
    message(STATUS "COBOL build enabled")
else()
    message(WARNING "cobc not found, skipping COBOL builds")
endif()

# C# Hello World 001
set(CSHARP_COMPILER "")
if(MCS_EXECUTABLE)
    set(CSHARP_COMPILER ${MCS_EXECUTABLE})
    set(CSHARP_FLAGS "-optimize+")
    message(STATUS "C# compiler: mcs (Mono)")
endif()

if(CSHARP_COMPILER)
    foreach(COMPILER ${COMPILERS})
        set(CSHARP_001_SRC ${TEST_SRC_DIR}/csharp/hello_world/001/hello.cs)
        set(CSHARP_001_BIN ${TEST_BIN_DIR}/${COMPILER}/csharp/hello_world/001/hello.exe)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/csharp/hello_world/001)
        
        add_custom_command(
            OUTPUT ${CSHARP_001_BIN}
            COMMAND ${CSHARP_COMPILER} ${CSHARP_FLAGS} -out:${CSHARP_001_BIN} ${CSHARP_001_SRC}
            DEPENDS ${CSHARP_001_SRC}
            COMMENT "Building C# hello_world/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_csharp_hello_001 ALL DEPENDS ${CSHARP_001_BIN})
    endforeach()
    message(STATUS "C# build enabled")
else()
    message(WARNING "No C# compiler found (csc or mcs), skipping C# builds")
endif()

# Python Hello World 001 - Compile with Cython
if(CYTHON_EXECUTABLE AND PYTHON_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(PY_001_SRC ${TEST_SRC_DIR}/python/hello_world/001/hello.py)
        set(PY_001_C ${TEST_BIN_DIR}/${COMPILER}/python/hello_world/001/hello.c)
        set(PY_001_BIN ${TEST_BIN_DIR}/${COMPILER}/python/hello_world/001/hello)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/python/hello_world/001)
        
        # Get Python include and library paths
        execute_process(
            COMMAND ${PYTHON_EXECUTABLE} -c "import sysconfig; print(sysconfig.get_path('include'))"
            OUTPUT_VARIABLE PYTHON_INCLUDE_DIR
            OUTPUT_STRIP_TRAILING_WHITESPACE
        )
        
        execute_process(
            COMMAND ${PYTHON_EXECUTABLE} -c "import sysconfig; print(sysconfig.get_config_var('LIBDIR'))"
            OUTPUT_VARIABLE PYTHON_LIB_DIR
            OUTPUT_STRIP_TRAILING_WHITESPACE
        )
        
        execute_process(
            COMMAND ${PYTHON_EXECUTABLE} -c "import sysconfig; print(sysconfig.get_config_var('LDVERSION'))"
            OUTPUT_VARIABLE PYTHON_LDVERSION
            OUTPUT_STRIP_TRAILING_WHITESPACE
        )
        
        # Set compiler executable
        if(COMPILER STREQUAL "gcc")
            set(C_COMPILER ${GCC_C_EXECUTABLE})
        elseif(COMPILER STREQUAL "clang")
            set(C_COMPILER ${CLANG_C_EXECUTABLE})
        endif()
        
        # Step 1: Cython compile .py to .c with --embed flag for standalone executable
        add_custom_command(
            OUTPUT ${PY_001_C}
            COMMAND ${CYTHON_EXECUTABLE} --embed -o ${PY_001_C} ${PY_001_SRC}
            DEPENDS ${PY_001_SRC}
            COMMENT "Cythonizing Python hello_world/001 for ${COMPILER}..."
            VERBATIM
        )
        
        # Step 2: Compile .c to executable
        add_custom_command(
            OUTPUT ${PY_001_BIN}
            COMMAND ${C_COMPILER} -O3 ${PY_001_C} -I${PYTHON_INCLUDE_DIR} -L${PYTHON_LIB_DIR} -lpython${PYTHON_LDVERSION} -o ${PY_001_BIN}
            DEPENDS ${PY_001_C}
            COMMENT "Compiling Cython hello_world/001 with ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_python_hello_001 ALL DEPENDS ${PY_001_BIN})
    endforeach()
    message(STATUS "Python (Cython) build enabled")
else()
    message(WARNING "Cython or Python not found, skipping Python builds")
endif()

# R Hello World 001 - Compile with embedded R
if(R_EXECUTABLE)
    # Get R include and library paths
    execute_process(
        COMMAND ${R_EXECUTABLE} CMD config --cppflags
        OUTPUT_VARIABLE R_INCLUDE_FLAGS_RAW
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    
    execute_process(
        COMMAND ${R_EXECUTABLE} CMD config --ldflags
        OUTPUT_VARIABLE R_LD_FLAGS_RAW
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    
    # Remove -ltirpc which may not be available on all systems
    string(REPLACE "-ltirpc" "" R_LD_FLAGS_RAW "${R_LD_FLAGS_RAW}")
    
    # Convert to lists for proper handling
    separate_arguments(R_INCLUDE_FLAGS UNIX_COMMAND "${R_INCLUDE_FLAGS_RAW}")
    separate_arguments(R_LD_FLAGS UNIX_COMMAND "${R_LD_FLAGS_RAW}")
    
    foreach(COMPILER ${COMPILERS})
        set(R_001_WRAPPER ${TEST_SRC_DIR}/r/hello_world/001/hello_wrapper.c)
        set(R_001_BIN ${TEST_BIN_DIR}/${COMPILER}/r/hello_world/001/hello)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/r/hello_world/001)
        
        if(COMPILER STREQUAL "gcc")
            set(C_COMPILER ${GCC_C_EXECUTABLE})
        elseif(COMPILER STREQUAL "clang")
            set(C_COMPILER ${CLANG_C_EXECUTABLE})
        endif()
        
        add_custom_command(
            OUTPUT ${R_001_BIN}
            COMMAND ${C_COMPILER} ${R_001_WRAPPER} ${R_INCLUDE_FLAGS} ${R_LD_FLAGS} -o ${R_001_BIN}
            DEPENDS ${R_001_WRAPPER}
            COMMENT "Building R hello_world/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_r_hello_001 ALL DEPENDS ${R_001_BIN})
    endforeach()
    message(STATUS "R build enabled")
else()
    message(WARNING "R not found, skipping R builds")
endif()

message(STATUS "Test binaries output directory: ${TEST_BIN_DIR}")

# ======================================================================
# Build benchmark tool
# ======================================================================

set(BENCHMARK_SRC ${CMAKE_SOURCE_DIR}/src/benchmark.cpp)
set(BENCHMARK_BIN ${BIN_DIR}/benchmark)

add_executable(benchmark_tool ${BENCHMARK_SRC})
set_target_properties(benchmark_tool PROPERTIES
    OUTPUT_NAME benchmark
    RUNTIME_OUTPUT_DIRECTORY ${BIN_DIR}
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED ON
)

target_compile_options(benchmark_tool PRIVATE -O3 -Wall -Wextra)
target_link_libraries(benchmark_tool stdc++fs)

message(STATUS "Benchmark tool will be built to: ${BIN_DIR}/benchmark")

# ======================================================================
# FIRST 1000 PRIMES SERIES
# ======================================================================

# C++ first1000primes 001
foreach(COMPILER ${COMPILERS})
    if(COMPILER STREQUAL "gcc")
        set(COMPILER_EXEC ${GCC_EXECUTABLE})
    elseif(COMPILER STREQUAL "clang")
        set(COMPILER_EXEC ${CLANG_EXECUTABLE})
    endif()
    
    set(CPP_PRIMES_SRC ${TEST_SRC_DIR}/cpp/first1000primes/001/primes.cpp)
    set(CPP_PRIMES_BIN ${TEST_BIN_DIR}/${COMPILER}/cpp/first1000primes/001/primes)
    file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/cpp/first1000primes/001)
    
    add_custom_command(
        OUTPUT ${CPP_PRIMES_BIN}
        COMMAND ${COMPILER_EXEC} ${CPP_PRIMES_SRC} -o ${CPP_PRIMES_BIN} -O3 -std=c++17
        DEPENDS ${CPP_PRIMES_SRC}
        COMMENT "Building C++ first1000primes/001 with ${COMPILER}..."
        VERBATIM
    )
    
    add_custom_target(${COMPILER}_cpp_primes_001 ALL DEPENDS ${CPP_PRIMES_BIN})
endforeach()

# C first1000primes 001
if(GCC_C_EXECUTABLE OR CLANG_C_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        if(COMPILER STREQUAL "gcc")
            set(C_COMPILER ${GCC_C_EXECUTABLE})
        elseif(COMPILER STREQUAL "clang")
            set(C_COMPILER ${CLANG_C_EXECUTABLE})
        endif()
        
        if(C_COMPILER)
            set(C_PRIMES_SRC ${TEST_SRC_DIR}/c/first1000primes/001/primes.c)
            set(C_PRIMES_BIN ${TEST_BIN_DIR}/${COMPILER}/c/first1000primes/001/primes)
            file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/c/first1000primes/001)
            
            add_custom_command(
                OUTPUT ${C_PRIMES_BIN}
                COMMAND ${C_COMPILER} ${C_PRIMES_SRC} -o ${C_PRIMES_BIN} -O3 -std=c11 -lm
                DEPENDS ${C_PRIMES_SRC}
                COMMENT "Building C first1000primes/001 with ${COMPILER}..."
                VERBATIM
            )
            
            add_custom_target(${COMPILER}_c_primes_001 ALL DEPENDS ${C_PRIMES_BIN})
        endif()
    endforeach()
endif()

# ASM first1000primes 001 - NASM only
if(NASM_EXECUTABLE AND LD_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(ASM_PRIMES_SRC ${TEST_SRC_DIR}/asm/first1000primes/001/primes.asm)
        set(ASM_PRIMES_OBJ ${TEST_BIN_DIR}/${COMPILER}/asm/first1000primes/001/primes.o)
        set(ASM_PRIMES_BIN ${TEST_BIN_DIR}/${COMPILER}/asm/first1000primes/001/primes)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/asm/first1000primes/001)
        
        add_custom_command(
            OUTPUT ${ASM_PRIMES_OBJ}
            COMMAND ${NASM_EXECUTABLE} -f elf64 ${ASM_PRIMES_SRC} -o ${ASM_PRIMES_OBJ}
            DEPENDS ${ASM_PRIMES_SRC}
            COMMENT "Assembling ASM first1000primes/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_command(
            OUTPUT ${ASM_PRIMES_BIN}
            COMMAND ${LD_EXECUTABLE} ${ASM_PRIMES_OBJ} -o ${ASM_PRIMES_BIN}
            DEPENDS ${ASM_PRIMES_OBJ}
            COMMENT "Linking ASM first1000primes/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_asm_primes_001 ALL DEPENDS ${ASM_PRIMES_BIN})
    endforeach()
endif()

# Rust first1000primes 001
if(RUSTC_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(RUST_PRIMES_SRC ${TEST_SRC_DIR}/rust/first1000primes/001/primes.rs)
        set(RUST_PRIMES_BIN ${TEST_BIN_DIR}/${COMPILER}/rust/first1000primes/001/primes)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/rust/first1000primes/001)
        
        add_custom_command(
            OUTPUT ${RUST_PRIMES_BIN}
            COMMAND ${RUSTC_EXECUTABLE} ${RUST_PRIMES_SRC} -o ${RUST_PRIMES_BIN} -C opt-level=3
            DEPENDS ${RUST_PRIMES_SRC}
            COMMENT "Building Rust first1000primes/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_rust_primes_001 ALL DEPENDS ${RUST_PRIMES_BIN})
    endforeach()
endif()

# Zig first1000primes 001
if(ZIG_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(ZIG_PRIMES_SRC ${TEST_SRC_DIR}/zig/first1000primes/001/primes.zig)
        set(ZIG_PRIMES_BIN ${TEST_BIN_DIR}/${COMPILER}/zig/first1000primes/001/primes)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/zig/first1000primes/001)
        
        add_custom_command(
            OUTPUT ${ZIG_PRIMES_BIN}
            COMMAND ${ZIG_EXECUTABLE} build-exe ${ZIG_PRIMES_SRC} -O ReleaseFast -femit-bin=${ZIG_PRIMES_BIN}
            DEPENDS ${ZIG_PRIMES_SRC}
            COMMENT "Building Zig first1000primes/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_zig_primes_001 ALL DEPENDS ${ZIG_PRIMES_BIN})
    endforeach()
endif()

# Go first1000primes 001
if(GO_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(GO_PRIMES_SRC ${TEST_SRC_DIR}/go/first1000primes/001/primes.go)
        set(GO_PRIMES_BIN ${TEST_BIN_DIR}/${COMPILER}/go/first1000primes/001/primes)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/go/first1000primes/001)
        
        add_custom_command(
            OUTPUT ${GO_PRIMES_BIN}
            COMMAND ${GO_EXECUTABLE} build -o ${GO_PRIMES_BIN} ${GO_PRIMES_SRC}
            DEPENDS ${GO_PRIMES_SRC}
            WORKING_DIRECTORY ${TEST_SRC_DIR}/go/first1000primes/001
            COMMENT "Building Go first1000primes/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_go_primes_001 ALL DEPENDS ${GO_PRIMES_BIN})
    endforeach()
endif()

# D first1000primes 001
if(LDC2_EXECUTABLE)
    set(D_PRIMES_FLAGS "-O3")
    foreach(COMPILER ${COMPILERS})
        set(D_PRIMES_SRC ${TEST_SRC_DIR}/d/first1000primes/001/primes.d)
        set(D_PRIMES_BIN ${TEST_BIN_DIR}/${COMPILER}/d/first1000primes/001/primes)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/d/first1000primes/001)
        
        add_custom_command(
            OUTPUT ${D_PRIMES_BIN}
            COMMAND ${LDC2_EXECUTABLE} ${D_PRIMES_FLAGS} -of=${D_PRIMES_BIN} ${D_PRIMES_SRC}
            DEPENDS ${D_PRIMES_SRC}
            COMMENT "Building D first1000primes/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_d_primes_001 ALL DEPENDS ${D_PRIMES_BIN})
    endforeach()
endif()

# Nim first1000primes 001
if(NIM_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(NIM_PRIMES_SRC ${TEST_SRC_DIR}/nim/first1000primes/001/primes.nim)
        set(NIM_PRIMES_BIN ${TEST_BIN_DIR}/${COMPILER}/nim/first1000primes/001/primes)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/nim/first1000primes/001)
        
        add_custom_command(
            OUTPUT ${NIM_PRIMES_BIN}
            COMMAND ${NIM_EXECUTABLE} c -d:release -o:${NIM_PRIMES_BIN} ${NIM_PRIMES_SRC}
            DEPENDS ${NIM_PRIMES_SRC}
            COMMENT "Building Nim first1000primes/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_nim_primes_001 ALL DEPENDS ${NIM_PRIMES_BIN})
    endforeach()
endif()

# Ada first1000primes 001
if(GNAT_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(ADA_PRIMES_SRC ${TEST_SRC_DIR}/ada/first1000primes/001/primes.adb)
        set(ADA_PRIMES_BIN ${TEST_BIN_DIR}/${COMPILER}/ada/first1000primes/001/primes)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/ada/first1000primes/001)
        
        add_custom_command(
            OUTPUT ${ADA_PRIMES_BIN}
            COMMAND ${GNAT_EXECUTABLE} ${ADA_PRIMES_SRC} -o ${ADA_PRIMES_BIN} -O3 -gnatpn
            DEPENDS ${ADA_PRIMES_SRC}
            WORKING_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/ada/first1000primes/001
            COMMENT "Building Ada first1000primes/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_ada_primes_001 ALL DEPENDS ${ADA_PRIMES_BIN})
    endforeach()
endif()

# Haskell first1000primes 001
if(GHC_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(HS_PRIMES_SRC ${TEST_SRC_DIR}/haskell/first1000primes/001/primes.hs)
        set(HS_PRIMES_BIN ${TEST_BIN_DIR}/${COMPILER}/haskell/first1000primes/001/primes)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/haskell/first1000primes/001)
        
        add_custom_command(
            OUTPUT ${HS_PRIMES_BIN}
            COMMAND ${GHC_EXECUTABLE} -O2 -o ${HS_PRIMES_BIN} ${HS_PRIMES_SRC}
            DEPENDS ${HS_PRIMES_SRC}
            WORKING_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/haskell/first1000primes/001
            COMMENT "Building Haskell first1000primes/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_haskell_primes_001 ALL DEPENDS ${HS_PRIMES_BIN})
    endforeach()
endif()

# Fortran first1000primes 001
if(GFORTRAN_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(F90_PRIMES_SRC ${TEST_SRC_DIR}/fortran/first1000primes/001/primes.f90)
        set(F90_PRIMES_BIN ${TEST_BIN_DIR}/${COMPILER}/fortran/first1000primes/001/primes)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/fortran/first1000primes/001)
        
        add_custom_command(
            OUTPUT ${F90_PRIMES_BIN}
            COMMAND ${GFORTRAN_EXECUTABLE} ${F90_PRIMES_SRC} -o ${F90_PRIMES_BIN} -O3
            DEPENDS ${F90_PRIMES_SRC}
            COMMENT "Building Fortran first1000primes/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_fortran_primes_001 ALL DEPENDS ${F90_PRIMES_BIN})
    endforeach()
endif()

# Pascal first1000primes 001
if(FPC_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(PAS_PRIMES_SRC ${TEST_SRC_DIR}/pascal/first1000primes/001/primes.pas)
        set(PAS_PRIMES_BIN ${TEST_BIN_DIR}/${COMPILER}/pascal/first1000primes/001/primes)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/pascal/first1000primes/001)
        
        add_custom_command(
            OUTPUT ${PAS_PRIMES_BIN}
            COMMAND ${FPC_EXECUTABLE} ${PAS_PRIMES_SRC} -O3 -FE${TEST_BIN_DIR}/${COMPILER}/pascal/first1000primes/001 -oprimes
            DEPENDS ${PAS_PRIMES_SRC}
            COMMENT "Building Pascal first1000primes/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_pascal_primes_001 ALL DEPENDS ${PAS_PRIMES_BIN})
    endforeach()
endif()

# COBOL first1000primes 001
if(COBC_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(COB_PRIMES_SRC ${TEST_SRC_DIR}/cobol/first1000primes/001/primes.cob)
        set(COB_PRIMES_BIN ${TEST_BIN_DIR}/${COMPILER}/cobol/first1000primes/001/primes)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/cobol/first1000primes/001)
        
        add_custom_command(
            OUTPUT ${COB_PRIMES_BIN}
            COMMAND ${COBC_EXECUTABLE} -x ${COB_PRIMES_SRC} -o ${COB_PRIMES_BIN} -O3
            DEPENDS ${COB_PRIMES_SRC}
            COMMENT "Building COBOL first1000primes/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_cobol_primes_001 ALL DEPENDS ${COB_PRIMES_BIN})
    endforeach()
endif()

# C# first1000primes 001
if(CSHARP_COMPILER)
    foreach(COMPILER ${COMPILERS})
        set(CS_PRIMES_SRC ${TEST_SRC_DIR}/csharp/first1000primes/001/primes.cs)
        set(CS_PRIMES_BIN ${TEST_BIN_DIR}/${COMPILER}/csharp/first1000primes/001/primes.exe)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/csharp/first1000primes/001)
        
        add_custom_command(
            OUTPUT ${CS_PRIMES_BIN}
            COMMAND ${CSHARP_COMPILER} ${CSHARP_FLAGS} -out:${CS_PRIMES_BIN} ${CS_PRIMES_SRC}
            DEPENDS ${CS_PRIMES_SRC}
            COMMENT "Building C# first1000primes/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_csharp_primes_001 ALL DEPENDS ${CS_PRIMES_BIN})
    endforeach()
endif()

# Python first1000primes 001
if(CYTHON_EXECUTABLE AND PYTHON_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(PY_PRIMES_SRC ${TEST_SRC_DIR}/python/first1000primes/001/primes.py)
        set(PY_PRIMES_C ${TEST_BIN_DIR}/${COMPILER}/python/first1000primes/001/primes.c)
        set(PY_PRIMES_BIN ${TEST_BIN_DIR}/${COMPILER}/python/first1000primes/001/primes)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/python/first1000primes/001)
        
        if(COMPILER STREQUAL "gcc")
            set(C_COMPILER ${GCC_C_EXECUTABLE})
        elseif(COMPILER STREQUAL "clang")
            set(C_COMPILER ${CLANG_C_EXECUTABLE})
        endif()
        
        add_custom_command(
            OUTPUT ${PY_PRIMES_C}
            COMMAND ${CYTHON_EXECUTABLE} --embed -o ${PY_PRIMES_C} ${PY_PRIMES_SRC}
            DEPENDS ${PY_PRIMES_SRC}
            COMMENT "Cythonizing Python first1000primes/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_command(
            OUTPUT ${PY_PRIMES_BIN}
            COMMAND ${C_COMPILER} -O3 ${PY_PRIMES_C} -I${PYTHON_INCLUDE_DIR} -L${PYTHON_LIB_DIR} -lpython${PYTHON_LDVERSION} -o ${PY_PRIMES_BIN}
            DEPENDS ${PY_PRIMES_C}
            COMMENT "Compiling Cython first1000primes/001 with ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_python_primes_001 ALL DEPENDS ${PY_PRIMES_BIN})
    endforeach()
endif()

# R first1000primes 001
if(R_EXECUTABLE)
    foreach(COMPILER ${COMPILERS})
        set(R_PRIMES_WRAPPER ${TEST_SRC_DIR}/r/first1000primes/001/primes_wrapper.c)
        set(R_PRIMES_BIN ${TEST_BIN_DIR}/${COMPILER}/r/first1000primes/001/primes)
        file(MAKE_DIRECTORY ${TEST_BIN_DIR}/${COMPILER}/r/first1000primes/001)
        
        if(COMPILER STREQUAL "gcc")
            set(C_COMPILER ${GCC_C_EXECUTABLE})
        elseif(COMPILER STREQUAL "clang")
            set(C_COMPILER ${CLANG_C_EXECUTABLE})
        endif()
        
        add_custom_command(
            OUTPUT ${R_PRIMES_BIN}
            COMMAND ${C_COMPILER} ${R_PRIMES_WRAPPER} ${R_INCLUDE_FLAGS} ${R_LD_FLAGS} -o ${R_PRIMES_BIN}
            DEPENDS ${R_PRIMES_WRAPPER}
            COMMENT "Building R first1000primes/001 for ${COMPILER}..."
            VERBATIM
        )
        
        add_custom_target(${COMPILER}_r_primes_001 ALL DEPENDS ${R_PRIMES_BIN})
    endforeach()
endif()
