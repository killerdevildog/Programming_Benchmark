#include <iostream>
#include <vector>
#include <string>
#include <filesystem>
#include <algorithm>
#include <cstdlib>
#include <sstream>

namespace fs = std::filesystem;

struct TestBinary {
    std::string compiler;
    std::string language;
    std::string implementation;
    fs::path path;
    
    std::string getName() const {
        return compiler + "_" + language + "_" + implementation;
    }
};

std::vector<TestBinary> findTestBinaries(const fs::path& testDir, const std::string& testSeries) {
    std::vector<TestBinary> binaries;
    
    if (!fs::exists(testDir)) {
        std::cerr << "Error: Test directory does not exist: " << testDir << std::endl;
        return binaries;
    }
    
    // Iterate through compiler directories (gcc, clang, etc.)
    for (const auto& compilerEntry : fs::directory_iterator(testDir)) {
        if (!compilerEntry.is_directory()) continue;
        
        std::string compiler = compilerEntry.path().filename().string();
        
        // Iterate through language directories (cpp, asm, rust, etc.)
        for (const auto& langEntry : fs::directory_iterator(compilerEntry.path())) {
            if (!langEntry.is_directory()) continue;
            
            std::string language = langEntry.path().filename().string();
            fs::path seriesPath = langEntry.path() / testSeries;
            
            if (!fs::exists(seriesPath)) continue;
            
            // Iterate through implementation directories (001, 002, etc.)
            for (const auto& implEntry : fs::directory_iterator(seriesPath)) {
                if (!implEntry.is_directory()) continue;
                
                std::string implementation = implEntry.path().filename().string();
                
                // Find executable in the implementation directory
                for (const auto& fileEntry : fs::directory_iterator(implEntry.path())) {
                    // Skip .o files and only get executable files
                    if (fileEntry.is_regular_file() && 
                        fileEntry.path().extension() != ".o" &&
                        (fs::status(fileEntry).permissions() & fs::perms::owner_exec) != fs::perms::none) {
                        
                        binaries.push_back({
                            compiler,
                            language,
                            implementation,
                            fileEntry.path()
                        });
                        break; // Only take the first executable
                    }
                }
            }
        }
    }
    
    // Sort by compiler, language, then implementation
    std::sort(binaries.begin(), binaries.end(), 
        [](const TestBinary& a, const TestBinary& b) {
            if (a.compiler != b.compiler) return a.compiler < b.compiler;
            if (a.language != b.language) return a.language < b.language;
            return a.implementation < b.implementation;
        });
    
    return binaries;
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <test_series> [hyperfine_options...]" << std::endl;
        std::cerr << "Example: " << argv[0] << " hello_world" << std::endl;
        std::cerr << "Example: " << argv[0] << " hello_world --warmup 10 --runs 1000" << std::endl;
        return 1;
    }
    
    std::string testSeries = argv[1];
    
    // Get the directory where this executable is located
    fs::path exePath = fs::canonical("/proc/self/exe");
    fs::path binDir = exePath.parent_path();
    fs::path projectRoot = binDir.parent_path();
    fs::path testDir = projectRoot / "bin" / "tests";
    fs::path hyperfinePath = binDir / "hyperfine";
    
    // Check if hyperfine exists
    if (!fs::exists(hyperfinePath)) {
        std::cerr << "Error: hyperfine not found at: " << hyperfinePath << std::endl;
        std::cerr << "Please build hyperfine first with: cmake --build build" << std::endl;
        return 1;
    }
    
    std::cout << "==========================================================\n";
    std::cout << "  Benchmark Series: " << testSeries << "\n";
    std::cout << "==========================================================\n\n";
    
    // Find all test binaries
    auto binaries = findTestBinaries(testDir, testSeries);
    
    if (binaries.empty()) {
        std::cerr << "Error: No test binaries found for series '" << testSeries << "'" << std::endl;
        std::cerr << "Searched in: " << testDir << std::endl;
        return 1;
    }
    
    std::cout << "Found " << binaries.size() << " implementations:\n";
    for (const auto& bin : binaries) {
        std::cout << "  - " << bin.getName() << ": " << bin.path << "\n";
    }
    std::cout << "\n";
    
    // Build hyperfine command
    std::ostringstream cmd;
    cmd << hyperfinePath.string();
    
    // Add default options
    cmd << " --warmup 5";
    cmd << " --style full";
    cmd << " --export-markdown results_" << testSeries << ".md";
    cmd << " --export-json results_" << testSeries << ".json";
    
    // Add any additional user-provided options (skip argv[0] and argv[1])
    for (int i = 2; i < argc; ++i) {
        cmd << " " << argv[i];
    }
    
    // Add all binaries with their names
    for (const auto& bin : binaries) {
        cmd << " --command-name '" << bin.getName() << "'";
        cmd << " '" << bin.path.string() << "'";
    }
    
    std::cout << "Running benchmark...\n";
    std::cout << "Command: " << cmd.str() << "\n\n";
    std::cout << "==========================================================\n\n";
    
    // Execute hyperfine
    int result = std::system(cmd.str().c_str());
    
    if (result == 0) {
        std::cout << "\n==========================================================\n";
        std::cout << "Benchmark complete!\n";
        std::cout << "Results saved to:\n";
        std::cout << "  - results_" << testSeries << ".md (Markdown table)\n";
        std::cout << "  - results_" << testSeries << ".json (JSON data)\n";
        std::cout << "==========================================================\n";
    } else {
        std::cerr << "\nError: Benchmark failed with exit code " << result << std::endl;
        return 1;
    }
    
    return 0;
}
