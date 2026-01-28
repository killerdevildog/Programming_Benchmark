#include <R.h>
#include <Rinternals.h>
#include <Rembedded.h>
#include <R_ext/Parse.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    setenv("R_HOME", "/usr/lib/R", 1);
    
    char *r_argv[] = {"R", "--slave", "--no-save", "--no-restore"};
    Rf_initEmbeddedR(4, r_argv);
    
    SEXP e, code_str, parsed;
    int errorOccurred;
    ParseStatus status;
    
    // R code as a string
    const char *r_code = 
        "is_prime <- function(n) {"
        "  if (n < 2) return(FALSE);"
        "  if (n == 2) return(TRUE);"
        "  if (n %% 2 == 0) return(FALSE);"
        "  i <- 3;"
        "  while (i * i <= n) {"
        "    if (n %% i == 0) return(FALSE);"
        "    i <- i + 2;"
        "  };"
        "  return(TRUE);"
        "};"
        "count <- 0;"
        "num <- 2;"
        "last_prime <- 0;"
        "while (count < 1000) {"
        "  if (is_prime(num)) {"
        "    last_prime <- num;"
        "    count <- count + 1;"
        "  };"
        "  num <- num + 1;"
        "};\n"
        "writeLines(as.character(last_prime));";
    
    // Parse the R code using ParseVector
    PROTECT(code_str = mkString(r_code));
    PROTECT(parsed = R_ParseVector(code_str, -1, &status, R_NilValue));
    UNPROTECT(1);  // code_str
    
    if (status == PARSE_OK) {
        // Evaluate each expression
        int n = length(parsed);
        for (int i = 0; i < n; i++) {
            PROTECT(e = VECTOR_ELT(parsed, i));
            R_tryEval(e, R_GlobalEnv, &errorOccurred);
            UNPROTECT(1);
            if (errorOccurred) break;
        }
    }
    UNPROTECT(1);  // parsed
    
    Rf_endEmbeddedR(0);
    return 0;
}
