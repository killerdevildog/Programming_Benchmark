#include <R.h>
#include <Rinternals.h>
#include <Rembedded.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    setenv("R_HOME", "/usr/lib/R", 1);
    
    char *r_argv[] = {"R", "--slave", "--no-save", "--no-restore"};
    Rf_initEmbeddedR(4, r_argv);
    
    SEXP e;
    int errorOccurred;
    
    PROTECT(e = lang3(install("cat"), mkString("Hello, World!"), mkString("\n")));
    R_tryEval(e, R_GlobalEnv, &errorOccurred);
    UNPROTECT(1);
    
    Rf_endEmbeddedR(0);
    return 0;
}
