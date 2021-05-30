#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <R_ext/Rdynload.h>


// copied from: https://gist.github.com/wch/3280369#file-unlockenvironment-r
#define FRAME_LOCK_MASK (1<<14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))

SEXP _unlockEnvironment(SEXP env) {
    UNLOCK_FRAME(env);
    
    SEXP result = PROTECT( Rf_allocVector(LGLSXP, 1) );
    LOGICAL(result)[0] = FRAME_IS_LOCKED(env) == 0;
    UNPROTECT(1);
    return result;
}
