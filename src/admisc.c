#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <R_ext/Rdynload.h>


// copied from: https://gist.github.com/wch/3280369#file-unlockenvironment-r
#define FRAME_LOCK_MASK (1<<14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))

SEXP C_unlockEnvironment(SEXP env) {
    UNLOCK_FRAME(env);
    
    SEXP result = PROTECT( Rf_allocVector(LGLSXP, 1) );
    LOGICAL(result)[0] = FRAME_IS_LOCKED(env) == 0;
    UNPROTECT(1);
    return result;
}


/*
the following functions are adapted from package haven,
to avoid unnecessary dependency chain
*/

typedef union {
    double value;
    char byte[8];
} ieee_double;


#ifdef WORDS_BIGENDIAN
// First two bytes are sign & expoonent
// Last four bytes are 1954
const int TAG_BYTE = 3;
#else
const int TAG_BYTE = 4;
#endif

SEXP C_tagged_na(SEXP x) {
    int n = Rf_length(x);
    SEXP out = PROTECT(Rf_allocVector(REALSXP, n));

    for (int i = 0; i < n; ++i) {
        char xi = CHAR(STRING_ELT(x, i))[0];
        ieee_double y;

        y.value = NA_REAL;
        y.byte[TAG_BYTE] = xi;

        REAL(out)[i] = y.value;
    }

    UNPROTECT(1);
    return out;
}

SEXP C_is_tagged_na(SEXP x) {
    int n = Rf_length(x);
    SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));

    if (TYPEOF(x) != REALSXP) {
        for (int i = 0; i < n; ++i) {
            LOGICAL(out)[i] = 0;
        }
        return out;
    }
    else {    

        for (int i = 0; i < n; ++i) {
            double xi = REAL(x)[i];

            if (!isnan(xi)) {
                LOGICAL(out)[i] = false;
            } else {
                ieee_double y;
                y.value = xi;
                char tag = y.byte[TAG_BYTE];

                if (tag == '\0') {
                    LOGICAL(out)[i] = false;
                } else {
                    LOGICAL(out)[i] = true;
                }
            }
        }
    }

    UNPROTECT(1);
    return out;
}

SEXP C_na_tag(SEXP x) {
    
    int n = Rf_length(x);
    SEXP out = PROTECT(Rf_allocVector(STRSXP, n));

    for (int i = 0; i < n; ++i) {
        double xi = REAL(x)[i];

        if (!isnan(xi)) {
            SET_STRING_ELT(out, i, NA_STRING);
        } else {
            ieee_double y;
            y.value = xi;
            char tag = y.byte[TAG_BYTE];

            if (tag == '\0') {
                SET_STRING_ELT(out, i, NA_STRING);
            } else {
                SET_STRING_ELT(out, i, Rf_mkCharLenCE(&tag, 1, CE_UTF8));
            }
        }
    }

    UNPROTECT(1);
    return out;
}
