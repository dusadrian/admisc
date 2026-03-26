#include <R.h>
#include <Rinternals.h>
#include <R_ext/Memory.h>
#include <limits.h>
#include <string.h>
#include "utils.h"


static R_INLINE unsigned long long int nchoosek(int n, int k) {
    if (k > n) return 0;
    if (k == 0 || k == n) return 1;

    unsigned long long int result = 1;

    if (k > n - k) {
        k = n - k;
    }

    for (int i = 0; i < k; i++) {
        if (result > ULLONG_MAX / (unsigned long long int) (n - i)) {
            return 0;
        }

        result *= (unsigned long long int) (n - i);

        if (result % (unsigned long long int) (i + 1) != 0) {
            return 0;
        }

        result /= (unsigned long long int) (i + 1);
    }

    return result;
}


void increment(
    int k,
    int *e,
    int *h,
    int nconds,
    int *tempk,
    int minval
) {

    if (k == 1) {
        tempk[0] += 1;
    }
    else {
        if (*e < nconds - *h) {
            *h = 1;
            tempk[k - 1] += 1;
            *e = tempk[k - 1];

            if (tempk[k - 1] < minval) {
                tempk[k - 1] = minval;
                *e = minval;
            }
        }
        else {
            *e = tempk[k - *h - 1] + 1;
            ++*h;

            Rboolean under = TRUE;
            for (int j = 0; j < *h; j++) {
                under = under && (*e + j < minval);
                tempk[k - *h + j] = *e + j;
            }

            if (under) {
                *h = 1;
                tempk[k - *h] = minval;
                *e = minval;
            }
        }
    }
}


SEXP C_ombnk(SEXP list) {
    // ogte = at least one value greater than or equal to
    int nconds, k, ogte, zerobased;

    nconds = INTEGER(VECTOR_ELT(list, 0))[0];
    k = INTEGER(VECTOR_ELT(list, 1))[0];
    ogte = INTEGER(VECTOR_ELT(list, 2))[0] - 1;
    zerobased = INTEGER(VECTOR_ELT(list, 3))[0];

    int nck = 1;
    for (int i = 1; i <= k; i++) {
        nck *= nconds - (k - i);
        nck /=  i;
    }

    SEXP out;
    out = PROTECT(allocMatrix(INTSXP, k, nck));
    int *p_out = INTEGER(out);

    int found = nck;
    int *valid = NULL;

    if (ogte > 0) {
        valid = (int *) R_Calloc(nck, int);
        found = 0;
    }

    #ifdef _OPENMP
        #pragma omp parallel for schedule(static, 1) reduction(+:found)
    #endif
    for (int task = 0; task < nck; task++) {
        #ifndef _OPENMP
            if (task > 0 && task % 1024 == 0) {
                R_CheckUserInterrupt();
            }
        #endif

        int tempk[k];
        unsigned long long int combination = (unsigned long long int) task;
        int x = 0;

        for (int i = 0; i < k; i++) {
            while (1) {
                unsigned long long int cval = nchoosek(nconds - (x + 1), k - (i + 1));
                if (cval == 0 || cval > combination) {
                    break;
                }
                combination -= cval;
                x++;
            }

            if (x < 0) {
                x = 0;
            }
            if (x >= nconds) {
                x = nconds - 1;
            }

            tempk[i] = x;
            x++;
        }

        Rboolean keep = (ogte <= 0) || (tempk[k - 1] >= ogte);
        if (ogte > 0) {
            valid[task] = keep;
            found += keep;
        }

        for (int i = 0; i < k; i++) {
            p_out[task * k + i] = tempk[i] + 1 - zerobased;
        }
    }

    R_CheckUserInterrupt();

    if (ogte > 0 && found < nck) {
        SEXP copy = PROTECT(duplicate(out));
        int *p_copy = INTEGER(copy);

        out = PROTECT(allocMatrix(INTSXP, k, found));
        p_out = INTEGER(out);

        int col = 0;
        for (int task = 0; task < nck; task++) {
            if (task > 0 && task % 1024 == 0) {
                R_CheckUserInterrupt();
            }
            if (valid[task]) {
                memcpy(&p_out[col * k], &p_copy[task * k], (size_t) k * sizeof(int));
                col++;
            }
        }

        R_Free(valid);
        UNPROTECT(3);
        return(out);
    }

    if (valid) {
        R_Free(valid);
    }

    UNPROTECT(1);
    return(out);

}
