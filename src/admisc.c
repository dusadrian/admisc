#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>
#include <R_ext/Rdynload.h>


typedef union {
    double value;
    char byte[16];
} ieee_double;



#ifdef WORDS_BIGENDIAN
// First two bytes are sign & exponent
// Last four bytes (that is, 32 bits) are 1954
const int TAG_BYTE = 3;
#else
const int TAG_BYTE = 4;
#endif



static R_INLINE Rboolean hasDimnames(SEXP matrix) {

    return !Rf_isNull(getAttrib(matrix, R_DimNamesSymbol));

}

static R_INLINE Rboolean hasColnames(SEXP matrix) {

    return hasDimnames(matrix) ? !Rf_isNull(VECTOR_ELT(getAttrib(matrix, R_DimNamesSymbol), 1)) : FALSE;

}

static R_INLINE Rboolean hasRownames(SEXP matrix) {

    return hasDimnames(matrix) ? !Rf_isNull(VECTOR_ELT(getAttrib(matrix, R_DimNamesSymbol), 0)) : FALSE;

}

SEXP C_setDimnames(SEXP tt, SEXP dimnames) {
    setAttrib(tt, R_DimNamesSymbol, dimnames);
    return(R_NilValue);
}

SEXP C_setColnames(SEXP matrix, SEXP colnames) {
    SEXP dimnames = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(dimnames, 1, colnames);

    if (hasRownames(matrix)) {
        SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(getAttrib(matrix, R_DimNamesSymbol), 0));
    }

    setAttrib(matrix, R_DimNamesSymbol, dimnames);
    UNPROTECT(1);
    return(R_NilValue);
}

SEXP C_setRownames(SEXP matrix, SEXP rownames) {
    SEXP dimnames = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(dimnames, 0, rownames);

    if (hasColnames(matrix)) {
        SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(getAttrib(matrix, R_DimNamesSymbol), 1));
    }

    setAttrib(matrix, R_DimNamesSymbol, dimnames);
    UNPROTECT(1);
    return(R_NilValue);
}

SEXP _tag(SEXP x) {
    int n = Rf_length(x);
    SEXP out = PROTECT(Rf_allocVector(REALSXP, n));

    for (int i = 0; i < n; ++i) {

        int nchars = Rf_length(STRING_ELT(x, i));
        Rboolean firstminus = CHAR(STRING_ELT(x, i))[0] == CHAR(mkChar("-"))[0];

        if (nchars > 2 + firstminus) {
            nchars = 2 + firstminus;
        }

        ieee_double y;
        y.value = NA_REAL;

        if (firstminus) {
            y.value = -1 * NA_REAL;
        }

        int bytepos = TAG_BYTE;

        for (int c = firstminus; c < nchars; c++) {
            y.byte[bytepos] = CHAR(STRING_ELT(x, i))[c];
            if (TAG_BYTE == 3) {
                bytepos -= 1;
            }
            else {
                bytepos += 1;
            }
        }

        REAL(out)[i] = y.value;
    }

    UNPROTECT(1);
    return(out);
}

SEXP _any_tagged(SEXP x) {
    int n = Rf_length(x);
    SEXP out = PROTECT(Rf_allocVector(LGLSXP, 1));
    LOGICAL(out)[0] = 0;

    int i = 0;

    while (!LOGICAL(out)[0] && i < n) {
        if (TYPEOF(x) == REALSXP) {
            double xi = REAL(x)[i];
            if (isnan(xi)) {
                ieee_double y;
                y.value = xi;

                Rboolean firstminus = signbit(xi);

                char test[16 + 8 * firstminus];
                if (firstminus) {
                    test[0] = CHAR(mkChar("-"))[0];
                }

                test[firstminus] = y.byte[TAG_BYTE];
                LOGICAL(out)[0] = test[0] != '\0';
            }
        }
        i += 1;
    }

    UNPROTECT(1);
    return out;
}

SEXP _has_tag(SEXP x, SEXP tag_) {
    int n = Rf_length(x);
    SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));

    if (TYPEOF(x) != REALSXP) {
        for (int i = 0; i < n; ++i) {
            LOGICAL(out)[i] = 0;
        }
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

                Rboolean test = true;

                if (tag == '\0') {
                    LOGICAL(out)[i] = false;
                }
                else {
                    if (TYPEOF(tag_) != NILSXP) {

                        int nchars = Rf_length(STRING_ELT(tag_, 0));
                        Rboolean firstminus = CHAR(STRING_ELT(tag_, 0))[0] == CHAR(mkChar("-"))[0];

                        if ((firstminus && !signbit(xi)) || (!firstminus && signbit(xi))) {
                            LOGICAL(out)[i] = false;
                        }
                        else {

                            if (nchars > 2 + firstminus) {
                                nchars = 2 + firstminus;
                            }

                            test = test && tag == CHAR(STRING_ELT(tag_, 0))[firstminus];
                            char tag = y.byte[(TAG_BYTE == 4) ? 5 : 2];

                            if (Rf_length(STRING_ELT(tag_, 0)) > 1 && tag != '\0') {
                                test = test && tag == CHAR(STRING_ELT(tag_, 0))[firstminus + 1];
                            }

                            LOGICAL(out)[i] = test;
                        }
                    }
                    else {
                        LOGICAL(out)[i] = true;
                    }
                }
            }
        }
    }

    UNPROTECT(1);
    return out;
}

SEXP _get_tag(SEXP x) {

    int n = Rf_length(x);
    SEXP out = PROTECT(Rf_allocVector(STRSXP, n));

    for (int i = 0; i < n; ++i) {
        double xi = REAL(x)[i];

        if (!isnan(xi)) {
            SET_STRING_ELT(out, i, NA_STRING);
        }
        else {

            ieee_double y;
            y.value = xi;

            Rboolean firstminus = signbit(xi);

            char test[16 + 8 * firstminus];
            if (firstminus) {
                test[0] = CHAR(mkChar("-"))[0];
            }

            test[firstminus] = y.byte[TAG_BYTE];

            if (test[0] == '\0') {
                SET_STRING_ELT(out, i, NA_STRING);
            }
            else {
                char tag2 = y.byte[(TAG_BYTE == 4) ? 5 : 2];
                int nchars = 1 + (strlen(&tag2) > 0) + firstminus;

                test[firstminus + 1] = tag2;
                SET_STRING_ELT(out, i, Rf_mkCharLenCE(test, nchars, CE_UTF8));
            }
        }
    }


    UNPROTECT(1);
    return out;
}
