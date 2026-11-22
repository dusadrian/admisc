#include <R.h>
#include <stdbool.h>
#include <R_ext/Rdynload.h>
#include "admisc.h"

typedef union {
    double value;
    unsigned char byte[sizeof(double)];
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
    R_xlen_t n = XLENGTH(x);
    SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
    for (R_xlen_t i = 0; i < n; ++i) {
        SEXP value = STRING_ELT(x, i);
        ieee_double y;
        y.value = NA_REAL;
        if (value != NA_STRING) {
            const unsigned char *tag = (const unsigned char *)CHAR(value);
            if (*tag == '-') {
                ++tag;
                y.value = copysign(NA_REAL, -1.0);
            }
            if (*tag) {
                y.byte[TAG_BYTE] = tag[0];
                y.byte[TAG_BYTE == 4 ? 5 : 2] = tag[1];
            }
        }
        REAL(out)[i] = y.value;
    }
    UNPROTECT(1);
    return out;
}


SEXP _has_tag(SEXP x, SEXP tag_) {
    R_xlen_t n = XLENGTH(x);
    SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
    if (TYPEOF(x) != REALSXP) {
        Memzero(LOGICAL(out), n);
        UNPROTECT(1);
        return out;
    }
    if (tag_ != R_NilValue && (TYPEOF(tag_) != STRSXP || XLENGTH(tag_) != 1)) {
        Rf_error("Expected one tag.");
    }
    const unsigned char *tag = tag_ == R_NilValue ? NULL :
        (const unsigned char *)CHAR(STRING_ELT(tag_, 0));
    int negative = tag && *tag == '-';
    if (negative) ++tag;

    for (R_xlen_t i = 0; i < n; ++i) {
        ieee_double y;
        y.value = REAL(x)[i];
        int tagged = ISNAN(y.value) && y.byte[TAG_BYTE] != 0;
        if (tagged && tag) {
            tagged = (!!signbit(y.value) == negative &&
                      y.byte[TAG_BYTE] == tag[0] &&
                      y.byte[TAG_BYTE == 4 ? 5 : 2] == tag[1]);
        }
        LOGICAL(out)[i] = tagged;
    }
    UNPROTECT(1);
    return out;
}


SEXP _get_tag(SEXP x) {
    R_xlen_t n = XLENGTH(x);
    SEXP out = PROTECT(Rf_allocVector(STRSXP, n));
    for (R_xlen_t i = 0; i < n; ++i) {
        ieee_double y;
        y.value = REAL(x)[i];
        if (!ISNAN(y.value) || !y.byte[TAG_BYTE]) {
            SET_STRING_ELT(out, i, NA_STRING);
        }
        else {
            char tag[3];
            int length = 0;
            if (signbit(y.value)) tag[length++] = '-';
            tag[length++] = y.byte[TAG_BYTE];
            char second = y.byte[TAG_BYTE == 4 ? 5 : 2];
            if (second) tag[length++] = second;
            SET_STRING_ELT(out, i, Rf_mkCharLenCE(tag, length, CE_UTF8));
        }
    }
    UNPROTECT(1);
    return out;
}


SEXP _any_tagged(SEXP x) {
    if (TYPEOF(x) != REALSXP) return Rf_ScalarLogical(FALSE);
    R_xlen_t n = XLENGTH(x);
    for (R_xlen_t i = 0; i < n; ++i) {
        ieee_double y;
        y.value = REAL(x)[i];
        if (ISNAN(y.value) && y.byte[TAG_BYTE]) return Rf_ScalarLogical(TRUE);
    }
    return Rf_ScalarLogical(FALSE);
}
