#include <Rinternals.h>
#include "utils.h"

SEXP C_setDimnames(SEXP tt, SEXP dimnames);

SEXP C_setColnames(SEXP matrix, SEXP colnames);

SEXP C_setRownames(SEXP matrix, SEXP rownames);

SEXP _tag(SEXP x);

SEXP _any_tagged(SEXP x);

SEXP _has_tag(SEXP x, SEXP tag_);

SEXP _get_tag(SEXP x);
