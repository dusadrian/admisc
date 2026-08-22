#' @keywords internal
"_PACKAGE"
#' @name admisc_package
#'
#' @title Adrian Dusa's Miscellaneous
#'
#' @description Contains functions used across packages 'DDIwR', 'QCA' and 'venn'.
#' Interprets and translates, factorizes and negates SOP - Sum of Products
#' expressions, for both binary and multi-value crisp sets, and extracts
#' information (set names, set values) from those expressions. Other functions
#' perform various checks if possibly numeric (even if all numbers reside in a
#' character vector) and coerce to numeric, or check if the numbers are whole. It
#' also offers, among many others, a highly versatile recoding routine and some
#' more flexible alternatives to the base functions `with()` and `within()`.
#' SOP simplification functions in this package use related minimization from
#' package **QCA**, which is recommended to be installed despite not being listed
#' in the Imports field, due to circular dependency issues.
#'
#' @author Adrian Dusa
#'
#' Maintainer: Adrian Dusa (dusa.adrian@unibuc.ro)
#'
#' @details
#' \tabular{ll}{
#'   Package: \tab admisc\cr
#'   Type: \tab Package\cr
#'   Version: \tab 0.41\cr
#'   Date: \tab 2026-08-22\cr
#'   License: \tab GPL (>= 3)\cr
#' }
#'
#' @importFrom utils read.csv write.csv write.table capture.output installed.packages packageDescription compareVersion remove.packages tail
#' @importFrom stats na.omit dist relevel
#' @importFrom methods is
#' @importFrom grDevices hcl
#' @useDynLib admisc, .registration = TRUE
NULL
