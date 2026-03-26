#' Colors from the HCL spectrum
#'
#' Produces colors from the HCL (Hue Chroma Luminance) spectrum, based on the number of levels
#'   from a factor.
#'
#' @name hclr
#' @rdname hclr
#' @rawRd
#' \usage{
#' hclr(x, starth = 25, c = 50, l = 75, alpha = 1, fixup = TRUE)
#' }
#'
#' \arguments{
#'   \item{x}{Number of factor levels, or the factor itself, or a frequency distribution
#'     from a factor}
#'   \item{starth}{Starting point for the hue (in the interval 0 - 360)}
#'   \item{c}{chroma - color purity, small values produce dark and high values produce
#'     bright colors}
#'   \item{l}{color luminance - a number between 0 and 100}
#'   \item{alpha}{color transparency, where 0 is a completely transparent color, up to 1}
#'   \item{fixup}{logical, corrects the RGB values foto produce a realistic color}
#' }
#'
#' \value{
#' The RBG code for the corresponding HCL colors.
#' }
#'
#' \details{
#' Any value of \code{h} outside the interval 0 - 360 is constrained to this interval using
#' modulo values. For instance, 410 is constrained to 50 = 410%%360.
#' }
#'
#' \author{Adrian Dusa}
#'
#'
#' \examples{
#'
#' aa <- sample(letters[1:5], 100, replace = TRUE)
#'
#' hclr(aa)
#'
#' # same with
#' hclr(5)
#'
#' # or
#' hclr(table(aa))
#' }
#'
#' \keyword{misc}
NULL
#' @export
`hclr` <-
function(x, starth = 25, c = 50, l = 75, alpha = 1, fixup = TRUE) {
    if (length(x) > 1) {
        x <- length(table(x))
    }
    return(
        hcl(
            h = seq(starth, starth + 360, length = x + 1)%%360,
            c = c,
            l = l,
            alpha = alpha,
            fixup = fixup
        )[1:x]
    )
}
