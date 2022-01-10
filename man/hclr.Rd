\name{hclr}
\alias{hclr}

\title{Colors from the HCL spectrum}

\description{
  Produces colors from the HCL (Hue Chroma Luminance) spectrum, based on the number of levels
  from a factor.
}

\usage{
hclr(x, starth = 25, c = 50, l = 75, alpha = 1, fixup = TRUE)
}

\arguments{
  \item{x}{Number of factor levels, or the factor itself, or a frequency distribution
    from a factor}
  \item{starth}{Starting point for the hue (in the interval 0 - 360)}
  \item{c}{chroma - color purity, small values produce dark and high values produce
    bright colors}
  \item{l}{color luminance - a number between 0 and 100}
  \item{alpha}{color transparency, where 0 is a completely transparent color, up to 1}
  \item{fixup}{logical, corrects the RGB values foto produce a realistic color}
}

\value{
The RBG code for the corresponding HCL colors.
}

\details{
Any value of \code{h} outside the interval 0 - 360 is constrained to this interval using
modulo values. For instance, 410 is constrained to 50 = 410%%360.
}

\author{Adrian Dusa}


\examples{

aa <- sample(letters[1:5], 100, replace = TRUE)

hclr(aa)

# same with
hclr(5)

# or
hclr(table(aa))
}

\keyword{misc}
