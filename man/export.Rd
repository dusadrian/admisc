\name{export}

\alias{export}

\title{Export an object to a file or a connection}

\description{
This is a generic function, usually a wrapper to \bold{\code{\link[utils]{write.table}()}}.
}

\usage{
export(what, ...)
}

\arguments{
    \item{what}{The object to be written (matrix or dataframe)}
    \item{...}{Specific arguments to class functions.}
}

\details{
The default convention for \bold{\code{\link[utils]{write.table}()}} is to add a blank column
name for the row names, but (despite it is a standard used for CSV files) that doesn't work
with all spreadsheets or other programs that attempt to import the result of
\bold{\code{\link[utils]{write.table}()}}.

This function acts as if \bold{\code{\link[utils]{write.table}()}} was called, with only one
difference: if row names are present in the dataframe (i.e. any of them should be different
from the default row numbers), the final result will display a new column called
\bold{\code{cases}} in the first position, except the situation that another column called
\bold{\code{cases}} already exists in the data, when the row names will be completely ignored.

If not otherwise specified, an argument \bold{\code{sep = ","}} is added by default.

The argument \bold{\code{row.names}} is always set to FALSE, a new column being added anyways (if possible).

Since this function pipes everything to \bold{\code{\link[utils]{write.table}()}}, the argument \bold{\code{file}}
can also be a connection open for writing, and \bold{\code{""}} indicates output to the console.
}

\author{
Adrian Dusa
}


\seealso{
  The \dQuote{R Data Import/Export} manual.

  \code{\link[utils]{write.table}}
}

\keyword{functions}
