\name{.rda functions: listRDA, objRDA}

\alias{listRDA}
\alias{objRDA}

\title{Load and list objects from an .rda file}

\description{
Utility functions to read the names and load the objects from an .rda file, into
an R list.
}

\usage{
listRDA(.filename)

objRDA(.filename)
}

\arguments{
  \item{.filename}{The path to the file where the R object is saved.}  
}

\details{
Files with the extension .rda are routinely created using the base function
\bold{\code{\link[base]{save}()}}.

The function \bold{\code{listRDA()}} loads the object(s) from the .rda file into a list,
preserving the object names in the list components.

The .rda file can naturally be loaded with the base \bold{\code{\link[base]{load}()}} function,
but in doing so the containing objects will overwrite any existing objects with the same names.

The function \bold{\code{objRDA()}} returns the names of the objects from the .rda file.
}

\value{
A list, containing the objects from the loaded .rda file.
}

\author{
Adrian Dusa
}


\keyword{functions}
