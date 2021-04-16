\name{tryCatchWEM}

\alias{tryCatchWEM}

\title{Try functions to capture warnings, errors and messages.}

\description{
This function combines the base functions \bold{\code{tryCatch}()} and
\bold{\code{withCallingHandlers}()} for the specific purpose of capturing
not only  errors and warnings but messages as well.
}

\usage{
tryCatchWEM(expr, capture = FALSE)
}

\arguments{
    \item{expr}{Expression to be evaluated.}
    \item{capture}{Logical, capture the visible output.}
}

\details{
In some situations it might be important not only to test a function, but also
to capture everything that is written in the R console, be it an error, a warning
or simply a message.

For instance package \bold{\pkg{QCA}} (version 3.4) has a Graphical User Interface
that simulates an R console embedded into a web based \bold{\pkg{shiny}} app.

It is not intended to replace function \bold{\code{tryCatch}()} in any
way, especially not evaluating an expression before returning or exiting, it simply
captures everything that is printed on the console (the visible output).
}


\value{
A list, if anything would be printed on the screen, or an empty (NULL) object
otherwise.
}

\author{
Adrian Dusa
}

\keyword{functions}
