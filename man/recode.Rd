\name{recode}

\alias{recode}

\title{Recode a variable}

\description{
Recodes a vector (numeric, character or factor) according to a set of rules.
Similar to the \bold{\code{\link[car]{recode}()}} function in package \bold{\pkg{car}},
but more flexible. It is also similar to function \bold{\code{\link[base]{findInterval}()}}
from package \bold{\pkg{base}}.
}

\usage{
recode(x, rules, cuts, values, ...)
}

\arguments{
    \item{x}{A vector of mode numeric, character or factor.}
    \item{rules}{Character string or a vector of character strings
                 for recoding specifications.}
    \item{cuts}{A vector of one or more unique cut points.}
    \item{values}{A vector of output values.}
    \item{...}{Other parameters, for compatibility with other functions such as
               \bold{\code{\link[car]{recode}()}} in package \bold{\pkg{car}} but also
         \bold{\code{\link[base]{factor}()}} in package \bold{\pkg{base}}}
}

\details{
Similar to the \bold{\code{recode()}} function in package \bold{\pkg{car}}, the
recoding rules are separated by semicolons, of the form \bold{\code{input = output}},
and allow for:


\tabular{rl}{
    a single value \tab \bold{\code{1 = 0}}\cr
    a range of values \tab \bold{\code{2:5 = 1}}\cr
    a set of values \tab \bold{\code{c(6,7,10) = 2}}\cr
    \bold{\code{else}} \tab everything that is not covered by the previously specified rules
}

Contrary to the \bold{\code{\link[car]{recode}()}} function in package \bold{\pkg{car}}, this
function allows the \bold{\code{:}} sequence operator (even for factors), so that a rule
such as \bold{\code{c(1,3,5:7)}}, or \bold{\code{c(a,d,f:h)}} would be valid.

Actually, since all rules are specified in a string, it really doesn't matter
if the \bold{\code{c()}} function is used or not. For compatibility reasons it accepts it, but
a more simple way to specify a set of rules is \bold{\code{"1,3,5:7=A; else=B"}}

Special values \bold{\code{lo}} and \bold{\code{hi}} may also appear in the range of values,
while \bold{\code{else}} can be used with \bold{\code{else=copy}} to copy all values which
were not specified in the recoding rules.

In the package \pkg{car}, a character \bold{\code{output}} would have to be quoted,
like \bold{\code{"1:2='A'"}} but that is not mandatory in this function, \bold{\code{"1:2=A"}}
would do just as well. Output values such as \bold{\code{"NA"}} or \bold{\code{"missing"}}
are converted to \bold{\code{NA}}.

Another difference from the \bold{\pkg{car}} package: the output is \bold{not} automatically
converted to a factor even if the original variable is a factor. That option is left to the
user's decision to specify \bold{\code{as.factor.result}}, defaulted to \bold{\code{FALSE}}.

A capital difference is the treatment of the values not present in the recoding rules. By
default, package \bold{\pkg{car}} copies all those values in the new object, whereas in this
package the default values are \bold{\code{NA}} and new values are added only if they are
found in the rules. Users can choose to copy all other values not present in the recoding
rules, by specifically adding \bold{\code{else=copy}} in the rules.

Since the two functions have the same name, it is possible that users loading both
packages to use one instead of the other (depending which package is loaded first).
In order to preserve functionality and minimize possible namespace collisions with package
\bold{\pkg{car}}, special efforts have been invested to ensure perfect compatibility with
the other \bold{\code{\link[car]{recode}()}} function (plus more).

The argument \bold{\code{...}} allows for more arguments specific to the \pkg{car} package,
such as \bold{\code{as.factor.result}}, \bold{\code{as.numeric.result}} and \bold{\code{levels}}.
In addition, it also accepts \bold{\code{labels}} and \bold{\code{ordered}} specific to function
\bold{\code{\link[base]{factor}()}} in package \bold{\pkg{base}}.

Blank spaces outside category labels are ignored, see the last example.

It is possible to use \bold{\code{recode()}} in a similar way to function \bold{\code{cut()}},
by specifying a vector of cuts which work for both numeric and character/factor objects. 
For any number of \bold{\code{c}} cuts, there should be \bold{\code{c + 1}} values, and if
not otherwise specified the argument \bold{\code{values}} is automatically constructed as a
sequence of numbers from \bold{\code{1}} to \bold{\code{c + 1}}.

Unlike the function \bold{\code{cut()}}, arguments such as \bold{\code{include.lowest}} or
\bold{\code{right}} are not necessary because the final outcome can be changed by tweaking
the cut values.
}

\author{
Adrian Dusa
}

\examples{
x <- rep(1:3, 3)
x
#  [1] 1 2 3 1 2 3 1 2 3

recode(x, "1:2 = A; else = B")
#  [1] "A" "A" "B" "A" "A" "B" "A" "A" "B"

recode(x, "1:2 = 0; else = copy")
#  [1] 0 0 3 0 0 3 0 0 3

set.seed(1234)
x <- factor(sample(letters[1:10], 20, replace = TRUE),
          levels = letters[1:10])
x
#  [1] b g g g i g a c g f g f c j c i c c b c
# Levels: a b c d e f g h i j

recode(x, "b:d = 1; g:hi = 2; else = NA") # note the "hi" special value
#  [1]  1  2  2  2  2  2 NA  1  2 NA  2 NA  1  2  1  2  1  1  1  1

recode(x, "a, c:f = A; g:hi = B; else = C", as.factor.result = TRUE)
#  [1] C B B B B B A A B A B A A B A B A A C A
# Levels: A B C

recode(x, "a, c:f = 1; g:hi = 2; else = 3", as.factor.result = TRUE,
       labels = c("one", "two", "three"), ordered = TRUE)
#  [1] three two   two   two   two   two   one   one   two   one  
# [11] two   one   one   two   one   two   one   one   three one  
# Levels: one < two < three  

set.seed(1234)
categories <- c("An", "example", "that has", "spaces")
x <- factor(sample(categories, 20, replace = TRUE),
            levels = categories, ordered = TRUE)
sort(x)
#  [1] An       An       An       An       An       example 
#  [7] example  example  example  that has that has that has
# [13] that has that has that has that has that has spaces  
# [19] spaces   spaces  
# Levels: An < example < that has < spaces

recode(sort(x), "An : that has = 1; spaces = 2")
#  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2

# single quotes work, but are not necessary
recode(sort(x), "An : 'that has' = 1; spaces = 2")
#  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2

# same using cut values
recode(sort(x), cuts = "that has")
#  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2

# modifying the output values
recode(sort(x), cuts = "that has", values = 0:1)
#  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1

# more treatment of "else" values
x <- 10:20

# recoding rules don't overlap all existing values, the rest are empty
recode(x, "8:15=1")

# all other values are copied
recode(x, "8:15=1; else=copy")
}

\keyword{functions}
