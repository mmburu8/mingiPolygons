\name{mingiPolygons}
\alias{mingiPolygons}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Function for Identifying Polygon Objects In A Map In Shiny
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This package is able to select any polgyon object(country, county or state)
from any map visualization formed by ggplot or any other package extension deployed in Shiny.
It is an extension of nearPoints() but specifically for maps.
It takes in original data used to form map in ggplot, data collected by nearPoints function
and input$plot_click value
}
\usage{
mingiPolygons(data_main, data_click, click_vector, col_spec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_main}{original data used to form map in ggplot
%%     ~~Describe \code{data_main} here~~
}
  \item{data_click}{data collected by nearPoints function
%%     ~~Describe \code{data_click} here~~
}
  \item{click_vector}{input$plot_click value
%%     ~~Describe \code{click_vector} here~~
}
  \item{col_spec}{column containing names of polygon objects
%%     ~~Describe \code{col_spec} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Name of the polygon object
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Martin Mburu
Ted Muthomi
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
state <- mingiPolygons(final_stats, sep_df, unknown, "county")
print(state)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
