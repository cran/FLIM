\name{plot.flimboot}
\alias{plot.flimboot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot function for \code{"flimboot"} objects.
%%  ~~function to do ... ~~
}
\description{Plots hypothetical mean responses for a longitudinal dataset reconstructed with \code{\link{flim}}, together with point wise bootstrapped confidence bands.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
%plot.flimboot(x, response, grouping = NULL, col = 1:20, ...)
\method{plot}{flimboot}(x, response, grouping=NULL, col=1:20, ylab="Response",
                          xlab="Times", ylim=NULL, main=NULL, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{\code{"flimboot"} object from using \code{\link{flimboot}}.
%%     ~~Describe \code{x} here~~
}
  \item{response}{a response variable in the model formula.
%%     ~~Describe \code{response} here~~
}
  \item{grouping}{optional group/factor variable in the model formula.
%%     ~~Describe \code{grouping} here~~
}
  \item{col}{optional colors for each level of the group variable.
  	%%     ~~Describe \code{col} here~~
}
\item{ylab}{title for the y axis.}
\item{xlab}{title for the x axis.}
\item{ylim}{range limit for the y axis; if nothing is specified it will auto adjust.}
\item{main}{an overall title for the plot}
  \item{\dots}{additional arguments passed to \code{plot}, except \code{lty} which should be left unspecified.

%%     ~~Describe \code{\dots} here~~
}
}
\details{Note that here the hypothetical mean responses are plotted with full-drawn lines, and confidence bands are in the same color but stipulated. The observed mean responses are not plotted. The confidence bands are calculated as hypothetical mean response +- two times the bootstrap standard deviation at each time point. The standard error estimates are available with \code{\link{flimSD}}, see \code{\link{flim}} for example on usage.
%%  ~~ If necessary, more details than the description above ~~
}
\value{Mean response plot with point wise bootstrap confidence bands.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Rune Hoff
%%  ~~who you are~~
}


\keyword{ bootstrap }

