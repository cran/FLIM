\name{flimboot}
\alias{flimboot}
\alias{print.flimboot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{\code{\link{FLIM}} bootstrap function
	%%  ~~function to do ... ~~
}
\description{\code{flimboot} can be used to bootstrap an object of class \code{"flim"}. The function resamples the original data, and reconstructs each sample with \code{\link{flim}}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
flimboot(fo, R, counter = FALSE)
\method{print}{flimboot}(x, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fo}{object of class \code{"flim"}.
%%     ~~Describe \code{x} here~~
}
  \item{R}{number of bootstrap samples.
%%     ~~Describe \code{R} here~~
}
  \item{counter}{logical. If \code{TRUE} displays a bootstrap sample counter. Works by default on Mac and Linux platforms.
%%     ~~Describe \code{counter} here~~
}
\item{x}{object of class \code{"flimboot"}.}
\item{\dots}{not used.}
}
\details{The resampling is performed on the individuals with replacement, so that if an individual is sampled to the bootstrap data, the whole set of longitudinal responses for this individual is included.
%%  ~~ If necessary, more details than the description above ~~
}
\value{\code{flimboot} returns an object of class \code{"flimboot"}.
	
	The function \code{\link{plot.flimboot}} can be used for plotting reconstructed mean responses of the original \code{"flim"} object together with bootstrap confidence bands. Additionally one may calculate the standard errors for the mean response with \code{\link{flimSD}}.
	
	An object of class \code{"flimboot"} contains the following components
	%%  ~Describe the value returned
%%  If it is a LIST, use
\item{org }{the original \code{"flim"} object.}
\item{samples }{a list with the bootstrapped \code{"flim"} objects.}

%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\note{To access the reconstructed bootstrap datasets if needed, type e.g \code{flimbootobject$samples[[1]]$df} for the first sample, \code{flimbootobject$samples[[2]]$df} for the second etc.  

}
\seealso{\code{\link{flim}}, \code{\link{flimSD}}, \code{\link{plot.flimboot}}}

\author{Rune Hoff
%%  ~~who you are~~
}


\keyword{ bootstrap }

