\name{flimSD}
\alias{flimSD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Standard deviation for bootstrapped hypothetical mean responses.
%%  ~~function to do ... ~~
}
\description{Calculates the empirical standard deviation of the bootstrapped hypothetical mean responses in a 
	\code{"flimboot"} object.
	%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
flimSD(fbo, response, grouping = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fbo}{  \code{"flimboot"} object from using \code{\link{flimboot}}.
%%     ~~Describe \code{fbo} here~~
}
  \item{response}{a response variable in the model formula.
%%     ~~Describe \code{response} here~~
}
  \item{grouping}{optional group/factor variable used in the model 
%%     ~~Describe \code{grouping} here~~
}
}

\value{Bootstrap standard deviations for the estimated hypothetical mean response.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Rune Hoff
%%  ~~who you are~~
}


\keyword{ standard deviation }% __ONLY ONE__ keyword per line
