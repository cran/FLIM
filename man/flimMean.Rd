\name{flimMean}
\alias{flimMean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mean response function for \code{FLIM}
%%  ~~function to do ... ~~
}
\description{Calculates mean responses for \code{"flim"} objects.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
flimMean(flimobject, response, grouping = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flimobject}{object of class \code{"flim"}.
%%     ~~Describe \code{flimobject} here~~
}
  \item{response}{name of one of the responses in the model formula
%%     ~~Describe \code{response} here~~
}
  \item{grouping}{optional group/factor variable used in the model.
%%     ~~Describe \code{grouping} here~~
}
}
\details{\code{flimMean} takes a fitted \code{"flim"} object and calculates the mean response at the observed timepoints.
If some group variable was used as predictor in the model, mean responses can be calculated restricted to different levels of this variable, by specifying the \code{"grouping"} argument. See example in \code{\link{flim}} for usage.
%%  ~~ If necessary, more details than the description above ~~
}
\value{Mean responses for \code{"flim"} objects.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Rune Hoff
%%  ~~who you are~~
}

