\name{flimList}
\alias{flimList}
\alias{print.flimList}
\alias{summary.flimList}
\alias{plot.flimList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Assess the fitted models for observed increments
%%  ~~function to do ... ~~
}
\description{Formats and prints the linear fits for observed increments.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
flimList(fo)
\method{print}{flimList}(x, ...)
\method{summary}{flimList}(object, ...)
\method{plot}{flimList}(x, response, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fo}{an object of class \code{"flim"}}
    	  \item{x}{an object of class \code{"flimList"}}
  	  	  \item{object}{an object of class \code{"flimList"}}
  	  	  	\item{response}{name of a response variable in the model formula. Plot diagnostics for the linear model for this response will be shown at the different timepoints starting on the first, and jumps to next when <ENTER> is pressed.}
  	  	  	\item{\dots}{not used.}
%%     ~~Describe \code{x} here~~
}


\value{Returns an object of class \code{"flimList"} which has corresponding \code{print}, \code{summary} and \code{plot} methods.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
