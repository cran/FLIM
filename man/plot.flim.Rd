\name{plot.flim}
\alias{plot.flim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot function for flim objects.
%%  ~~function to do ... ~~
}
\description{
Can be used to plot the imputed data. Either one can plot mean responses, or another option is to plot individual trajectories in a spaghetti plot. For plot options for model fits, see \code{\link{flimList}}.  
}
\usage{
\method{plot}{flim}(x, response, grouping = NULL, ylim = NULL, col = NULL, naive = T,
 lty = 1:2, ptype = "mean", ylab = "Response", xlab = "Times", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{object of class \code{"flim"}
%%     ~~Describe \code{x} here~~
}
  \item{response}{name of a longitudinal response
%%     ~~Describe \code{response} here~~
}


  \item{grouping}{optional group/factor variable used in the model.%%     ~~Describe \code{grouping} here~~
}
  \item{ylim}{plot limits for the y-axis. Default is autogenerating suitable limits.
%%     ~~Describe \code{ylim} here~~
}
  \item{col}{Optional colors for each level of the varialbe \code{grouping}.
%%     ~~Describe \code{col} here~~
}
  \item{naive}{logical. If \code{TRUE} the observed mean response is plotted as well.
%%     ~~Describe \code{naive} here~~
}
  \item{lty}{Should be left to default, but may be specified if needed. The default is that observed values are full drawn, and hypothetical values are stipulated.
%%     ~~Describe \code{lty} here~~
}
  \item{ptype}{specifies the plot type, options are "mean" which is default, or "spa" for spaghetti plots of all individual trajectories.
%%     ~~Describe \code{ptype} here~~
}
  \item{ylab}{label of y-axis
%%     ~~Describe \code{ylab} here~~
}
  \item{xlab}{label of x-axis
%%     ~~Describe \code{xlab} here~~
}
  \item{\dots}{additional arguments passed to \code{plot}.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
	\code{plot.flim} is used to either plot mean trajectories for longitudinal responses or individual trajectories. Users need to specify which response variable should be plotted with the \code{response} argument. The mean response may be split into several trajectories (with separate coloring) corresponding to the different levels of a factor variable, by specifying the \code{grouping} argument. Both \code{response} and \code{grouping} should be entered with quotation marks, e.g \code{grouping="treatment"}. The \code{col} argument can be used to determine the colors of plots for different levels of the grouping variable, e.g \code{col=c("red", "blue")} for a dichotomous variable, but may also be used without any grouping to specify the color of the plot. Any other arguments that can be passed to \code{\link{plot}} can be entered. See example on usage at the bottom of \code{\link{flim}} help file.
	}

\value{Response measurement plots for reconstructed and observed data.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Rune hoff
%%  ~~who you are~~
}
%\examples{## See example in \code{\link{flim}}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plotting }