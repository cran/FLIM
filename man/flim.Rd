\name{flim}
\alias{flim}
\alias{print.flim}
\alias{summary.flim}
\title{Farewell's Linear Increments Model}
%- Also NEED an '\alias' for EACH other topic documented here.
\description{\code{flim} is the main function in \code{\link{FLIM}} and is used to fit linear models for the observed increments and fill in missing values in a longitudinal dataset according to the linear increments model. The end result is a list of fitted models and a hypothetical complete dataset that can be used for further analysis.
}
\usage{
flim(formula, data, id, obstime, t.values = NULL, method = "locf", lambda = NULL,
art.cens = NULL)

\method{print}{flim}(x, ...)
\method{summary}{flim}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{an R formula on the form \code{response ~ predictors}. The package fits models for the increments, so that \code{formula = Y ~ Y + X}, specifies the model 
  	
  	E(Y(t+1) - Y(t)) = beta0(t) + beta1(t)*Y(t) + beta2(t)*X(t). 
  	
  	For several responses and the same set of predictors,
  	
  	 \code{cbind(response1, response2) ~ predictors} can be used. For full flexibility a list of formulae can be supplied. 
%%     ~~Describe \code{formula} here~~
}
  \item{data}{a longitudinal dataset in long format. See e.g \code{\link{panss}}. Only rows corresponding actual observations should be contained in the dataset. Partial observation of some, of pontentially many measurement variables, should also be contained as a row in the data, but with the non observed values coded as \code{NA}. Fully non observed entries should be excluded.
  	
  	The dataset needs to include a variable with id numbers, which uniquely identifies the individual subjects under study, and in addition a variable with the timepoints of the observations.
  	%%     ~~Describe \code{data} here~~
}
  \item{id}{name of the column in \code{data} which uniquely identifies the subjects under study.
%%     ~~Describe \code{id} here~~
}
  \item{obstime}{name of the column with observation times
%%     ~~Describe \code{obstime} here~~
}
  \item{t.values}{timepoints at which models should be fitted and missing data imputed. Because of the nature of the algorithm, models are fitted from the first timepoint all the way through to the second to last, and missing data are imputed from the second timepoint to the last. If nothing is specified, this argument will be set to the observation times contained in the data.
%%     ~~Describe \code{t.values} here~~
}
  \item{method}{method for filling in values between observations if there is nonmonotone missingness, options are "locf", "approx", "recursive" and "recursiveX". Default is "locf". See details below.
%%     ~~Describe \code{method} here~~
}
  \item{lambda}{ridge parameter for doing ridge regression in the linear fits, default is OLS.
%%     ~~Describe \code{lambda} here~~
}
  \item{art.cens}{used to specify a 0-1 factor variable, for which artificial censoring is performed to response values for all subjects after the first switch from 0 to 1. Missing values will then be imputed as if subjects never made the switch. This is intended for advanced users and does not come with readily available tools for investigating results. Original dataset with added columns for counterfactual response values is stored in fitted object.
%%     ~~Describe \code{art.cens} here~~
}
\item{x}{object of class \code{"flim"}}
\item{object}{object of class \code{"flim"}}
  \item{\dots}{ not used.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
\code{flim} works by fitting linear models for the increments. At each observation time t, observed increments are regressed onto values of the longitudinal responses and possibly other covariates. Missing values are estimated according to the models and imputed.
	
	To use \code{flim}, a longitudinal dataset in long/stacked format is required, see e.g \code{\link{panss}}. In addition to longitudinal variable(s) and possibly covariates, the data must contain a variable with ids that uniquely indentifies study subjects, and also a time variable of the time of observation. In the case where there is only one longitudinal measurement being recorded, missing observations should not be included a row in the data. If there are more than one response, row entries where all responses are missing should be excluded. If some of the responses are observed for an individual, this row should enter the data and the missing measurement(s) should be coded as \code{NA}.
	
	 \code{method} determines how nonmonotone missingness should be handled. \code{"locf"} stands for last observation carried forward, \code{"approx"} means that missing values are filled in by linear interpolation between two observed values, and \code{"recursive"} results in the linear increments model being used throughout. The "recursiveX" option is the same as "recursive", but also utilizes observed data when there are no observed increment (observations with missing data on the previous timepoint), by using the last imputed value to estimate an increment whenever someone returns to the study.}
\value{
\code{flim} returns an object of class \code{"flim"}.
	
	Taking \code{summary} of a \code{"flim"} object will give some information about the reconstruction and fitted models. Hypothetical mean responses can be calculated with \code{\link{flimMean}}. Some graphical options for investigating the imputed data are available in \code{\link{plot.flim}}. To assess the fitted models for increments use \code{\link{flimList}}. For standard error estimates of the mean response, see \code{\link{flimboot}} and \code{\link{flimSD}}
	
	
	
	 An object of class \code{"flim"} contains the following components of interest for users
	\item{dataset}{the reconstructed dataset}
	\item{fit}{a list with the fitted models}	
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\references{
See \code{\link{FLIM}}
}
\author{Rune Hoff
%%  ~~who you are~~
}

\examples{
library(FLIM)
data(panss)
panss.flim <- flim(Y~Y+factor(treat), panss, "id", "time",
                   t.values=c(0,1,2,4,6,8))
plot(panss.flim, "Y", "treat") 
flimMean(panss.flim, "Y", "treat")
flimList(panss.flim)
summary(flimList(panss.flim))
#plot(flimList(panss.flim),"Y")
panss.boot <- flimboot(panss.flim, 20)
flimSD(panss.boot,"Y","treat")
plot(panss.boot, "Y", "treat")

# Imputing counterfactual CD4 values as if treatment never started
data(CD4sim)
CD4.flim <- flim(cd4 ~ cd4, id="id", obstime="time", data=CD4sim,
             art.cens="treat")
# CD4.flim$dataset has added column for counterfactual values             
}

