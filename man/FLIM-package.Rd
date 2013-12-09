\name{FLIM-package}
\alias{FLIM-package}
\alias{FLIM}
\docType{package}
\title{
Farewell's Linear Increments Model
}
\description{
The \pkg{FLIM} package can be used to fit linear models for the observed increments of longitudinal processes found in longitudinal datasets. In addition, it can reconstruct any missing data by imputing estimated values according to the fitted models. The mean structure of the reconstructed data will be correct under certain assumptions. However, imputed data will not exhibit the random variation it should, and pointwise confidence bands can be based on bootstrap samples.
}
\details{
\tabular{ll}{
Package: \tab FLIM\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2013-12-09\cr
License: \tab GPL-2\cr
}
The model fits and reconstruction is done with the core function \code{\link{flim}}, and mean responses can be calculated with \code{\link{flimMean}}. To investigate results graphically one can use \code{\link{plot.flim}}. 

To assess model fits at each observation time after having used \code{\link{flim}}, see \code{\link{flimList}}.

A bootstrap function \code{\link{flimboot}} for \code{"flim"} objects is implemented as well.
 }
\author{
Rune Hoff with contributions from Jon Michael Gran and Daniel Farewell


Maintainer: Rune Hoff <rune.hoff@medisin.uio.no>
}
\references{
D. M. Farewell. Linear models for censored data. PhD thesis, Lancaster University, 2006.


P. Diggle, D. M. Farewell, and R. Henderson. Analysis of longitudinal data with drop-out; objectives, assumptions and a proposal. Journal of the Royal Statistical Society, 2007.


O. O. Aalen and N. Gunnes. A dynamic approach for reconstructing missing longitudinal data using the linear increments model. Journal of Biostatistics, 2010.

}

\keyword{ package }
