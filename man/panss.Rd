\name{panss}
\alias{panss}
\docType{data}
\title{PANSS data
%%   ~~ data name/kind ... ~~
}
\description{The PANSS or the Positive and Negative Syndrome Scale is a medical scale used for measuring symptom severity of patients with schizophrenic conditions.	\code{panss} contains data from a longitudinal study where 3 different treatments were considered. Patients were followed for 8 weeks and PANSS score was recorded on week 0, 1, 2, 4, 6 and 8. The lower PANSS score a patient has, the less symptoms. Data was extracted from a larger, and confidential, set of clinical trial data from a randomised clinical trial.

%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(panss)}
\format{
  A data frame with 685 observations on the following 4 variables.
  \describe{
    \item{\code{treat}}{a factor variable with 3 levels}
    \item{\code{time}}{observation times}
    \item{\code{Y}}{PANSS score}
    \item{\code{id}}{id number}
  }
}


\source{\url{http://www.lancaster.ac.uk/staff/diggle/APTS-data-sets/PANSS_short_data.txt}
	
	P. Diggle. Dealing with missing values in longitudinal studies. In Recent Advances in the Statistical Analysis of Medical Data, editor, B.S. Everitt and G. Dunn, pages 203–228, 1998.

%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\note{Data was reshaped from the source with \code{\link{reshape}}}

\examples{
library(FLIM)	
data(panss)
head(panss, 8)
}
\keyword{datasets}
