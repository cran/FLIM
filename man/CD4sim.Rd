\name{CD4sim}
\alias{CD4sim}
\docType{data}
\title{Simulated HIV data
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A simulated dataset of HIV patients undergoing treatment. At each timepoint treatment is initiated with increasing probability as CD4 values for patients are decreasing. Treatment has a positive effect on CD4. The lower the CD4 value, the higher risk the patient has for getting the event AIDS.
}
\usage{data(CD4sim)}
\format{
  A data frame with 4894 observations on the following 5 variables.
  \describe{
    \item{\code{id}}{id number}
    \item{\code{time}}{observation times}
    \item{\code{treat}}{treatment indicator}
    \item{\code{cd4}}{square root of CD4}
    \item{\code{AIDS}}{event indicator}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{Simulated data
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(CD4sim)
}
\keyword{datasets}
