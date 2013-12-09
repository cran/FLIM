flim <-
function(formula, data, id, obstime,
                  t.values=NULL, method="locf", lambda=NULL) {
  info <- FlimFormula(formula)
  covariates <- info$covariates
  responses <- info$responses
  Cdata <- as.data.frame(data[, c(id, obstime, covariates, responses)])
  Cdata <- Cdata[order(Cdata[, 1], Cdata[, 2]),]
  first.obs <- match(unique(Cdata[, 1]), Cdata[, 1])
  if(any(is.na(Cdata[first.obs, ]))) {
    indx <- apply(Cdata[first.obs, ], 1, function(x) any(is.na(x)))
    who <- Cdata[first.obs[which(indx)], 1]
    print(c("Check the following IDs: ", who))
    stop("There are missing values in the first observation set for some IDs")
  }
  Cdata$obs.type <- rep(1, dim(Cdata)[1])
  times <- SetImputationTimes(Cdata, t.values)
  Hdata <- FillInGaps(Cdata, times)
  Hdata <- ImputateGaps(Hdata, times, responses, method)
  Hdata <- ExpandDataset(Hdata, times)  
  Hdata <- SetStaticVariables(Hdata, covariates)
  Hdata <- SetIncrements(Hdata, times, responses) 
  flim.fit <- ImputateMissingValues(Hdata, times, covariates,
                                    method, lambda, info) 
  model.fits <- flim.fit$model.fits
  Hdata <- flim.fit$dataset
  Hdata <- Hdata[, c(id, obstime, responses, covariates, "obs.type")]
  rownames(Hdata) <- seq(1, dim(Hdata)[1])
  returnme <- list(df = Hdata,
                   dataset = Hdata,
                   fit = model.fits,
                   times = times,
                   t.values = t.values,
                   clean = Cdata,
                   covariates = covariates,
                   responses = responses,
                   method = method,
                   info = info,
                   formula = info$reg.fmlas,
                   call = match.call(),
                   lambda = lambda)
  class(returnme) <- "flim"
  returnme
}
