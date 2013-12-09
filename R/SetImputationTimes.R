SetImputationTimes <-
function(dataset, t.values) {
  if(is.null(t.values)) t.values <- unique(dataset[, 2])
  return(t.values)
}
