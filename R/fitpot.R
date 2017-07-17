fitpot<-function (data, threshold = NA, nextremes = NA, evi = NA) 
{
  data <- as.numeric(data)
  data <- data[!is.na(data)]
  data <- sort(data)
  n <- length(data)
  if (!is.na(nextremes) && !is.na(threshold)) 
    stop("Enter EITHER a threshold or the number of upper extremes")
  if (is.na(nextremes) && is.na(threshold)) {
    threshold <- min(data)
  }
  else {
    if (!is.na(nextremes)) 
      threshold <- rev(data)[as.numeric(nextremes)]
  }
  xdat <- data[data >= threshold] - threshold
  tes <- ifelse(Tm(xdat, nsim = 0, m = 5, omit = 8)["evi"] > 
                  0.1, T, F)
  prob <- length(xdat)/n
  aux <- egpd(xdat, evi = evi, heavy = tes)
  if (is.na(evi)) 
    evi <- aux$evi
  psi <- aux$psi
  c(evi = evi, psi = psi, threshold = threshold, prob = prob)
}
