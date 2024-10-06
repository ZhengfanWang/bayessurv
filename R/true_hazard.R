# Define the true piecewise hazard functions
true_hazard <- function(t,cutpoint,hazard) {
  if (t < cutpoint[1]) {
    return(hazard[1])
  } else if (t < cutpoint[2]) {
    return(hazard[2])
  } else {
    return(hazard[3])
  }
}
