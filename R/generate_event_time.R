# Generate event time with a piecewise baseline hazard function and covariates
generate_event_time <- function(age, gender, ocs_start_time, ocs_stop_time) {
  time <- 0
  ocs_exposure <- 0  # No OCS exposure initially
  
  for (i in 1:(length(hazard_intervals) - 1)) {
    # Determine the baseline hazard for the current interval
    base_hazard <- baseline_hazards[i]
    
    # Update OCS exposure status depending on time
    if (time >= ocs_start_time & time <= ocs_stop_time) {
      ocs_exposure <- 1  # OCS exposure is active
    } else {
      ocs_exposure <- 0  # No OCS exposure
    }
    
    # Calculate covariate-adjusted hazard
    hazard <- covariate_hazard(base_hazard, age, gender, ocs_exposure)
    
    # Generate event time for this interval based on the covariate-adjusted hazard
    event_time_in_interval <- rexp(1, rate = hazard)
    
    if (time + event_time_in_interval <= hazard_intervals[i + 1]) {
      return(time + event_time_in_interval)
    } else {
      time <- hazard_intervals[i + 1]
    }
  }
  
  return(time)
}