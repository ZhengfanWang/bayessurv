# Function to generate time-to-event data for a given hazard rate
generate_piecewise_time <- function(n, hazards, intervals) {
  times <- numeric(n)
  events <- numeric(n)
  
  for (i in 1:n) {
    total_time <- 0
    event_occurred <- FALSE
    
    for (j in 1:length(hazards)) {
      time_interval <- intervals[j + 1] - intervals[j]
      event_time <- rexp(1, rate = hazards[j])
      
      if (event_time < time_interval) {
        total_time <- total_time + event_time
        event_occurred <- TRUE
        break
      } else {
        total_time <- total_time + time_interval
      }
    }
    
    times[i] <- total_time
    events[i] <- ifelse(event_occurred, 1, 0)
  }
  
  return(data.frame(time = times, event = events))
}