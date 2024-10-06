library(muhaz)
library(survival)

# Set seed for reproducibility
set.seed(12345)

## load functions
Rfiles <- list.files(file.path(paste0(getwd(),"/R/")), ".R") #gets names of r files
sapply(paste0(paste0(getwd(),"/R/"), Rfiles), source) #gets functions from file

# Parameters
n_samples <- 1000  # Total number of patients
proportion_control <- 0.5  # Proportion in control group
n_control <- round(n_samples * proportion_control)
n_treatment <- n_samples - n_control

# Define piecewise hazard rates and time intervals
time_intervals <- c(0, 5, 10, 15)  # Time intervals (0-5, 5-10, 10-15)
hazard_treatment_true <- c(0.05, 0.1, 0.15)  # Hazard rates for treatment group
hazard_control_true <- c(0.1, 0.1, 0.1)  # Hazard rates for control group

# Generate data for control group
control_data <- generate_piecewise_time(n_control, hazard_control_true, time_intervals)
control_data$group <- "control"

# Generate data for treatment group
treatment_data <- generate_piecewise_time(n_treatment, hazard_treatment_true, time_intervals)
treatment_data$group <- "treatment"

# Combine into one dataset
time_to_event_data <- rbind(control_data, treatment_data)
saveRDS(time_to_event_data, "data/piecewise.rds")


