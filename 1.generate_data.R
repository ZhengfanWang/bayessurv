# Load necessary libraries
library(survival)
library(dplyr)

## load functions
Rfiles <- list.files(file.path(paste0(getwd(),"/R/")), ".R") #gets names of r files
sapply(paste0(paste0(getwd(),"/R/"), Rfiles), source) #gets functions from file

# Set seed for reproducibility
set.seed(123)

# Parameters
N_patient <- 1687
N_female <- 893
prop_female <- N_female / N_patient
mean_age <- 76
sd_age <- 10.78
min_age <- 18
max_age <- 90

# Define piecewise hazard rates for different time intervals
hazard_intervals <- c(0, 2.5, 5, 7.5, 10)  # Time intervals (years)
baseline_hazards <- c(0.08, 0.09, 0.1, 0.11)  # Piecewise baseline hazards


# Generate patient characteristics
patients <- data.frame(
  patient_id = 1:N_patient,
  gender = ifelse(runif(N_patient) < prop_female, "Female", "Male"),
  age = pmin(pmax(rnorm(N_patient, mean_age, sd_age), min_age), max_age)
)

# Generate time-varying exposure (OCS) start and stop times
patients <- patients %>%
  mutate(ocs_start_time = runif(N_patient, 0, 10),  # Random start time for OCS within follow-up
         ocs_duration = runif(N_patient, 0.5, 3),   # Random OCS exposure duration (0.5 to 3 years)
         ocs_stop_time = ocs_start_time + ocs_duration)  # Calculate stop time as start + duration


# Apply the event time generation to each patient
patients <- patients %>%
  rowwise() %>%
  mutate(event_time = generate_event_time(age, gender, ocs_start_time, ocs_stop_time))

# Generate censoring times
max_follow_up <- 10  # Maximum follow-up time (years)
patients$censor_time <- runif(N_patient, 0, max_follow_up)

# Determine observed time and event status
patients$time <- pmin(patients$event_time, patients$censor_time)
patients$status <- ifelse(patients$event_time <= patients$censor_time, 1, 0)

mod_data_ocs0 <- patients %>% 
  mutate(OCS_oa = ifelse(ocs_start_time < censor_time, 0, 1))

# Create a base time-varying data set 
mod_data_ocs1 <- tmerge(
  data1 = patients,
  data2 = patients,
  id = patient_id,
  event = event(time, status),  # Time-to-event and status
  OCS = tdc(ocs_start_time)     # Time-dependent OCS exposure
)

# Add stop times for OCS
mod_data_ocs2 <- tmerge(
  mod_data_ocs1, mod_data_ocs1,
  id = patient_id,
  OCS = tdc(ocs_stop_time)  # Stopping OCS at stop time
)

# table(mod_data_ocs0$OCS_oa)
# table(mod_data_ocs1$OCS)
# table(mod_data_ocs2$OCS)

# Fit a Cox proportional hazards model with age, gender, and OCS exposure
cox_model <- coxph(Surv(tstart, tstop, event) ~ age + gender + OCS, data = mod_data_ocs2)
summary(cox_model)
mod_data_ocs1
# max(table(mod_data_ocs2$patient_id))
# sum(mod_data_ocs2$event)
# mod_data_ocs2 %>% filter(patient_id == 33)