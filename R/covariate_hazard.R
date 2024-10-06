# Function to calculate hazard based on covariates (age, sex, OCS)
covariate_hazard <- function(base_hazard, age, gender, ocs_exposure) {
  # Adjust hazard based on covariates
  hazard <- base_hazard * (1 + 0.01 * (age - 70))  # Age effect: 1% increase per year over 70
  hazard <- hazard * ifelse(gender == "Female", 0.8, 1)  # Females have 20% lower hazard
  hazard <- hazard + ifelse(ocs_exposure == 1, 0.1, 0)  # OCS exposure increases hazard by 50%
  return(hazard)
}
