names(mod_data_ocs1)

library(tidyverse)
library(survival)
library(rstan)



## Cutpoints
cutpoints <- quantile(mod_data_ocs0$time,probs = seq(from = 0, to = 1, by = 0.2))
## First cutpoint should be time 0.
cutpoints[1] <- 0
## Last cutpoint should be larger than the maximum failure time.
cutpoints[length(cutpoints)] <- cutpoints[length(cutpoints)] + 1
cutpoints

## real cutpoints 
cutpoints_true <- hazard_intervals   # Time intervals (years)
cutpoints[length(cutpoints)] <- cutpoints_true[length(cutpoints_true)] + 1
## Evaluation grid
grid <- seq(from = 0, to = max(cutpoints), by = 0.5)

## Load and compile
piecewise_model <- rstan::stan_model("mod/surv_wo_cov.stan")
piecewise_ph_model <- rstan::stan_model("mod/piecewise_ph_mod.stan")
piecewise_covs_model <- rstan::stan_model("mod/piecewise_covs_mod.stan")
piecewise_covs_model2 <- rstan::stan_model("mod/piecewise_covs_mod2.stan")

piecewise_sample <- rstan::sampling(object = piecewise_model,
                                    data = list(lambda1_mean = 0.01,
                                                lambda1_length_w = 10^4,
                                                w = 0.01,
                                                lambda_star = 0.05,
                                                K = length(cutpoints) - 1,
                                                cutpoints = cutpoints,
                                                N = length(mod_data_ocs0$time),
                                                cens = mod_data_ocs0$status,
                                                y = mod_data_ocs0$time,
                                                grid_size = length(grid),
                                                grid = grid))
piecewise_sample

piecewise_ph_sample <-
  rstan::sampling(object = piecewise_ph_model,
                  data = list(lambda1_mean = 0.01,
                              lambda1_length_w = 10^4,
                              w = 0.01,
                              lambda_star = 0.05,
                              beta_mean = 0,
                              beta_sd = 100,
                              K = length(cutpoints) - 1,
                              cutpoints = cutpoints,
                              N = length(mod_data_ocs0$time),
                              cens = mod_data_ocs0$status,
                              y = mod_data_ocs0$time,
                              x = mod_data_ocs0$OCS_oa,
                              grid_size = length(grid),
                              grid = grid))

piecewise_ph_sample

# piecewise_covs_sample <-
#   rstan::sampling(object = piecewise_covs_model,
#                   data = list(lambda1_mean = 0.01,
#                               lambda1_length_w = 10^4,
#                               w = 0.01,
#                               lambda_star = 0.05,
#                               beta_mean = 0,
#                               beta_sd = 100,
#                               K = length(cutpoints) - 1,
#                               cutpoints = cutpoints,
#                               N = length(mod_data_ocs0$time),
#                               cens = mod_data_ocs0$status,
#                               y = mod_data_ocs0$time,
#                               x_ocs = mod_data_ocs0$OCS_oa,
#                               x_sex = ifelse(mod_data_ocs0$gender == "Female", 1, 0),
#                               x_age = mod_data_ocs0$age,
#                               grid_size = length(grid),
#                               grid = grid))
# 
# piecewise_covs_sample

piecewise_covs_sample2 <-
  rstan::sampling(object = piecewise_covs_model2,
                  data = list(lambda1_mean = 0.01,
                              lambda1_length_w = 10^4,
                              w = 0.01,
                              lambda_star = 0.05,
                              beta_mean = 0,
                              beta_sd = 100,
                              K = length(cutpoints) - 1,
                              P = 3,
                              cutpoints = cutpoints,
                              N = length(mod_data_ocs0$time),
                              cens = mod_data_ocs0$status,
                              y = mod_data_ocs0$time,
                              x = cbind(mod_data_ocs0$OCS_oa,
                                        mod_data_ocs0$age,
                                        ifelse(mod_data_ocs0$gender == "Female", 1, 0)
                              ),
                              grid_size = length(grid),
                              grid = grid))

piecewise_covs_sample2
###############################################
### cutpoint_true
##################################################

piecewise_sample_true <- rstan::sampling(object = piecewise_model,
                                    data = list(lambda1_mean = 0.01,
                                                lambda1_length_w = 10^4,
                                                w = 0.01,
                                                lambda_star = 0.05,
                                                K = length(cutpoints_true) - 1,
                                                cutpoints = cutpoints_true,
                                                N = length(mod_data_ocs0$time),
                                                cens = mod_data_ocs0$status,
                                                y = mod_data_ocs0$time,
                                                grid_size = length(grid),
                                                grid = grid))
piecewise_sample_true

piecewise_ph_sample_true <-
  rstan::sampling(object = piecewise_ph_model,
                  data = list(lambda1_mean = 0.01,
                              lambda1_length_w = 10^4,
                              w = 0.01,
                              lambda_star = 0.05,
                              beta_mean = 0,
                              beta_sd = 100,
                              K = length(cutpoints_true) - 1,
                              cutpoints = cutpoints_true,
                              N = length(mod_data_ocs0$time),
                              cens = mod_data_ocs0$status,
                              y = mod_data_ocs0$time,
                              x = mod_data_ocs0$OCS_oa,
                              grid_size = length(grid),
                              grid = grid))
piecewise_ph_sample_true


# piecewise_covs_sample_true <-
#   rstan::sampling(object = piecewise_covs_model,
#                   data = list(lambda1_mean = 0.01,
#                               lambda1_length_w = 10^4,
#                               w = 0.01,
#                               lambda_star = 0.05,
#                               beta_mean = 0,
#                               beta_sd = 100,
#                               K = length(cutpoints_true) - 1,
#                               cutpoints = cutpoints_true,
#                               N = length(mod_data_ocs0$time),
#                               cens = mod_data_ocs0$status,
#                               y = mod_data_ocs0$time,
#                               x_ocs = mod_data_ocs0$OCS_oa,
#                               x_sex = ifelse(mod_data_ocs0$gender == "Female", 1, 0),
#                               x_age = mod_data_ocs0$age,
#                               grid_size = length(grid),
#                               grid = grid))
# 
# piecewise_covs_sample_true

piecewise_covs_sample2_true <-
  rstan::sampling(object = piecewise_covs_model2,
                  data = list(lambda1_mean = 0.01,
                              lambda1_length_w = 10^4,
                              w = 0.01,
                              lambda_star = 0.05,
                              beta_mean = 0,
                              beta_sd = 100,
                              K = length(cutpoints_true) - 1,
                              P = 3,
                              cutpoints = cutpoints_true,
                              N = length(mod_data_ocs0$time),
                              cens = mod_data_ocs0$status,
                              y = mod_data_ocs0$time,
                              x = cbind(mod_data_ocs0$OCS_oa,
                                        mod_data_ocs0$age,
                                        ifelse(mod_data_ocs0$gender == "Female", 1, 0)
                              ),
                              grid_size = length(grid),
                              grid = grid))

piecewise_covs_sample2
