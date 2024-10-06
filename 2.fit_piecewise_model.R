library(tidyverse)
## Install
install.packages("rstanarm", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(rstanarm)
library(tidybayes)
library(bayesplot)
library(survival)

time_to_event_data <- readRDS("data/piecewise.rds")

## Cutpoints
cutpoints <- quantile(time_to_event_data$time,probs = seq(from = 0, to = 1, by = 0.2))
## First cutpoint should be time 0.
cutpoints[1] <- 0
## Last cutpoint should be larger than the maximum failure time.
cutpoints[length(cutpoints)] <- cutpoints[length(cutpoints)] + 1
cutpoints

## real cutpoints 
cutpoints_true <- c(5, 10, 15)     # Time intervals (years) c(0, 5, 10, 15)  
cutpoints[length(cutpoints)] <- cutpoints_true[length(cutpoints_true)] + 1
## Evaluation grid
grid <- seq(from = 0, to = max(cutpoints), by = 0.5)

## Load and compile
piecewise_model <- rstan::stan_model("mod/surv_wo_cov.stan")

treatment_data <- time_to_event_data %>% filter(group == "treatment")
control_data <- time_to_event_data %>% filter(group == "control")

piecewise_sample <- rstan::sampling(object = piecewise_model,
                                    data = list(lambda1_mean = 0.01,
                                                lambda1_length_w = 10^4,
                                                w = 0.01,
                                                lambda_star = 0.05,
                                                K = length(cutpoints) - 1,
                                                cutpoints = cutpoints,
                                                N = length(treatment_data$time),
                                                cens = treatment_data$event,
                                                y = treatment_data$time,
                                                grid_size = length(grid),
                                                grid = grid))

stan_surv_exponential <- stan_surv(formula = Surv(time, status) ~ group,
                                   data = time_to_event_data,
                                   basehaz = "exp")
