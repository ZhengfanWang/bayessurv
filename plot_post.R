# Extract the posterior samples of the generated quantities
generated_quantities <- extract(piecewise_sample, pars = c("S_grid", "h_grid"))

# Assuming you have access to the grid points used in the Stan code
grid <- seq(0, 16, length.out = length(generated_quantities$S_grid[1,]))  # Adjust this based on your grid size

# Calculate the mean survival and hazard values at each grid point
S_mean <- apply(generated_quantities$S_grid, 2, quantile, c(0.1,0.5,0.9))
h_mean <- apply(generated_quantities$h_grid, 2, quantile, c(0.1,0.5,0.9))

treatment_true_hazard_values <- sapply(grid, true_hazard, hazard = hazard_treatment_true, cutpoint = time_intervals[-1])
# Perform kernel-based hazard estimation using muhaz
# Assuming you have `y` (event times) and `cens` (censoring indicators) from your data
treatment_hazard <- muhaz(treatment_data$time, treatment_data$event,max.time = 15)

# Plot the survival curve
survival_df <- data.frame(Time = grid, Survival = S_mean[2,],upper = S_mean[1,],lower = S_mean[3,])
ggplot(survival_df, aes(x = Time, y = Survival)) +
  geom_line(color = 'blue', size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = 'blue')+
  labs(title = 'Survival Curve', x = 'Time', y = 'Survival Probability') +
  theme_minimal()

# Plot the hazard function
hazard_df <- data.frame(Time = grid, Hazard = h_mean[2,], upper = h_mean[1,],lower = h_mean[3,], True_Hazard = treatment_true_hazard_values)
ggplot(hazard_df, aes(x = Time, y = Hazard)) +
  geom_line(color = 'red',linetype= "dashed", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = 'red')+
  geom_line(aes(y = True_Hazard), color = 'black', linetype = "solid", size = 1) +
  geom_line(data = data.frame(Time = treatment_hazard$est.grid, Hazard = treatment_hazard$haz.est), 
            aes(x = Time, y = Hazard), color = 'blue', linetype = "dashed", size = 1) +
  labs(title = 'Hazard Function', x = 'Time', y = 'Hazard Rate') +
  theme_minimal()
