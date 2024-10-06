
time_to_event_data <- readRDS("data/piecewise.rds")
# Kaplan-Meier estimation
km_fit <- survfit(Surv(time, event) ~ group, data = time_to_event_data)

# Print KM summary
summary(km_fit)

# Plot Kaplan-Meier curves
ggsurvplot(km_fit,
           data = time_to_event_data,
           pval = TRUE,           # Show p-value from log-rank test
           conf.int = TRUE,       # Show confidence intervals
           risk.table = TRUE,     # Show risk table
           xlab = "Time",
           ylab = "Survival Probability",
           title = "Kaplan-Meier Survival Curves for Treatment and Control Groups",
           legend.title = "Group",
           legend.labs = c("Control", "Treatment"),
           palette = c("#E7B800", "#2E9FDF"))  # Custom colors for the plot


# Split the data into treatment and control groups
control_data <- subset(time_to_event_data, group == "control")
treatment_data <- subset(time_to_event_data, group == "treatment")

# Estimate hazard functions using the muhaz function
control_hazard <- muhaz(control_data$time, control_data$event,max.time = 14.5)
treatment_hazard <- muhaz(treatment_data$time, treatment_data$event,max.time = 14.5)


# Create a time grid for plotting the true hazard functions
time_grid <- seq(0, 15, by = 0.1)
control_true_hazard_values <- sapply(time_grid, true_hazard, hazard = hazard_control_true, cutpoint = time_intervals[-1])
treatment_true_hazard_values <- sapply(time_grid, true_hazard, hazard = hazard_treatment_true, cutpoint = time_intervals[-1])
# Plot the estimated hazard functions
plot(control_hazard, col = "blue", lty = 1, main = "Estimated and True Hazard Functions",
     xlab = "Time", ylab = "Hazard Rate", lwd = 2, ylim = c(0.01, 0.14))

# Add the estimated hazard function for the treatment group
lines(treatment_hazard, col = "red", lty = 1, lwd = 2)

# Add the true hazard functions to the plot
lines(time_grid, control_true_hazard_values, col = "blue", lty = 3, lwd = 2)
lines(time_grid, treatment_true_hazard_values, col = "red", lty = 3, lwd = 2)

# Add legend
legend("topright", legend = c("Control (Estimated)", "Treatment (Estimated)", "Control (True)", "Treatment (True)"), 
       col = c("blue", "red", "blue", "red"), lty = c(1, 1, 3, 3), lwd = 2)

