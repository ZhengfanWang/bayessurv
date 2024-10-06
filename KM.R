library(survival)
library(survminer)

#### Given the mod_data from 1.generate_data.R, check the KM

km_fit <- survfit(Surv(time, status) ~ 1, data = mod_data_ocs1)

ggsurvplot(km_fit, 
           data = patients,
           xlab = "Time (Years)", 
           ylab = "Survival Probability",
           ggtheme = theme_minimal(),
           risk.table = TRUE,          # Add a risk table
           conf.int = TRUE,            # Add confidence intervals for the survival curve
           title = "Kaplan-Meier Survival Curve")

km_fit_gender <- survfit(Surv(time, status) ~ gender, data = mod_data_ocs1)

# Plot KM curve stratified by OCS exposure
ggsurvplot(km_fit_gender, 
           data = patients,
           xlab = "Time (Years)", 
           ylab = "Survival Probability",
           ggtheme = theme_minimal(),
           risk.table = TRUE,
           conf.int = TRUE,
           title = "Kaplan-Meier Survival Curve by gender")


