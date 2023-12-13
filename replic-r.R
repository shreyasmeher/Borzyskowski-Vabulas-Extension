library(clarify)
library(MatchIt)  # If propensity score matching is needed
library(Amelia)   # For multiple imputation
library(dplyr)
library(kableExtra)
library(logistf)
library(haven)
library(knitr)
library(broom)
library(tidyr)
# end

data <- read_dta("replication code/RIO_replication data.dta")

PolDom_vars <- c("Democracy_Lag1", "GovmtOrientChange", "Nationalist_Lag1")
Function_vars <- c("Institutionalization_Lag1", "IOavgDemScore", "IOissuePolitics", "IOissueEcon")
PolIntl_vars <- c("StatePowerChange", "PrefDiversionFromIOavg_Lag1", "WithdrawalLeadState_Lag1")
controls <- c("IOMembershipDuration_Lag1Log", "IOsize_Lag1Log", "time", "time2", "time3")

# Construct the formulas
formula1 <- as.formula(paste("Withdrawal ~", paste(PolDom_vars, collapse = " + "), "+", paste(controls, collapse = " + ")))
formula2 <- as.formula(paste("Withdrawal ~", paste(Function_vars, collapse = " + "), "+", paste(controls, collapse = " + ")))
formula3 <- as.formula(paste("Withdrawal ~", paste(PolIntl_vars, collapse = " + "), "+", paste(controls, collapse = " + ")))
formula4 <- as.formula(paste("Withdrawal ~", paste(c(PolDom_vars, Function_vars, PolIntl_vars), collapse = " + "), "+", paste(controls, collapse = " + ")))

# Fit the models
fit1 <- glm(formula1, data = data, family = "binomial")
fit2 <- glm(formula2, data = data, family = "binomial")
fit3 <- glm(formula3, data = data, family = "binomial")
fit4 <- glm(formula4, data = data, family = "binomial")

s_fit1 <- clarify::sim(fit1)
s_fit2 <- clarify::sim(fit2)
s_fit3 <- clarify::sim(fit3)
s_fit4 <- clarify::sim(fit4)

# Example for one model
est_fit1 <- sim_setx(s_fit1, x = list(Democracy_Lag1 = mean(data$Democracy_Lag1, na.rm = TRUE)), x1 = list(Democracy_Lag1 = mean(data$Democracy_Lag1, na.rm = TRUE) + 1))
summary(est_fit1)

plot(est_fit1)

# Extract model summaries
summary_fit1 <- glance(fit1)
summary_fit2 <- glance(fit2)
summary_fit3 <- glance(fit3)
summary_fit4 <- glance(fit4)

# Combine summaries into a single data frame
model_summaries <- rbind(summary_fit1, summary_fit2, summary_fit3, summary_fit4)
model_summaries$model <- c("Model 1", "Model 2", "Model 3", "Model 4")

kable(model_summaries, caption = "Determinants of IGO Withdrawals", align = 'c')

# Select relevant columns
model_summaries_selected <- model_summaries %>%
  select(model, AIC, BIC, logLik, deviance, df.residual)

kable(model_summaries_selected, caption = "Determinants of IGO Withdrawals", align = 'c')


# Extract coefficients from each model and add model identification
tidy_fit1 <- tidy(fit1) %>% mutate(model = "Model 1")
tidy_fit2 <- tidy(fit2) %>% mutate(model = "Model 2")
tidy_fit3 <- tidy(fit3) %>% mutate(model = "Model 3")
tidy_fit4 <- tidy(fit4) %>% mutate(model = "Model 4")

# Combine all the tidied models into one dataframe
combined_tidy <- bind_rows(tidy_fit1, tidy_fit2, tidy_fit3, tidy_fit4)

# Use pivot_wider from tidyr to reshape data for a wide table format
wide_format <- combined_tidy %>%
  pivot_wider(names_from = model, values_from = c(estimate, std.error))

# Create the table
kable(wide_format, caption = "Determinants of IGO Withdrawals", booktabs = TRUE) %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)



# Define the predictor variables
predictor_vars <- c("Democracy_Lag1", "GovmtOrientChange", "Nationalist_Lag1",
                    "Institutionalization_Lag1", "IOavgDemScore", "IOissuePolitics", "IOissueEcon",
                    "PrefDiversionFromIOavg_Lag1", "WithdrawalLeadState_Lag1", "StatePowerChange",
                    "IOMembershipDuration_Lag1Log", "IOsize_Lag1Log", "time", "time2", "time3")


PolDom_vars <- c("Democracy_Lag1", "GovmtOrientChange", "Nationalist_Lag1")
Function_vars <- c("Institutionalization_Lag1", "IOavgDemScore", "IOissuePolitics", "IOissueEcon")
PolIntl_vars <- c("StatePowerChange", "PrefDiversionFromIOavg_Lag1", "WithdrawalLeadState_Lag1")
controls <- c("IOMembershipDuration_Lag1Log", "IOsize_Lag1Log", "time", "time2", "time3")

# Combine the variables for the 'All' model
all_vars <- c(PolDom_vars, Function_vars, PolIntl_vars, controls)

# Fit the four models
fit1_new <- logistf(as.formula(paste("Withdrawal ~", paste(PolDom_vars, collapse = " + "), "+", paste(controls, collapse = " + "))), data = data)
fit2_new <- logistf(as.formula(paste("Withdrawal ~", paste(Function_vars, collapse = " + "), "+", paste(controls, collapse = " + "))), data = data)
fit3_new <- logistf(as.formula(paste("Withdrawal ~", paste(PolIntl_vars, collapse = " + "), "+", paste(controls, collapse = " + "))), data = data)
fit4_new <- logistf(as.formula(paste("Withdrawal ~", paste(all_vars, collapse = " + "))), data = data)

# Summarize the models
summary(fit1_new)
summary(fit2_new)
summary(fit3_new)
summary(fit4_new)

