install.packages("SparseM")
install.packages("minqa")
install.packages("lme4")
install.packages("pbkrtest")
install.packages("quantreg")
install.packages("car")
install.packages("lmtest")
install.packages("AER")
install.packages('Formula')
install.packages('geepack')
install.packages('sandwich')
install.packages('MatchIt')
install.packages('maxLik')
install.packages('MCMCpack')
install.packages('survey')
install.packages('VGAM')
install.packages("processx")
install.packages("devtools")
install.packages("haven")
install.packages("margins")
devtools::install_github('IQSS/Zelig')
install_github("sondreus/QuickEffectSize")
library(Zelig)
library(devtools)
library(haven)
library(dplyr)
library(zoo)
library(visdat)
library(readr)
library(peacesciencer)
library(vdemdata)
library(stargazer)
library(coefplot)
library(margins)
library(Amelia)
library(QuickEffectSize)

# Load in the data
data <- read_dta("RIO_replication data.dta")
IGOData <- read_dta("IGOData.dta")
chisol <- read_csv("CHISOLSstyr5_0.csv")


# Define the formulas for your models
formula1 <- as.formula("Withdrawal ~ Democracy_Lag1 + GovmtOrientChange + Nationalist_Lag1 + IOMembershipDuration_Lag1Log + IOsize_Lag1Log + time + time2 + time3")
formula2 <- as.formula("Withdrawal ~ Institutionalization_Lag1 + IOavgDemScore + IOissuePolitics + IOissueEcon + IOMembershipDuration_Lag1Log + IOsize_Lag1Log + time + time2 + time3")
formula3 <- as.formula("Withdrawal ~ StatePowerChange + PrefDiversionFromIOavg_Lag1 + WithdrawalLeadState_Lag1 + IOMembershipDuration_Lag1Log + IOsize_Lag1Log + time + time2 + time3")
formula4 <- as.formula("Withdrawal ~ Democracy_Lag1 + GovmtOrientChange + Nationalist_Lag1 + Institutionalization_Lag1 + IOavgDemScore + IOissuePolitics + IOissueEcon + StatePowerChange + PrefDiversionFromIOavg_Lag1 + WithdrawalLeadState_Lag1 + IOMembershipDuration_Lag1Log + IOsize_Lag1Log + time + time2 + time3")
formula5 <- as.formula("Withdrawal ~ Democracy_Lag1 + AvgIGOsShared + MovingAvgCentrality + IOsize_Lag1Log + IOMembershipDuration_Lag1Log + + time + time2 + time3")

# Fit the models using Zelig's relogit
relogit_fit1 <- zelig(formula1, data = data, model = "relogit")
relogit_fit2 <- zelig(formula2, data = data, model = "relogit")
relogit_fit3 <- zelig(formula3, data = data, model = "relogit")
relogit_fit4 <- zelig(formula4, data = data, model = "relogit")
relogit_fit5 <- zelig(formula5, data = data_merged, model = "relogit")

summary(relogit_fit1)
summary(relogit_fit2)
summary(relogit_fit3)
summary(relogit_fit4)
summary(relogit_fit5)

# Get all unique values for the 'IGO_long' column
unique_igo_long <- unique(data$IGO_long)

# View the unique values
print(unique_igo_long)

# Preparing IGOData for moving average calculation
IGOData_prepared <- IGOData %>%
  select(statea, year, IGOSame) %>%
  arrange(statea, year) %>%
  group_by(statea)

# Calculate the 3-year moving average for IGOSame
IGOData_avg <- IGOData_prepared %>%
  mutate(AvgIGOsShared = rollapply(IGOSame, width = 3, FUN = function(x) mean(x, na.rm = TRUE), 
                                   fill = NA, align = "right", partial = TRUE))

# Ensure column names are consistent for merging
colnames(IGOData_avg) <- c("cowcode", "year", "IGOSame", "AvgIGOsShared")

IGOSame_yearly_avg <- IGOData_avg %>% 
  group_by(cowcode, year) %>% 
  summarise(AvgIGOsShared = mean(AvgIGOsShared, na.rm = TRUE)) 

# Merge with your main dataset
data_merged <- merge(data, IGOSame_yearly_avg, by = c("cowcode", "year"), all.x = TRUE)

# Calculate the 3-year moving average for Cent1 for each statea (country)
IGOData_avg <- IGOData %>%
  arrange(statea, year) %>%
  group_by(statea) %>%
  mutate(MovingAvgCentrality = rollmean(Cent1, 3, fill = NA, align = "right"))

Cent_yearly_avg <- IGOData_avg %>% 
  group_by(statea, year) %>% 
  summarise(MovingAvgCentrality = mean(MovingAvgCentrality, na.rm = TRUE)) 

# Ensure column names are consistent for merging
colnames(Cent_yearly_avg)[1] <- "cowcode"

# Merge with your main dataset
data_merged <- merge(data_merged, Cent_yearly_avg, by = c("cowcode", "year"), all.x = TRUE)

# Lagging the AvgIGOsShared and MovingAvgCentrality by one year
data_merged <- data_merged %>%
  arrange(cowcode, year) %>%
  group_by(cowcode) %>%
  mutate(AvgIGOsShared_Lag1 = lag(AvgIGOsShared, 1),
         MovingAvgCentrality_Lag1 = lag(MovingAvgCentrality, 1))

# Check for NA values after merging and lagging
sum(is.na(data_merged$AvgIGOsShared_Lag1))
sum(is.na(data_merged$MovingAvgCentrality_Lag1))

# Update formula to use the lagged variables
formula5 <- as.formula("Withdrawal ~ Democracy_Lag1 + AvgIGOsShared_Lag1 + MovingAvgCentrality_Lag1 + IOsize_Lag1Log + IOMembershipDuration_Lag1Log + time + time2 + time3")

# Running models
relogit_fit5 <- zelig(formula5, data = data_merged, model = "relogit")
summary(relogit_fit5)
relogit_fit6 <- zelig(formula5, data = data_merged, model = "logit.bayes")
summary(relogit_fit6)

# Trying to plot
relogit_fit5
range<-quantile(data_merged$AvgIGOsShared_Lag1, probs=c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
z_sim<-relogit_fit5 %>%
  setx(AvgIGOsShared_Lag1 = range) %>%
  sim()
z_sim

z_sim %>%
  # Let's get these in a dataset
  zelig_qi_to_df() %>%
  # and shrink them down to just the median values and 95% confidence intervals
  # Let's get them for just expected values
  qi_slimmer(qi_type = "ev", ci = 0.95) %>%
  # And we can map them here!
  ggplot(mapping = aes(x = AvgIGOsShared_Lag1, y = qi_ci_median, 
                       ymin = qi_ci_min, ymax = qi_ci_max)) +
  geom_ribbon(alpha = 0.5, fill = "firebrick") +
  geom_line() +
  theme_minimal(base_size = 14) +
  theme(plot.caption = element_text(hjust = 0.5)) +
  labs(x = "Avg IGOs Shared",
       y = "Withdrawals",
       caption = "Based on 1000 simulations made with the sim() function in Zelig.")

range<-quantile(data_merged$MovingAvgCentrality_Lag1, probs=c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
z_sim<-relogit_fit5 %>%
  setx(MovingAvgCentrality_Lag1 = range) %>%
  sim()
z_sim

z_sim %>%
  # Let's get these in a dataset
  zelig_qi_to_df() %>%
  # and shrink them down to just the median values and 95% confidence intervals
  # Let's get them for just expected values
  qi_slimmer(qi_type = "ev", ci = 0.95) %>%
  # And we can map them here!
  ggplot(mapping = aes(x = MovingAvgCentrality_Lag1, y = qi_ci_median, 
                       ymin = qi_ci_min, ymax = qi_ci_max)) +
  geom_ribbon(alpha = 0.5, fill = "firebrick") +
  geom_line() +
  theme_minimal(base_size = 14) +
  theme(plot.caption = element_text(hjust = 0.5)) +
  labs(x = "Centrality",
       y = "Withdrawals",
       caption = "Based on 1000 simulations made with the sim() function in Zelig.")


# Set the baseline values for the covariates using setx()
x_baseline <- setx(relogit_fit5)

# Optionally, set alternative values to compare against the baseline using setx()
# For example, if you want to see the effect of changing AvgIGOsShared_Lag1:
x_alternative <- setx(relogit_fit5, AvgIGOsShared_Lag1 = 5)

# Simulate the quantities of interest from the fitted model
s.out <- sim(relogit_fit5, x = x_baseline, x1 = x_alternative)

# Plotting expected values
plot(s.out, "ev", main = "Expected Probability of Withdrawal")

# Plotting predicted values
plot(s.out, "pr", main = "Predicted Probability of Withdrawal")

# Plotting first differences
plot(s.out, "fd", main = "First Differences in Probability of Withdrawal")

qes(relogit_fit5, iv.var = "MovingAvgCentrality_Lag1", xlab = "Centrality (n = 1000 simulations using Zelig & QuickEffectTime)", ylab = "Withdrawal", sim.n = 1000)
qes(relogit_fit5, iv.var = "AvgIGOsShared_Lag1", xlab = "Avg IGOs Shared (n = 1000 simulations using Zelig & QuickEffectTime)", ylab = "Withdrawal")



# ----------------------------------
# CHISOLS  Project Extension

# First, rename the 'ccode' column in chisol to 'cowcode' to match the data_merged dataset
chisol <- rename(chisol, cowcode = ccode)
str(chisol)

# Now merge the datasets
merged_data <- merge(data_merged, chisol[, c('cowcode', 'year', 'solschange', 'regtrans')], 
                     by = c('cowcode', 'year'), 
                     all.x = TRUE)

# Check the structure of the merged data
str(merged_data)

# Replace 1000 with the number of samples you want
sampled_data <- sample_n(merged_data, 1000, replace = TRUE)

# To view the sampled data
vis_dat(sampled_data, warn_large_data = FALSE)

# Lagging variable
merged_data <- merged_data %>%
  group_by(cowcode) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(solschange_lag1 = lag(solschange, n = 1, default = NA), regtrans_lag1 = lag(regtrans, n=1, default=NA))

# Replace original equations
formula6 <- as.formula("Withdrawal ~ Democracy_Lag1 + solschange_lag1 + regtrans_lag1 + IOMembershipDuration_Lag1Log + IOsize_Lag1Log + time + time2 + time3")
relogit_fit7 <- zelig(formula6, data = merged_data, model = "relogit")
summary(relogit_fit7)
relogit_fit8 <- zelig(formula6, data = merged_data, model = "logit.bayes")
summary(relogit_fit8)



