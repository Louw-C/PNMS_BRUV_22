# Testing Linear Regression Assumptions for Plankton-Fish Relationships
# This code specifically checks if your data meets the assumptions for linear regression

# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(car)        # For diagnostic tests (VIF, ncvTest)
library(lmtest)     # For Breusch-Pagan test
library(nortest)    # For normality tests

# Load your site-level data
site_data <- read_csv("/Users/louwclaassens/Documents/Documents - Louw’s MacBook Air/Palau/Research/PNMS Research/PICRC led PNMS Research/Pelagic BRUVs/Palau BRUV Project/2022 Annual survey/Publication/PNMS_BRUV_22/Data/Publication /Plankton and BRUV_Combined/Combined_Plankton_BRUV_Site_Level.csv")
print(head(site_data))

# ---- PART 1: EXPLORE DATA DISTRIBUTIONS ----

# Check distributions of key variables
par(mfrow=c(2,3))
hist(site_data$Total_Plankton_Count, main="Total Plankton Count", xlab="Count")
hist(site_data$Unique_Plankton_Types, main="Unique Plankton Types", xlab="Count")
hist(site_data$Total_Fish_MaxN, main="Total Fish MaxN", xlab="Count")
hist(site_data$Total_Fish_Biomass_kg, main="Total Fish Biomass", xlab="Biomass (kg)")
hist(site_data$Fish_Species_Richness, main="Fish Species Richness", xlab="Count")
par(mfrow=c(1,1))

# Create Q-Q plots to check for normality
par(mfrow=c(2,3))
qqnorm(site_data$Total_Plankton_Count, main="QQ Plot: Plankton Count")
qqline(site_data$Total_Plankton_Count)
qqnorm(site_data$Unique_Plankton_Types, main="QQ Plot: Plankton Types")
qqline(site_data$Unique_Plankton_Types)
qqnorm(site_data$Total_Fish_MaxN, main="QQ Plot: Fish MaxN")
qqline(site_data$Total_Fish_MaxN)
qqnorm(site_data$Total_Fish_Biomass_kg, main="QQ Plot: Fish Biomass")
qqline(site_data$Total_Fish_Biomass_kg)
qqnorm(site_data$Fish_Species_Richness, main="QQ Plot: Fish Richness")
qqline(site_data$Fish_Species_Richness)
par(mfrow=c(1,1))

# Formal normality tests (Shapiro-Wilk, small sample appropriate)
shapiro_tests <- list(
  Plankton_Count = shapiro.test(site_data$Total_Plankton_Count),
  Plankton_Types = shapiro.test(site_data$Unique_Plankton_Types),
  Fish_MaxN = shapiro.test(site_data$Total_Fish_MaxN),
  Fish_Biomass = shapiro.test(site_data$Total_Fish_Biomass_kg),
  Fish_Richness = shapiro.test(site_data$Fish_Species_Richness)
)

# Print results of normality tests
cat("\nNormality Test Results (Shapiro-Wilk):\n")
for (name in names(shapiro_tests)) {
  cat(name, ": W =", round(shapiro_tests[[name]]$statistic, 3), 
      ", p-value =", round(shapiro_tests[[name]]$p.value, 3), 
      ifelse(shapiro_tests[[name]]$p.value > 0.05, " (normal)", " (not normal)"),
      "\n")
}

# ---- PART 2: DATA TRANSFORMATIONS (IF NEEDED) ----

# Check if transformations improve normality
site_data_transformed <- site_data %>%
  mutate(
    log_Plankton_Count = log(Total_Plankton_Count),
    log_Fish_MaxN = log(Total_Fish_MaxN + 1),  # Adding 1 to handle zeros
    log_Fish_Biomass = log(Total_Fish_Biomass_kg + 0.01),  # Adding small value to handle near-zeros
    sqrt_Plankton_Count = sqrt(Total_Plankton_Count),
    sqrt_Fish_MaxN = sqrt(Total_Fish_MaxN)
  )

# Test normality of transformed variables
shapiro_transformed <- list(
  log_Plankton_Count = shapiro.test(site_data_transformed$log_Plankton_Count),
  log_Fish_MaxN = shapiro.test(site_data_transformed$log_Fish_MaxN),
  log_Fish_Biomass = shapiro.test(site_data_transformed$log_Fish_Biomass),
  sqrt_Plankton_Count = shapiro.test(site_data_transformed$sqrt_Plankton_Count),
  sqrt_Fish_MaxN = shapiro.test(site_data_transformed$sqrt_Fish_MaxN)
)

# Print results of normality tests for transformed variables
cat("\nNormality Test Results for Transformed Variables (Shapiro-Wilk):\n")
for (name in names(shapiro_transformed)) {
  cat(name, ": W =", round(shapiro_transformed[[name]]$statistic, 3), 
      ", p-value =", round(shapiro_transformed[[name]]$p.value, 3), 
      ifelse(shapiro_transformed[[name]]$p.value > 0.05, " (normal)", " (not normal)"),
      "\n")
}

# ---- PART 3: FIT MODELS AND TEST REGRESSION ASSUMPTIONS ----

# Fit linear regression models
# 1. Original variables
model1 <- lm(Total_Fish_MaxN ~ Total_Plankton_Count, data = site_data)
model2 <- lm(Fish_Species_Richness ~ Total_Plankton_Count, data = site_data)

# 2. Transformed variables (if needed based on normality tests)
# Example with log transformations
model1_log <- lm(log_Fish_MaxN ~ log_Plankton_Count, data = site_data_transformed)
model2_log <- lm(Fish_Species_Richness ~ log_Plankton_Count, data = site_data_transformed)

# Test for heteroscedasticity (constant variance assumption)
# Breusch-Pagan test
bp_test1 <- bptest(model1)
bp_test2 <- bptest(model2)
bp_test1_log <- bptest(model1_log)
bp_test2_log <- bptest(model2_log)

# Print results of heteroscedasticity tests
cat("\nHeteroscedasticity Test Results (Breusch-Pagan):\n")
cat("Plankton Count vs. Fish MaxN: BP =", round(bp_test1$statistic, 3), 
    ", p-value =", round(bp_test1$p.value, 3), 
    ifelse(bp_test1$p.value > 0.05, " (homoscedastic)", " (heteroscedastic)"),
    "\n")
cat("Plankton Count vs. Fish Richness: BP =", round(bp_test2$statistic, 3), 
    ", p-value =", round(bp_test2$p.value, 3), 
    ifelse(bp_test2$p.value > 0.05, " (homoscedastic)", " (heteroscedastic)"),
    "\n")
cat("Log-transformed model (MaxN): BP =", round(bp_test1_log$statistic, 3), 
    ", p-value =", round(bp_test1_log$p.value, 3), 
    ifelse(bp_test1_log$p.value > 0.05, " (homoscedastic)", " (heteroscedastic)"),
    "\n")
cat("Log-transformed model (Richness): BP =", round(bp_test2_log$statistic, 3), 
    ", p-value =", round(bp_test2_log$p.value, 3), 
    ifelse(bp_test2_log$p.value > 0.05, " (homoscedastic)", " (heteroscedastic)"),
    "\n")

# Check residuals for model diagnostic plots
par(mfrow=c(2,2))
plot(model1)
title(main="Diagnostic Plots for Original Fish MaxN Model", line=-1, outer=TRUE)

par(mfrow=c(2,2))
plot(model1_log)
title(main="Diagnostic Plots for Log-Transformed Fish MaxN Model", line=-1, outer=TRUE)
par(mfrow=c(1,1))

# ---- PART 4: OUTLIER DETECTION AND INFLUENCE ----

# Check for outliers and influential points
outliers_model1 <- data.frame(
  site = site_data$Site,
  studentized_residuals = rstudent(model1),
  cook_distance = cooks.distance(model1),
  leverage = hatvalues(model1),
  dffits = dffits(model1)
)

# Sort by studentized residuals to see potential outliers
outliers_model1 <- outliers_model1 %>%
  arrange(desc(abs(studentized_residuals)))

print(outliers_model1)

# Identify potential outliers
potential_outliers <- outliers_model1 %>%
  filter(abs(studentized_residuals) > 2 | cook_distance > 4/(nrow(site_data)))

# Print potential outliers
if(nrow(potential_outliers) > 0) {
  cat("\nPotential outliers or influential points detected:\n")
  print(potential_outliers)
} else {
  cat("\nNo significant outliers or influential points detected.\n")
}

# ---- PART 5: ALTERNATIVE REGRESSION APPROACHES ----

# If assumptions aren't met, consider robust regression
if(require(MASS)) {
  # Robust regression is less sensitive to outliers
  robust_model1 <- rlm(Total_Fish_MaxN ~ Total_Plankton_Count, data = site_data)
  summary(robust_model1)
  
  cat("\nComparison of Regular vs Robust Regression Coefficients:\n")
  cat("Regular regression coefficient:", round(coef(model1)[2], 5), "\n")
  cat("Robust regression coefficient:", round(coef(robust_model1)[2], 5), "\n")
}

# Consider non-parametric correlation (Spearman) which doesn't assume normality
spearman_cor <- cor.test(site_data$Total_Plankton_Count, site_data$Total_Fish_MaxN, 
                         method = "spearman")
cat("\nSpearman Rank Correlation (non-parametric):\n")
cat("rho =", round(spearman_cor$estimate, 3), 
    ", p-value =", round(spearman_cor$p.value, 3), "\n")

# ---- PART 6: CONCLUSION AND RECOMMENDATIONS ----

# Summarize what we found about assumptions and best modeling approach
cat("\n\n--- SUMMARY OF ASSUMPTION TESTING ---\n")
cat("Based on the tests performed, here's what we found about linear regression assumptions:\n\n")

# Check normality of residuals from the first model
resid_normality <- shapiro.test(residuals(model1))
cat("1. Normality of residuals: ", 
    ifelse(resid_normality$p.value > 0.05, "Satisfied", "Violated"), 
    " (Shapiro-Wilk p =", round(resid_normality$p.value, 3), ")\n")

# Check for heteroscedasticity
cat("2. Homoscedasticity (constant variance): ", 
    ifelse(bp_test1$p.value > 0.05, "Satisfied", "Violated"), 
    " (Breusch-Pagan p =", round(bp_test1$p.value, 3), ")\n")

# Check for linearity (this is subjective based on plots)
cat("3. Linearity: Check 'Residuals vs Fitted' plot for patterns\n")

# Check for independence (this typically comes from study design)
cat("4. Independence: Depends on your sampling design\n")

# Output recommended approach
cat("\nRECOMMENDED APPROACH:\n")
if(resid_normality$p.value > 0.05 && bp_test1$p.value > 0.05) {
  cat("Standard linear regression appears appropriate for your data.\n")
} else if(resid_normality$p.value <= 0.05 && any(sapply(shapiro_transformed, function(x) x$p.value > 0.05))) {
  cat("Consider using transformed variables for regression (log or sqrt transformation).\n")
} else {
  cat("Consider non-parametric approaches (e.g., Spearman correlation) or robust regression.\n")
}

# Additional recommendations
cat("\nADDITIONAL NOTES:\n")
if(nrow(potential_outliers) > 0) {
  cat("- You have potential outliers that may be influencing your results.\n")
  cat("  Consider analyzing with and without these points.\n")
}
cat("- With only 11 sites, statistical power is limited. Interpret p-values with caution.\n")
cat("- Consider ecological knowledge in addition to statistical results when drawing conclusions.\n")
