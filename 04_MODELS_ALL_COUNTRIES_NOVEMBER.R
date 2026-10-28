

# 08.10.2025 run some models ------------------------------

library(dplyr)
library(tidyverse)
library(haven)
library(survey)
library(lme4)
library(ggeffects)
library(openxlsx)


# DATA
# load('00_data\\DATA_FINAL_IPUMS_JULY.RData')

load("00_data\\DATA_FINAL_MODEL_OCTOBER.RData")

df <- df_model

# remove individuals with weigth zero ---------------------------------------
df_model <- df |> filter(PERWT != 0) # n 13 --> Mexico 1995

df <- df_model 

## set reference categories 
df$DECADE <- as.factor(df$DECADE)
df$COUNTRY2 <- as.factor(df$COUNTRY2)
df$NCHILD3 <- as.factor(df$NCHILD3)
df$DECADE <- relevel(df$DECADE, ref = "2000s")
df$AGE22 <- relevel(df$AGE22, ref = "25-29")
df$YNGCH2 <- relevel(df$YNGCH2, ref = "0-4")
df$EDATTAIN2 <- relevel(df$EDATTAIN2, ref = "Primary or less")
df$MARST2 <- relevel(df$MARST2, ref = "Never married")
df$URBAN2 <- relevel(df$URBAN2, ref = "Urban")
df$NCHILD3 <- relevel(df$NCHILD3, ref = "1")
df$EMPSTAT3 <- relevel(df$EMPSTAT3, ref = "Employed")

# df <- df |> 
#     mutate(
#         LA.nuc = case_when(
#             LA %in% c('Extended with parents', 'Extended without parents') ~ 1,
#             LA == 'Only with children' ~ 0,
#             TRUE ~ NA),
#         
#         LA.ext = case_when(
#             LA == 'Extended without parents' ~ 0,
#             LA == 'Extended with parents' ~ 1,
#             TRUE ~ NA
#         ))
# 

# Ivars <- c('SERIAL', 'PERWT','LA.nuc','LA.ext','COUNTRY2', 'DECADE', 'SAMPLE2', 
#     'AGE22', 'NCHILD3', 'YNGCH2', 'URBAN2', 'MARST2', 'EDATTAIN2', 
#     'EMPSTAT3')
# 
# df <- df |> select(all_of(Ivars))
# 
# df_model <- df

# save(df_model, file="00_data\\DATA_FINAL_MODEL_OCTOBER.RData") # last version 08.10.25
# ----------------------------------------

## MODELS
df_model <- df
df_model$MARST2 <- relevel(df_model$MARST2, ref = "Never married")

df_model$MARST2 <- factor(df_model$MARST2,
    levels = c("Never married",
        "Sep/Div/Wid","Partner absent"))

# survey design with clustered SE at the household level
sd<-svydesign(id = ~SERIAL, weights=~PERWT, data=df_model); sd

## MODEL 1 - NUCLEAR VS EXTENDED ALL -------------------------------------------

# Country fixed effects included to account for between-country differences; 
# results omitted for brevity.

# SAMPLE YEAR, EDUCATION, MARITAL STATUS 
m1 <- svyglm(LA.nuc ~ DECADE + COUNTRY2 + EDATTAIN2 + MARST2, 
    design = sd,
    family=quasibinomial(),
    na.action = na.omit)

# summary(m1)


# FULL ONE ----------------------------------------------------
m2 <- svyglm(LA.nuc ~ DECADE + COUNTRY2 +  EDATTAIN2 + MARST2 + AGE22 + URBAN2+
        NCHILD3 + YNGCH2 + EMPSTAT3, 
    design = sd,
    family=quasibinomial(), 
    na.action = na.omit)

# summary(m2)

nobs(m1)

# check whether interactions contribute
# anova(m3, m4, test = "F")

## ADD PARAMETER (COPY THIS TO EXCEL)
# Model Fit: Deviance & DF
model_list <- list(m1 = m1, m2 = m2)

options(scipen = 100)
parameters <- as.data.frame(
    sapply(model_list, function(m) c(
    N = nobs(m),
    Deviance = deviance(m),
    PseudoR2 = 1 - (as.numeric(logLik(m)) / as.numeric(logLik(update(m, . ~ 1))))
)))

parameters <- t(parameters) |> as.data.frame()
parameters

# Parameter	Purpose	Notes
# N (Obs)	Shows data size	Helps contextualize confidence and missing data impact
# Deviance	Goodness-of-fit	Lower = better fit (already calculated)
# Pseudo R²	Overall explanatory power	Use McFadden’s R² (you computed this)
# Log-Likelihood	For comparing nested models	Already extracted with logLik()
# AIC/BIC	Penalize complexity	Not meaningful for quasibinomial, but can be approximated with caution


# notes
# Deviance drops with each step → better model fit.

# Pseudo R² improves substantially in m3 → a good jump in explanatory power.

# m4 shows tiny improvement (ΔR² ≈ 0.00014) — likely not meaningful practically.


# EXTRACT COEFFICIENT TO MAKE A TABLE 

# MODEL 1 -----------
# Extract coefficients, standard errors, and p-values model2 
coef_summary <- summary(m1)$coefficients
coefficients <- coef_summary[, "Estimate"]
standard_errors <- coef_summary[, "Std. Error"]
p_values <- coef_summary[, "Pr(>|t|)"]

# Calculate Odds Ratios (ORs) and 95% Confidence Intervals (CIs) model2
odds_ratios <- exp(coefficients)
conf_int_lower  <- exp(coefficients - 1.96 * standard_errors)
conf_int_upper  <- exp(coefficients + 1.96 * standard_errors)

# Create a data frame with coefficients, odds ratios, CIs, and p-values model2
results_m1 <- data.frame(
    category = rownames(coef_summary),
    Coefficient = coefficients,
    Odds_Ratio = odds_ratios,
    CI_Lower = conf_int_lower,
    CI_Upper = conf_int_upper,
    P_Value = p_values
)
results_m1[, -1] <- round(results_m1[, -1], 2)



# MODEL 2 -----------
# Extract coefficients, standard errors, and p-values model2 
coef_summary <- summary(m2)$coefficients
coefficients <- coef_summary[, "Estimate"]
standard_errors <- coef_summary[, "Std. Error"]
p_values <- coef_summary[, "Pr(>|t|)"]

# Calculate Odds Ratios (ORs) and 95% Confidence Intervals (CIs) model2
odds_ratios <- exp(coefficients)
conf_int_lower  <- exp(coefficients - 1.96 * standard_errors)
conf_int_upper  <- exp(coefficients + 1.96 * standard_errors)

# Create a data frame with coefficients, odds ratios, CIs, and p-values model2
results_m2 <- data.frame(
    category = rownames(coef_summary),
    Coefficient = coefficients,
    Odds_Ratio = odds_ratios,
    CI_Lower = conf_int_lower,
    CI_Upper = conf_int_upper,
    P_Value = p_values
)
results_m2[, -1] <- round(results_m2[, -1], 2)

head(results_m2)

# write a list and export in excel 

# list and write xlss
# list_of_datasets <- list("M1" = results_m1, "M2"= results_m2, 'PARAMETERS' = parameters) 
# 
# write.xlsx(list_of_datasets, file = '03_tables//MODEL1_AUGUST.xlsx')
# -----------------------------------------------------------------------------

## MODEL 2 - EXTENDED WITH PARENTS VS EXTENDED WITHOUT PARENTS -------------------

# Country fixed effects included to account for between-country differences; 
# results omitted for brevity.

# SAMPLE YEAR
m12 <- svyglm(LA.ext ~ DECADE + COUNTRY2 +  EDATTAIN2 + MARST2, 
    design = sd,
    family=quasibinomial(),
    na.action = na.omit)

# summary(m12)

# EDUCATION
m22 <- svyglm(LA.ext ~ DECADE + COUNTRY2 +  EDATTAIN2 + MARST2 + AGE22 + URBAN2+
        NCHILD3 + YNGCH2 + EMPSTAT3,
    design = sd,
    family=quasibinomial(),
    na.action = na.omit)

# summary(m22)


# check whether interactions contribute
# anova(m3, m4, test = "F")


## ADD PARAMETER (COPY THIS TO EXCEL)
# Model Fit: Deviance & DF
model_list <- list(m12 = m12, m22 = m22)

options(scipen = 100)
parameters2 <- as.data.frame(
    sapply(model_list, function(m) c(
        N = nobs(m),
        Deviance = deviance(m),
        PseudoR2 = 1 - (as.numeric(logLik(m)) / as.numeric(logLik(update(m, . ~ 1))))
    )))

parameters2 <- t(parameters2) |> as.data.frame()
parameters2

# Parameter	Purpose	Notes
# N (Obs)	Shows data size	Helps contextualize confidence and missing data impact
# Deviance	Goodness-of-fit	Lower = better fit (already calculated)
# Pseudo R²	Overall explanatory power	Use McFadden’s R² (you computed this)
# Log-Likelihood	For comparing nested models	Already extracted with logLik()
# AIC/BIC	Penalize complexity	Not meaningful for quasibinomial, but can be approximated with caution


# notes
# Deviance drops with each step → better model fit.

# Pseudo R² improves substantially in m3 → a good jump in explanatory power.

# m4 shows tiny improvement (ΔR² ≈ 0.00014) — likely not meaningful practically.


# EXTRACT COEFFICIENT TO MAKE A TABLE 

# MODEL 1 -----------
# Extract coefficients, standard errors, and p-values model2 
coef_summary <- summary(m12)$coefficients
coefficients <- coef_summary[, "Estimate"]
standard_errors <- coef_summary[, "Std. Error"]
p_values <- coef_summary[, "Pr(>|t|)"]

# Calculate Odds Ratios (ORs) and 95% Confidence Intervals (CIs) model2
odds_ratios <- exp(coefficients)
conf_int_lower  <- exp(coefficients - 1.96 * standard_errors)
conf_int_upper  <- exp(coefficients + 1.96 * standard_errors)

# Create a data frame with coefficients, odds ratios, CIs, and p-values model2
results_m12 <- data.frame(
    category = rownames(coef_summary),
    Coefficient = coefficients,
    Odds_Ratio = odds_ratios,
    CI_Lower = conf_int_lower,
    CI_Upper = conf_int_upper,
    P_Value = p_values
)
results_m12[, -1] <- round(results_m12[, -1], 2)

head(results_m12)

# MODEL 2 -----------
# Extract coefficients, standard errors, and p-values model2 
coef_summary <- summary(m22)$coefficients
coefficients <- coef_summary[, "Estimate"]
standard_errors <- coef_summary[, "Std. Error"]
p_values <- coef_summary[, "Pr(>|t|)"]

# Calculate Odds Ratios (ORs) and 95% Confidence Intervals (CIs) model2
odds_ratios <- exp(coefficients)
conf_int_lower  <- exp(coefficients - 1.96 * standard_errors)
conf_int_upper  <- exp(coefficients + 1.96 * standard_errors)

# Create a data frame with coefficients, odds ratios, CIs, and p-values model2
results_m22 <- data.frame(
    category = rownames(coef_summary),
    Coefficient = coefficients,
    Odds_Ratio = odds_ratios,
    CI_Lower = conf_int_lower,
    CI_Upper = conf_int_upper,
    P_Value = p_values
)
results_m22[, -1] <- round(results_m22[, -1], 2)

head(results_m22)

# write a list and export in excel 

# list and write xlss
list_of_datasets <- list("M1" = results_m1, "M2"= results_m2,
    "M12" = results_m12, "M22"= results_m22, 'PARAMETERS' = parameters, 'PARAMETERS2' = parameters2)


write.xlsx(list_of_datasets, file = '03_tables//MODELS_NOVEMBER_ALL.xlsx')
# -----------------------------------------------------------------------------


