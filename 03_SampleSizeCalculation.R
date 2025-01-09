## Sample size calculation
## Author: L.D. Veeken
## RScript: 03_SampleSizeCalculation.R
## Last updated: 6 January 2025
#| Data informing Supplementary Table S6 & S7

#| Sample size calculation for sensitivity specificity with a confidence level of (100-α) = 95% 
#| and an estimated prevalence of infectious tuberculosis disease of 0.5%


# Packages ====
library(dplyr)
library(epiR)  #https://search.r-project.org/CRAN/refmans/epiR/html/epi.ssdxsesp.html

# 1. Sample size - specificity ====
# Define ranges for parameters
spec_values    <- seq(0.994, 0.999, by = 0.001) # Specificity values    
CIwidth_values <- seq(0.001, 0.003, by = 0.001) # 95% CI width
Py             <- 0.005                         # Prevalence culture-positive tuberculosis (0.5%/100 = 0.005)
conf_level     <- 0.95                          # confidence level 

# Empty data frame to store SS_spec
SS_spec <- data.frame(
  Test = numeric(0),
  Epsilon = numeric(0),
  Result = numeric(0)
)

# Nested loop to iterate through specificity and 95% CI width values
for (test in spec_values) {
  for (epsilon in CIwidth_values) {
      result <- epi.ssdxsesp(
      test = test,
      type = "sp",
      Py = Py,
      epsilon = epsilon,
      error = "absolute",
      nfractional = FALSE,
      conf.level = conf_level
    )
    
    # Append the results to the data frame
    SS_spec <- rbind(
      SS_spec,
      data.frame(Specificity = test, CI_width = epsilon, SampleSize = result)
    )
  }
}

# View
SS_spec <- SS_spec %>%
  arrange(CI_width)
View(SS_spec)

# 2. Sample size sensitivity ====

# Define ranges for parameters
sens_values    <- seq(0.6, 0.9, by = 0.05)   # Sensitivity values
CIwidth_values <- c(0.001, 0.005, 0.01, 0.02, 0.05)
#CIwidth_values <- seq(0.01, 0.05, by = 0.01) # 95% CI width values
Py             <- 0.005                      # Prevalence culture-positive tuberculosis (0.5%/100 = 0.005)
conf_level     <- 0.95                       # Confidence level

# Empty data frame to store results
SS_sens <- data.frame(
  Test = numeric(0),
  Epsilon = numeric(0),
  Result = numeric(0)
)

# Nested loop to iterate through sensitivity and 95% CI width values
for (test in sens_values) {
  for (epsilon in CIwidth_values) {
     result <- epi.ssdxsesp(
      test = test,
      type = "se",
      Py = Py,
      epsilon = epsilon,
      error = "absolute",
      nfractional = FALSE,
      conf.level = conf_level
    )
    
    # Append the results to the data frame
    SS_sens <- rbind(
      SS_sens,
      data.frame(Sensitivity = test, CI_width = epsilon, SampleSize = result)
    )
  }
}

# View
SS_sens <- SS_sens %>%
  arrange(CI_width)
View(SS_sens)


