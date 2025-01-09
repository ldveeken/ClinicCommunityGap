## Sensitivity and specificty of prevalence surveys  
## Author: L.D. Veeken
## RScript: 00_SensSpec_PrevSurveys.R
## Last updated: 8 Jan 2025
#| Data informing Supplementary Table S4


# Packages ====
library(dplyr)
library(Hmisc)
library(tidyr)
library(mada)  #https://cran.r-project.org/web/packages/mada/vignettes/mada.pdf

#1. Sensitivity and specificity per study ====
#1.1 Data from studies ====

data <- data.frame(
  Country = c("Bangladesh", "Kenya", "Philippines", "Viet Nam", 
              "Treats ZMB", "Treats ZMB", 
              "Treats ZAF", "Treats ZAF", "South Africa", "South Africa", "Lesotho", "Lesotho"), 
  Year    = c("2015-2016", "2015-2016", "2016", "2017-2018",
               "2017-2021", "2017-2021","2017-2021", "2017-2021", "2017-2019", "2017-2019", "2019", "2019"),
  Xpert   = c("Xpert MTB/RIF", "Xpert MTB/RIF", "Xpert MTB/RIF", "Xpert MTB/RIF", 
           "Xpert Ultra - trace as pos", "Xpert Ultra - trace as neg", "Xpert Ultra - trace as pos", "Xpert Ultra - trace as neg", 
           "Xpert Ultra - trace as pos", "Xpert Ultra - trace as neg", "Xpert Ultra - trace as pos", "Xpert Ultra - trace as neg"),
  TP = c(132, 152, 159, 130, 36, 34, 19, 12, 170, 144, 86, 72),
  FP = c(137, 81, 228, 74, 14, 8, 15, 9, 104, 66, 121, 66),
  TN = c(20134, 8045, 15275, 3926,  626, 632, 291, 297, 6567, 6605, 6369, 6424),
  FN = c(22, 69, 73, 60, 5, 7, 3, 10, 48, 74, 22, 36)
)

#1.2 Calculate specificity, sensitivity, and their 95% CI ====

calculate_metrics <- function(TP, FP, TN, FN) {
  # Initialize outputs
  sensitivity <- specificity <- NA
  sensitivity_ci <- specificity_ci <- c(NA, NA, NA)
  
  # Calculate specificity and 95% CI if TN and FP are available
  if (!is.na(TN) && !is.na(FP)) {
    specificity <- TN / (TN + FP)
    specificity_ci <- binconf(TN, TN + FP, method = "wilson")
  }
  
  # Calculate sensitivity and 95% CI if TP and FN are available
  if (!is.na(TP) && !is.na(FN)) {
    sensitivity <- TP / (TP + FN)
    sensitivity_ci <- binconf(TP, TP + FN, method = "wilson")
  }
  
  # Return metrics as a named list
  list(
    Specificity = round(specificity, 3),
    Spec_LB = round(specificity_ci[2], 3),
    Spec_UB = round(specificity_ci[3],3),
    Sensitivity = round(sensitivity, 3),
    Sens_LB = round(sensitivity_ci[2], 3),
    Sens_UP = round(sensitivity_ci[3], 3)
  )
}

# Apply the function to each row and extract the metrics
data <- data %>%
  rowwise() %>%
  mutate(Metrics = list(calculate_metrics(TP, FP, TN, FN))) %>%
  ungroup()

#1.3 Combine results

data <- data %>%
  mutate(
    Specificity = sapply(Metrics, `[[`, "Specificity"),
    Spec_LB = sapply(Metrics, `[[`, "Spec_LB"),
    Spec_UB = sapply(Metrics, `[[`, "Spec_UB"),
    Sensitivity = sapply(Metrics, `[[`, "Sensitivity"),
    Sens_LB = sapply(Metrics, `[[`, "Sens_LB"),
    Sens_UP = sapply(Metrics, `[[`, "Sens_UP")
  
  ) %>%
  select(-Metrics) # Drop the list column

#View final dataset
View(data)

#2. Pooled sensitivity and specificity using mada package (Wilson 95%CI interval as default) ####

# List to store results
pooled_results <- list()

# Loop over unique Xpert groups
for (xpert_group in unique(data$Xpert)) {
  # Subset data for the current Xpert group
  subset_data <- data %>%
    filter(Xpert == xpert_group) %>%
    select(TP, FP, FN, TN)
  
  # Fit the Reitsma model
  fit <- reitsma(subset_data)
  fitsum <- summary(fit)[["coefficients"]]
  
  # Calculate pooled sensitivity and specificity
  senspool <- c(sens = round(fitsum["sensitivity", "Estimate"], 3),
                lo = round(fitsum["sensitivity", "95%ci.lb"], 3), 
                hi = round(fitsum["sensitivity", "95%ci.ub"],3))
  
  specpool <- c(spec = round(1 - fitsum["false pos. rate", "Estimate"],3),
                lo = round(1 - fitsum["false pos. rate", "95%ci.ub"],3), 
                hi = round(1 - fitsum["false pos. rate", "95%ci.lb"],3))
  
  # Store results
  pooled_results[[xpert_group]] <- list(
    Sensitivity = senspool,
    Specificity = specpool
  )
}

# Print results
for (xpert_group in names(pooled_results)) {
  cat("\nResults for:", xpert_group, "\n")
  print(pooled_results[[xpert_group]])
}


