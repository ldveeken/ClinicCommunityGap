## Ratio false positive diagnoses per one true positive diagnosis using 
## Author: L.D. Veeken
## RScript: 01_Ratio_FPtoTP.R
## Last updated: 6 January 2025
#| Data informing Supplementary Table S5

# Packages ====
library(dplyr)

#1. Ratios FP:TP ====
#1.1 Data from studies
data <- data.frame(
  Country = c("Bangladesh", "Kenya", "Philippines", "Viet Nam", 
              "Treats ZMB", "Treats ZMB", 
              "Treats ZAF", "Treats ZAF", "South Africa", "South Africa", "Lesotho", "Lesotho"), 
  Year    = c("2015-2016", "2015-2016", "2016", "2017-2018",
              "2017-2021", "2017-2021","2017-2021", "2017-2021", "2017-2019", "2017-2019", "2019", "2019"),
  Xpert = c("Xpert MTB/RIF", "Xpert MTB/RIF", "Xpert MTB/RIF", "Xpert MTB/RIF", 
            "Xpert Ultra - trace as pos", "Xpert Ultra - trace as neg", "Xpert Ultra - trace as pos", "Xpert Ultra - trace as neg", 
            "Xpert Ultra - trace as pos", "Xpert Ultra - trace as neg", "Xpert Ultra - trace as pos", "Xpert Ultra - trace as neg"),
  Screen_positive = c(20594, 9715, 18597, 4738, 1587, 1587, 491, 491, 9066, 9066, 7584, 7584), 
  TP = c(132, 152, 159, 130, 36, 34, 19, 12, 170, 144, 86, 72),
  FP = c(137, 81, 228, 74, 14, 8, 15, 9, 104, 66, 121, 66),
  TN = c(20134, 8045, 15275, 3926,  626, 632, 291, 297, 6567, 6605, 6369, 6424),
  FN = c(22, 69, 73, 60, 5, 7, 3, 10, 48, 74, 22, 36)
)

#1.2 Add columns with prevalence, FP:TP, % FP of all Xpert positive
## PS = as found in prevalence survey studies - 1.1 above
## CE = based on PS_Prev_CultPosTB and clinic-estimates (Zifodya et al.2021)

#1.2.1 Values Zifodya et al. 
sensitivity_Xpert_MTB_RIF <- 0.847  
specificity_Xpert_MTB_RIF <- 0.984 

sensitivity_Xpert_Ultra <- 0.909  
specificity_Xpert_Ultra <- 0.956  


#1.2.2 Study data
data <- data %>%
  mutate(
    # Calculate based on study data
    PS_Prev_CultPosTB   = ((TP + FN) / Screen_positive) * 100,
    
    PS_Ratio_FP_to_TP   = round(FP / TP, 2),
    PS_Prop_FP_XpertPos = round(FP/(TP+FP)*100, 0), 
  )

#1.2.3 Clinic-based estimations applied to study prevalence
data <- data %>%
  mutate(
    # Calculate CE_FP: False Positives based on clinic values
    CE_FP = ifelse(Xpert == "Xpert Ultra - trace as neg" | Xpert == "Xpert MTB/RIF - trace as neg", 
                   NA,  # If "trace as neg", set to NA
                   (1000 - (1000 * (PS_Prev_CultPosTB / 100))) * 
                           (ifelse(Xpert == "Xpert MTB/RIF", 1 - specificity_Xpert_MTB_RIF, 1 - specificity_Xpert_Ultra))), 
    
    # Calculate CE_TP: True Positives based on clinic values
    CE_TP = ifelse(Xpert == "Xpert Ultra - trace as neg" | Xpert == "Xpert MTB/RIF - trace as neg", 
                   NA,  # If "trace as neg", set to NA
                   1000 * (PS_Prev_CultPosTB / 100) * 
                           (ifelse(Xpert == "Xpert MTB/RIF", sensitivity_Xpert_MTB_RIF, sensitivity_Xpert_Ultra))),
    
    # Calculate CE_Ratio_FP_to_TP and CE_Prop_FP_XpertPos based on the CE_FP and CE_TP values
    CE_Ratio_FP_to_TP = round(CE_FP / CE_TP, 2),                   # Ratio of false positives to true positives
    CE_Prop_FP_XpertPos = round(CE_FP / (CE_TP + CE_FP) * 100, 0)  # Proportion of false positives to total Xpert positives (TP + FP)
    
  )

View(data)
