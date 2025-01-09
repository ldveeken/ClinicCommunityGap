## Figure 1
## Author: L.D. Veeken
## RScript: 02_Figure1.R
## Last updated: 8 January 2025
#| Data informing Figure 1

# Packages ====
library(dplyr)
library(ggplot2)
library(ggh4x)  

# Figure 1 - FP:TP ratios ####
## Using estimated specificity and sensitivity applied to culture-positive tuberculosis prevalence of 500/100,000 (0.5%)

#1. Clinic-data (Zifodya et al.2021) ====
# Define sensitivity and specificity for Xpert MTB/RIF and Xpert Ultra
sensitivity_Xpert_MTB_RIF <- 0.847  
specificity_Xpert_MTB_RIF <- 0.984  

sensitivity_Xpert_Ultra <- 0.909  
specificity_Xpert_Ultra <- 0.956 

# Total population and prevalence
N <- 1000            # Total population
prevalence <- 0.005  # Culture-positive tuberculosis prevalence = 0.5%

# Create a dataset and calculate results
ClinicData <- data.frame(
  Test = c("Xpert MTB/RIF", "Xpert Ultra"),
  Sensitivity = c(sensitivity_Xpert_MTB_RIF, sensitivity_Xpert_Ultra),
  Specificity = c(specificity_Xpert_MTB_RIF, specificity_Xpert_Ultra)
) %>%
  mutate(
    Expected_TP = N * prevalence * Sensitivity,
    Expected_FP = N * (1 - prevalence) * (1 - Specificity),
    FP_to_TP = round(ifelse(Expected_TP > 0, Expected_FP / Expected_TP, NA), 1), 
    Prop_FP_XpertPos = round(Expected_FP/(Expected_TP+Expected_FP)*100, 0)
  )

# View the dataset
View(ClinicData)

#2. Study-data: prevalence surveys ====
#2.1.1  Data from prevalence studies ====

StudyData_P <- data.frame(
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

StudyData_P <- StudyData_P %>%
  arrange(Xpert)

#2.1.2 Calculate specificity, sensitivity ====

  calculate_metrics <- function(TP, FP, TN, FN) {
    sensitivity <- ifelse(!is.na(TP) && !is.na(FN), TP / (TP + FN), NA)
    specificity <- ifelse(!is.na(TN) && !is.na(FP), TN / (TN + FP), NA)
    
    list(
      Sensitivity = sensitivity,
      Specificity = specificity
    )
  }
  
  # Add specificity and sensitivity to the dataset
  StudyData_P <- StudyData_P %>%
    rowwise() %>%
    mutate(Metrics = list(calculate_metrics(TP, FP, TN, FN))) %>%
    ungroup() %>%
    mutate(
      Specificity = sapply(Metrics, `[[`, "Specificity"),
      Sensitivity = sapply(Metrics, `[[`, "Sensitivity")
    ) %>%
    select(-Metrics)
  
  # Add population-based calculations
  N <- 1000            # Total population
  prevalence <- 0.005  # Prevalence = 0.5%
  
  StudyData_P <- StudyData_P %>%
    mutate(
      Expected_TP = N * prevalence * Sensitivity,
      Expected_FP = N * (1 - prevalence) * (1 - Specificity),
      FP_to_TP = round(ifelse(Expected_TP > 0, Expected_FP / Expected_TP, NA),1),
      Prop_FP_XpertPos = round(Expected_FP/(Expected_TP+Expected_FP)*100, 0)
    )
  
  # View the final dataset
  View(StudyData_P)
  
#3. Study data - community studies ====
  # Specificty = calculated in study with trace as positive & inferring culture status (Supplementary Table S3)
  # Sensitivity = pooled sensitivity prevalence surveys (00_SensSpec_PrevSurveys.R & Supplementary Table s3)
  
  
  # Define sensitivity and specificity for Xpert MTB/RIF and Xpert Ultra
  sensitivity_Xpert_MTB_RIF <- 0.732   
  specificity_Xpert_MTB_RIF <- 0.998  
  
  sensitivity_Xpert_Ultra_traceNEG <- 0.653  
  specificity_Xpert_Ultra_traceNEG <- 0.999
  
  sensitivity_Xpert_Ultra_tracePOS <- 0.825  
  specificity_Xpert_Ultra_tracePOS <- 0.994  
  
  # Total population and prevalence
  N <- 1000            # Total population
  prevalence <- 0.005  # Prevalence = 0.5%
  
  # Create a dataset and calculate results
  StudyData_C <- data.frame(
    Test = c("Xpert MTB/RIF", "Xpert Ultra (trace as neg)", "Xpert Ultra (trace as pos)"),
    Sensitivity = c(sensitivity_Xpert_MTB_RIF, sensitivity_Xpert_Ultra_traceNEG, sensitivity_Xpert_Ultra_tracePOS),
    Specificity = c(specificity_Xpert_MTB_RIF, specificity_Xpert_Ultra_traceNEG, specificity_Xpert_Ultra_tracePOS)
  ) %>%
    mutate(
      Expected_TP = N * prevalence * Sensitivity,
      Expected_FP = N * (1 - prevalence) * (1 - Specificity),
      FP_to_TP = round(ifelse(Expected_TP > 0, Expected_FP / Expected_TP, NA), 1), 
      Prop_FP_XpertPos = round(Expected_FP/(Expected_TP+Expected_FP)*100, 0)
    )
  
  View(StudyData_C)
  
  
# Figure 1 - specificty plot ####
#1.Study data (data from "00_SensSpec_PrevSurveys.R" & Supplementary Table S3) ====
  data <- data.frame(
    Study = c("Uganda (2019)", "Treats ZMB (2017-21)", "Treats ZAF (2017-21)", "South Africa (2017-2019)", "Lesotho (2019)", 
              "Uganda (2019)",  "Treats ZMB (2017-21)", "Treats ZAF (2017-21)", "South Africa (2017-2019)", "Lesotho (2019)", 
              "Vietnam (2014-15)", "Bangladesh (2015-16)", "Kenya (2015-16)", "Philippines (2016)", "Vietnam (2017-18)"),
    Specificity = c(0.994,  0.978, 0.951, 0.984, 0.981, 0.999, 0.988, 0.971, 0.990, 0.990, 0.998, 0.993, 0.990, 0.985, 0.982),
    Lower_CI    = c(0.993,  0.964, 0.921, 0.981, 0.978, 0.998, 0.976, 0.945, 0.987, 0.987, 0.998, 0.992, 0.988, 0.983, 0.977),
    Upper_CI    = c(0.995,  0.987, 0.970, 0.987, 0.984, 1.0,   0.994, 0.984, 0.992, 0.992, 0.999, 0.994, 0.992, 0.987, 0.985),
    Test = c(rep("Ultra (trace as positive)", 5), rep("Ultra (trace as negative)", 5), rep("MTB/RIF", 5)),
    Survey = c("Community", "Prevalence survey", "Prevalence survey", "Prevalence survey", "Prevalence survey", 
               "Community", "Prevalence survey", "Prevalence survey", "Prevalence survey", "Prevalence survey", 
               "Community", "Prevalence survey", "Prevalence survey", "Prevalence survey", "Prevalence survey")
  )
  
  
  # Create a new column for specificity and CI
  data <- data %>%
    mutate(Spec_CI = paste0(Specificity, " (", Lower_CI, "-", Upper_CI, ")")) 
  
  # Reorder factor levels for correct plotting
  data$Study <- factor(data$Study, levels = rev(unique(data$Study)))
  
  # Define custom colors for strip backgrounds
  strip_colors <- c(
    "Ultra (trace as pos)" = "#A0522D",  
    "Ultra (trace as neg)" = "#CA8C9FFF",  
    "MTB/RIF" = "#E5D8D6FF"              
  )
  
#2. Create the plot ====
  ggplot(data, aes(x = Specificity, y = Study, color = Survey)) +
    geom_point(size = 3) + # Larger points for visibility
    geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.3) + # Standard error bar
    
    # Add shaded area for specific ranges based on Test type without outlines
    geom_rect(data = subset(data, Test == "Ultra (trace as positive)"), aes(xmin = 0.930, xmax = 0.974, ymin = 0.5, ymax = 5.5), 
              alpha = 0.1, fill = "grey", color = NA) + # No outline for rectangles
    
    geom_rect(data = subset(data, Test == "MTB/RIF"), aes(xmin = 0.970, xmax = 0.993, ymin = 0.5, ymax = 5.5), 
              alpha = 0.1, fill = "grey", color = NA) + # No outline for rectangles
    
    # Add vertical dotted line at x = 0.984 only within the "MTB/RIF" shaded rectangle
    geom_segment(data = subset(data, Test == "MTB/RIF"), aes(x = 0.984, xend = 0.984, y = 0.5, yend = 5.5), 
                 linetype = "dotted", color = "grey30", size = 0.8) +
    
    # Add vertical dotted line at x = 0.956 only within the "Ultra (trace as positive)" shaded rectangle
    geom_segment(data = subset(data, Test == "Ultra (trace as positive)"), aes(x = 0.956, xend = 0.956, y = 0.5, yend = 5.5), 
                 linetype = "dotted", color = "grey30", size = 0.8) +
    
    # Add text in the upper left corner
    geom_text(data = subset(data, Test == "Ultra (trace as positive)"), aes(x = 0.930, y = 5.4, label = "Clinic-based data"), 
              vjust = 1.5, hjust = -0.1, size = 5.5, color = "#7A7A7A") +
    geom_text(data = subset(data, Test == "MTB/RIF"), aes(x = 0.970, y = 5.4, label = "Clinic-based data"), 
              vjust = 1.5, hjust = -0.1, size = 5.5, color = "#7A7A7A") +
    
    facet_grid2(Test ~ ., scales = "free_y", strip = strip_themed(
      background_y = list(
        "Ultra (trace as pos)" = element_rect(fill = "#E5D8D6FF", color = NA),  
        "Ultra (trace as neg)" = element_rect(fill = "#CA8C9FFF", color = NA),  
        "MTB/RIF" = element_rect(fill = "#A0522D", color = NA)               
      )
    )) +  
    theme_minimal(base_size = 12) + 
    labs(
      x = "Specificity (95% CI)", 
      y = NULL) + 
    theme(
      strip.text = element_text(size = 17, face = "bold", angle = 90, vjust = 1),  
      strip.placement = "outside", # Place the strip labels outside for clarity
      axis.text.y = element_text(size = 17, face = "bold", color = "black"), 
      axis.text.x = element_text(size = 15), 
      axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15)), 
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
      legend.title = element_blank(), 
      legend.position = "bottom", 
      legend.text = element_text(size = 17),
      panel.grid.major.x = element_line(color = "grey80", size = 0.5), 
      panel.grid.minor.x = element_blank(), 
      panel.grid.major.y = element_blank(), 
      panel.spacing = unit(2, "lines"), 
      plot.margin = margin(t = 10, r = 20, b = 10, l = 20)
    ) +
    scale_color_manual(values = c("Community" = "#3F7D8B", "Prevalence survey" = "#D7443FFF")) + 
    scale_x_continuous(breaks = seq(0.92, 1, by = 0.01), limits = c(0.92, 1.0)) + 
    # Add outlines around each facet
    geom_rect(data = data.frame(Test = unique(data$Test), xmin = 0.92, xmax = 1, ymin = 0, ymax = 6), 
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              color = strip_colors, fill = NA, size = 1, inherit.aes = FALSE)
  