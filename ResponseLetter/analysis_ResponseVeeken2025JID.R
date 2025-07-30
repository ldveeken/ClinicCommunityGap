## Analysis code for Author's response to “TB prevalence in people tested is a 
   # strong predictor of Xpert specificity in community and risk group screening” 
## Last updated by Lara Veeken (30 July 2025)

# 0. Packages ====
library(ggplot2)
library(here)
library(data.table)
library(rio)

# 1. Data input====
d  <- as.data.table(import(here("data_ResponseVeeken2025JID.xlsx")))

# 1.1 Create plot grouping variable
d$which_plot <- ifelse(d$Test == "UltraTracePos", "second", "first")

# 2. Calculate correlations for each group ====
cor_tracepos <- cor(d[d$Test=="UltraTracePos","Prevalence"], 
                 d[d$Test=="UltraTracePos","Specificity"])

cor_traceneg <- cor(d[d$Test!="UltraTracePos","Prevalence"], 
                 d[d$Test!="UltraTracePos","Specificity"])

# 3. Calculate regression coefficients for each group ====
lm_tracepos <- lm(Specificity ~ Prevalence, data = d[d$Test=="UltraTracePos", ])
lm_traceneg <- lm(Specificity ~ Prevalence, data = d[d$Test!="UltraTracePos", ])

# Extract coefficients
intercept_tracepos <- round(coef(lm_tracepos)[1], 7)
slope_tracepos     <- round(coef(lm_tracepos)[2], 8)
intercept_traceneg <- round(coef(lm_traceneg)[1], 7)
slope_traceneg     <- round(coef(lm_traceneg)[2], 8)

# 4. Create plot ====
plot <- ggplot(data = d) +
  theme_bw() +
  geom_smooth(aes(x = Prevalence, y = Specificity), method = "lm", 
              show.legend = FALSE, colour = "grey60") +
  geom_point(aes(x = Prevalence, y = Specificity, colour = Test), size = 2.5) +
  geom_errorbar(aes(x = Prevalence, ymin = Min, ymax = Max), width = .2) +
  scale_x_continuous(name = "Estimated culture-positive TB prevalence per 100,000") +
  scale_y_continuous(name = "Estimated Xpert specificity against culture") +
  coord_cartesian(ylim = c(0.92, 1)) +
  scale_color_manual(values = c("blue", "red", "green4"),
                     name = NULL,
                     labels = c("Xpert MTB/RIF", "Xpert Ultra\n(trace as negative)", "Xpert Ultra\n(trace as positive)")) +
  facet_wrap(vars(which_plot), ncol = 2, labeller = labeller(which_plot = c(
    "first" = "Algorithm 1: Xpert MTB/RIF and Xpert Ultra (trace as negative)",
    "second" = "Algorithm 2: Xpert Ultra (trace as positive)"
  ))) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.height = unit(2, "lines"))

dat_text <- data.frame(                     
  label = c(
    paste("r =", round(cor_traceneg, 2)),
    paste("r =", round(cor_tracepos, 2))
  ),
  which_plot = c("first", "second"),
  x = c(600, 600),
  y = c(0.93, 0.93)
)

dat_formula <- data.frame(
  label = c(
    paste("y =", intercept_traceneg, ifelse(slope_traceneg >= 0, "+", ""), slope_traceneg, "x"),
    paste("y =", intercept_tracepos, ifelse(slope_tracepos >= 0, "+", ""), slope_tracepos, "x")
  ),
  which_plot = c("first", "second"),
  x = c(1000, 1000),
  y = c(0.925, 0.925)
)

plot <- plot + 
  geom_text(data = dat_text, mapping = aes(x = x, y = y, label = label), size = 4) +
  geom_text(data = dat_formula, mapping = aes(x = x, y = y, label = label), size = 4)

print(plot)

# ggsave("figure1.tiff", plot = plot,
#        width = 14, height = 6, dpi = 600, compression = "none")

# 5. Calculate spec estimates based on lm ==== 
prevalences <- c(500, 250, 100, 50)

cat("\nSpec estimates algorithm 1:\n")
for(prev in prevalences) {
  spec <- intercept_traceneg + slope_traceneg * prev
  cat(sprintf("Prevalence %d: Specificity = %.3f\n", prev, spec))
}

cat("Spec estimates algorithm 2:\n")
for(prev in prevalences) {
  spec <- intercept_tracepos + slope_tracepos * prev
  cat(sprintf("Prevalence %d: Specificity = %.3f\n", prev, spec))
}


