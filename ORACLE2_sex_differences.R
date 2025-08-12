# This script contains the code for the sex-stratified analysis of ORACLE2
# Manuscript title: "Clinical risk factors and Type-2 biomarkers predict asthma attacks regardless of sex"

# 1: preparation ####
# Loading the required packages
library(MASS)
library(gridExtra)
library(readxl)
library(tableone)
library(xtable)
library(xlsx)
library(survival)
library(survminer)
library(dplyr)
library(scales)
library(ggplot2)
library(scales)
library(purrr)
library(tibble)
library(ggh4x)
library(ggtext)
library(tidyverse)

# Loading the imputed datasets with and without replacement of NA values
imp_data_ORACLE_final_COMP <- read.csv('data_ORACLE_COMP_20241122_joined.csv')
imp_data_ORACLE_final_COMP_NR <- read.csv('data_ORACLE_COMP_NR_20241122_joined.csv')

# For more information on these datasets, please check the original script "ORACLE2-IDA_and_analyses", starting from line 2717
# Code available at: https://github.com/flmeulmeester/ORACLE2/

# Make sure all variables have the correct class assigned
factors <- c(5,6,8,10:13,16,17,19:26,30,32:35,37:39)
characters <- 4
numeric <- c(1:3,7,9,14,15,18,27:29,31,36,40:59)

imp_data_ORACLE_final_COMP <- imp_data_ORACLE_final_COMP %>% mutate(across(all_of(factors), as.factor))
imp_data_ORACLE_final_COMP_NR <- imp_data_ORACLE_final_COMP_NR %>% mutate(across(all_of(factors), as.factor))

imp_data_ORACLE_final_COMP <- imp_data_ORACLE_final_COMP %>% mutate(across(all_of(characters), as.character))
imp_data_ORACLE_final_COMP_NR <- imp_data_ORACLE_final_COMP_NR %>% mutate(across(all_of(characters), as.character))

imp_data_ORACLE_final_COMP <- imp_data_ORACLE_final_COMP %>% mutate(across(all_of(numeric), as.numeric))
imp_data_ORACLE_final_COMP_NR <- imp_data_ORACLE_final_COMP_NR %>% mutate(across(all_of(numeric), as.numeric))

# Splitting the dataset based on sex
df_female <- subset(imp_data_ORACLE_final_COMP_NR, Gender_0Female_1Male == 0)
df_male   <- subset(imp_data_ORACLE_final_COMP_NR, Gender_0Female_1Male == 1)

# Loading the data (original, used for table 1)
demo <- read_xlsx(path = "29-07-2024 DATA mice completed + original/ORACLE_DATA_not_fleur_V23_2024-07-11_captainadherence and stratos fev1rev multiplied by 100.xlsx")

# Checking whether sex is reported for all 6513 patients
demo <- demo[demo$Included_in_dataset_6513==1,]
demo <- demo[!is.na(demo$Gender_0Female_1Male),]    # Sex missing for N=3

# Define colours for each sex
custom_colors2 <- c("Female" = "firebrick", "Male" = "deepskyblue3")


# 2: Table 1 baseline characteristics ####
# Replace 9999 with NA for categorical variables
categorical_vars <- c("Gender_0Female_1Male", "Smoking_0never_1ex_2current", "Ethnicity", 
                      "Atopy_history_0no_1yes_9999notknown", "Eczema_0no_1yes_9999notknown", 
                      "AllergicRhinitis__0no_1yes_9999notknown", 
                      "Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown", 
                      "Chronic_Rhinosinusitis_0no_1yes_9999notknown", 
                      "Nasal_polyposis_0no_1yes_9999notknown", 
                      "Psychiatric_disease_0no_1yes_9999notknown", 
                      "Treatment_step")
demo[categorical_vars] <- lapply(demo[categorical_vars], function(x) ifelse(x == 9999, NA, x))
demo[demo == 9999] <- NA

# Specify variables for TableOne
variables <- c("Age", "Gender_0Female_1Male", "BMI", "Smoking_0never_1ex_2current", 
               "Ethnicity", "Atopy_history_0no_1yes_9999notknown", 
               "Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown", 
               "Eczema_0no_1yes_9999notknown", "AllergicRhinitis__0no_1yes_9999notknown", 
               "Chronic_Rhinosinusitis_0no_1yes_9999notknown", 
               "Nasal_polyposis_0no_1yes_9999notknown", 
               "Psychiatric_disease_0no_1yes_9999notknown", "Treatment_step", 
               "ACQ_baseline_score_mean", "Any_severe_attack_previous_12m_0no_1yes", 
               "FEV1_postBD_PCT_Baseline", "FEV1_PCT_reversibility_postBD", 
               "FEV1_preBD_L_Baseline", "FEV1_postBD_L_Baseline", "FEV1_preBD_PCT_Baseline", 
               "FEV1/FVC_preBD_Baseline", "FEV1/FVC_postBD_Baseline", 
               "Blood_eosinophils", "FeNO", "Total_IgE")

# Define categorical variables
factor_vars <- c("Gender_0Female_1Male", "Smoking_0never_1ex_2current", "Ethnicity", 
                 "Atopy_history_0no_1yes_9999notknown", "Eczema_0no_1yes_9999notknown", 
                 "AllergicRhinitis__0no_1yes_9999notknown", 
                 "Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown", 
                 "Chronic_Rhinosinusitis_0no_1yes_9999notknown", 
                 "Nasal_polyposis_0no_1yes_9999notknown", 
                 "Psychiatric_disease_0no_1yes_9999notknown", "Treatment_step")


# Customize display for selected continuous variables
custom_cont_summary <- function(x) {
  if (length(na.omit(x)) == 0) {
    return("NA")
  }
  median_val <- median(x, na.rm = TRUE)
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  sprintf("%.1f (%.1f–%.1f)", median_val, q1, q3)
}

# Specify continuous variables to format as median (Q1–Q3)
continuous_vars_median_iqr <- c("Age", "BMI", "FEV1_preBD_PCT_Baseline", 
                                "FEV1_PCT_reversibility_postBD", 
                                "ACQ_baseline_score_mean", "Total_IgE")

# Initialize an empty list for custom summaries
summary_list <- list()

# Assign the custom summary function to each variable
for (var in continuous_vars_median_iqr) {
  summary_list[[var]] <- custom_cont_summary
}

# Sex-stratified analysis
table1_gender <- CreateTableOne(vars = variables, strata = "Gender_0Female_1Male", 
                                data = demo, factorVars = factor_vars, includeNA = TRUE)

table1_gender_custom <- print(table1_gender, nonnormal = continuous_vars_median_iqr, 
                              contDigits = 1, 
                              customSummary = summary_list, 
                              showAllLevels = TRUE)

# Export TableOne to Excel
write.xlsx(list("Overall" = as.data.frame(table1_custom),
                "Stratified by Gender" = as.data.frame(table1_gender_custom)), 
           file = "TableOne_Custom_Median_IQR_Gender.xlsx")


# 3: Table 1 descriptive statistics ####
# Age, BMI, FeNO, BEC
shapiro.test(demo$BMI[demo$Gender_0Female_1Male == 0])  # For females
shapiro.test(demo$BMI[demo$Gender_0Female_1Male == 1])  # For males

shapiro.test(demo$Age[demo$Gender_0Female_1Male == 0])  # For females
shapiro.test(demo$Age[demo$Gender_0Female_1Male == 1])  # For males

ggplot(demo, aes(x = BMI, fill = as.factor(Gender_0Female_1Male))) + 
  geom_density(alpha = 0.5) + ggtitle("BMI Density by Sex")
ggplot(demo, aes(x = Age, fill = as.factor(Gender_0Female_1Male))) + 
  geom_density(alpha = 0.5) + ggtitle("Age Density by Sex")
ggplot(demo, aes(x = FeNO_baseline_ppb, fill = as.factor(Gender_0Female_1Male))) + 
  geom_density(alpha = 0.5) + ggtitle("Age Density by Sex")
ggplot(demo, aes(x = Blood_Eos_baseline_x10_9_cells_per_L, fill = as.factor(Gender_0Female_1Male))) + 
  geom_density(alpha = 0.5) + ggtitle("Age Density by Sex")

choose_test <- function(variable) {
  p_female <- shapiro.test(demo[[variable]][demo$Gender_0Female_1Male == 0])$p.value
  p_male <- shapiro.test(demo[[variable]][demo$Gender_0Female_1Male == 1])$p.value
  
  if (p_female > 0.05 & p_male > 0.05) {
    return("Use t-test")
  } else {
    return("Use Wilcoxon test")
  }
}

choose_test("BMI")  
choose_test("Age")
choose_test("FeNO_baseline_ppb")
choose_test("Blood_Eos_baseline_x10_9_cells_per_L")
choose_test("FEV1_preBD_PCT_Baseline")
choose_test("FEV1FVC")
choose_test("FEV1_PCT_reversibility_postBD")

wilcox.test(BMI ~ Gender_0Female_1Male, data = demo)  
wilcox.test(Age ~ Gender_0Female_1Male, data = demo)
wilcox.test(FeNO_baseline_ppb ~ Gender_0Female_1Male, data = demo)
wilcox.test(Blood_Eos_baseline_x10_9_cells_per_L ~ Gender_0Female_1Male, data = demo)
wilcox.test(FEV1_preBD_PCT_Baseline ~ Gender_0Female_1Male, data = demo)
wilcox.test(FEV1FVC ~ Gender_0Female_1Male, data = demo)
wilcox.test(FEV1_PCT_reversibility_postBD ~ Gender_0Female_1Male, data = demo)


# Smoking, ethniciity, BMI category, treatment step
check_test_type <- function(var) {
  tbl <- table(demo[[var]], demo$Gender_0Female_1Male)  # Create a contingency table
  expected_counts <- chisq.test(tbl)$expected  # Get expected values
  
  if (any(expected_counts < 5)) {
    return("Use Fisher’s exact test")
  } else {
    return("Use Chi-square test")
  }
}

# Run for each categorical variable
check_test_type("BMI_Category")
check_test_type("Smoking_0never_1ex_2current")
check_test_type("Ethnicity")
check_test_type("Treatment_step")

chisq.test(table(demo$BMI_Category, demo$Gender_0Female_1Male))
chisq.test(table(demo$Smoking_0never_1ex_2current, demo$Gender_0Female_1Male))
fisher.test(table(demo$Ethnicity, demo$Gender_0Female_1Male))
chisq.test(table(demo$Treatment_step, demo$Gender_0Female_1Male))

# BMI
BMI_table <- table(demo$BMI_Category, demo$Gender_0Female_1Male)
chisq.test(BMI_table)

# Post-hoc: correct for multiple tests
chisq.test(BMI_table)$stdres
alpha = 0.05
alpha_adj <- alpha/(nrow(BMI_table)*ncol(BMI_table))
qnorm(alpha_adj/2)
# Significant differences in groups with |qnorm(alpha_adj/2)| < |chisq.test(BMI_table)$stdres|
# In this case, significant differences are found in 3 groups (overweight, Obesity class II and III)

# Smoking 
Smoking_table <- table(demo$Smoking_0never_1ex_2current, demo$Gender_0Female_1Male)
chisq.test(Smoking_table)

# Post-hoc smoking
chisq.test(Smoking_table)$stdres
alpha_adj <- alpha/(nrow(Smoking_table)*ncol(Smoking_table))
qnorm(alpha_adj/2)

# Ethnicity
Ethniciity_table <- table(demo$Ethnicity, demo$Gender_0Female_1Male)
chisq.test(Ethniciity_table)

# Post-hoc ethnicity
chisq.test(Ethniciity_table)$stdres
alpha_adj <- alpha/(4*ncol(Ethniciity_table))
qnorm(alpha_adj/2)

# Treatment step 
treatment_table <- table(demo$Treatment_step, demo$Gender_0Female_1Male)
chisq.test(treatment_table)

# Post-hoc smoking
chisq.test(treatment_table)$stdres
alpha_adj <- alpha/(nrow(treatment_table)*ncol(treatment_table))
qnorm(alpha_adj/2)

# mOCS
chisq.test(table(demo$Mainteance_OCS_dose, demo$Gender_0Female_1Male))

# Comorbidities
chisq.test(table(demo$Atopy_history_0no_1yes_9999notknown, demo$Gender_0Female_1Male))
chisq.test(table(demo$Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown, demo$Gender_0Female_1Male))
chisq.test(table(demo$Eczema_0no_1yes_9999notknown, demo$Gender_0Female_1Male))
chisq.test(table(demo$AllergicRhinitis__0no_1yes_9999notknown, demo$Gender_0Female_1Male))
chisq.test(table(demo$Chronic_Rhinosinusitis_0no_1yes_9999notknown, demo$Gender_0Female_1Male))
chisq.test(table(demo$Nasal_polyposis_0no_1yes_9999notknown, demo$Gender_0Female_1Male))
chisq.test(table(demo$Psychiatric_disease_0no_1yes_9999notknown, demo$Gender_0Female_1Male))


# 4: Figure 1 Time to first attack ####
# === UNADJUSTED ===
demo <- demo %>%
  mutate(
    event = ifelse(Number_severe_asthma_attacks_during_followup >= 1, 1, 0),
    time = ifelse(event == 1, Time_to_First_attack, Follow_up_duration_days),
    sex = factor(Gender_0Female_1Male, levels = c(0, 1), labels = c("Female", "Male"))
  )

demo$sex_label <- factor(demo1$sex, levels = c("Female", "Male"), labels = c("Female", "Male"))

# Create survival object
surv_obj <- Surv(time = demo$time, event = demo$event)

# Fit Kaplan-Meier survival curve by sex
fit_curve <- survfit(surv_obj ~ sex_label, data = demo)

# Fit Cox model for HR + p-value
cox_unadj <- coxph(surv_obj ~ sex_label, data = demo)
hr <- exp(coef(cox_unadj))
hr_ci <- exp(confint(cox_unadj))
pval <- signif(summary(cox_unadj)$wald["pvalue"], 3)
hr_label <- paste0(
  "HR = ", round(hr, 2), " (", 
  round(hr_ci[1], 2), "-", round(hr_ci[2], 2), 
  "), p = ", pval
)

g_unadj <- ggsurvplot(
  fit_curve,
  data = demo,
  fun = "event",
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  legend.title = "",
  legend.labs = c("Females", "Males"),
  xlab = "Time (days)",
  ylab = "Percentage of patients with a severe asthma attack",
  palette = c("red", "deepskyblue3"),
  ggtheme = theme_bw(),
  risk.table.height = 0.25,
  xlim = c(0, 365),
  ylim = c(0, 0.4)
)

# Convert to percent scale, add label
g_unadj$plot <- g_unadj$plot +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.4)) +
  annotate("text", x = 5, y = 0.39, label = hr_label, hjust = 0, size = 3.5) +
  annotate("text", x = 0, y = 0.42, label = "A", fontface = "bold", hjust = 0, size = 5)

# === ADJUSTED ===
# Fit adjusted Cox model
cox_model <- coxph(
  surv_obj ~ sex_label + Age + ACQ_baseline_score_mean + 
    Any_severe_attack_previous_12m_0no_1yes +
    FEV1_preBD_PCT_Baseline + Treatment_step + 
    FeNO_baseline_ppb + Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced + 
    Enrolled_Trial_name,
  data = demo
)

# Summarize model (for checking)
summary(cox_model)

# Create adjusted survival curves (for sex)
fit_adj <- survfit(cox_model, newdata = data.frame(
  sex_label = factor(c("Female", "Male"), levels = c("Female", "Male")),
  Age = mean(demo$Age, na.rm = TRUE),
  ACQ_baseline_score_mean = mean(demo$ACQ_baseline_score_mean, na.rm = TRUE),
  Any_severe_attack_previous_12m_0no_1yes = 0,  # or use mode
  FEV1_preBD_PCT_Baseline = mean(demo$FEV1_preBD_PCT_Baseline, na.rm = TRUE),
  Treatment_step = median(demo$Treatment_step, na.rm = TRUE),
  FeNO_baseline_ppb = mean(demo$FeNO_baseline_ppb, na.rm = TRUE),
  Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced = mean(demo$Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced, na.rm = TRUE),
  Enrolled_Trial_name = names(sort(table(demo$Enrolled_Trial_name), decreasing = TRUE))[1]  # most frequent trial
))

hr_adj <- exp(coef(cox_model)["sexMale"])
hr_ci_adj <- exp(confint(cox_model)["sexMale", ])
p_value_sex <- summary(cox_model)$coefficients["sexMale", "Pr(>|z|)"]
hr_label_adj <- paste0(
  "HR = ", round(hr_adj, 2), " (", 
  round(hr_ci_adj[1], 2), "-", round(hr_ci_adj[2], 2), 
  "), p =", p_value_sex
)

g_adj_plot <- ggsurvplot(
  fit_adj,
  data = demo,
  fun = "event",
  conf.int = TRUE,
  legend.title = "",
  legend.labs = c("Females", "Males"),
  xlab = "Time (days)",
  ylab = "Percentage of patients with a severe asthma attack",
  palette = c("red", "deepskyblue3"),
  ggtheme = theme_bw(),
  risk.table = FALSE,   
  tables.height = 0,
  xlim = c(0, 365),
  ylim = c(0, 0.4)
)

# Convert to percent scale, add label
g_adj_plot$plot <- g_adj_plot$plot +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.4)) +
  annotate("text", x = 5, y = 0.39, label = hr_label_adj, hjust = 0, size = 3.5) +
  annotate("text", x = 0, y = 0.42, label = "B", fontface = "bold", hjust = 0, size = 5)


# Combining plots 
fig1_final <- arrange_ggsurvplots(
  list(g_unadj, g_adj_plot),
  ncol = 1,
  nrow = 2,
  heights = c(1, 1), # equal height
  print = FALSE
)

png("Figure_1.png", units = "in", width = 6, height = 14, res = 600)
print(fig1_final)
dev.off()


# Add exact CI at 365 days to avoid confusing
# === Unadjusted: extract probabilities at 365 days ===
prob_365 <- summary(fit_curve, times = 365)$surv
ci_lower <- summary(fit_curve, times = 365)$lower
ci_upper <- summary(fit_curve, times = 365)$upper

# Convert to % and event probability (1 - survival)
event_365 <- 1 - prob_365
event_lower <- 1 - ci_upper  # note: CI for event is reversed from survival
event_upper <- 1 - ci_lower

# Create label: Female and Male with CI
label_unadj <- paste0(
  "Females: ", round(100 * event_365[1], 1), "% (",
  round(100 * event_lower[1], 1), "-", round(100 * event_upper[1], 1), "%)\n",
  "Males: ", round(100 * event_365[2], 1), "% (",
  round(100 * event_lower[2], 1), "-", round(100 * event_upper[2], 1), "%)"
)

# === Adjusted: extract probabilities at 365 days ===
prob_365_adj <- summary(fit_adj, times = 365)$surv
ci_lower_adj <- summary(fit_adj, times = 365)$lower
ci_upper_adj <- summary(fit_adj, times = 365)$upper

event_365_adj <- 1 - prob_365_adj
event_lower_adj <- 1 - ci_upper_adj
event_upper_adj <- 1 - ci_lower_adj

label_adj <- paste0(
  "Females: ", round(100 * event_365_adj[1], 1), "% (",
  round(100 * event_lower_adj[1], 1), "-", round(100 * event_upper_adj[1], 1), "%)\n",
  "Males: ", round(100 * event_365_adj[2], 1), "% (",
  round(100 * event_lower_adj[2], 1), "-", round(100 * event_upper_adj[2], 1), "%)"
)

label_unadj_full <- paste0(label_unadj, "\nHR = 0.77 (0.70–0.85), p < 0.001")
label_adj_full   <- paste0(label_adj,   "\nHR = 0.81 (0.73–0.90), p < 0.001")

# Final plots
g_unadj$plot <- g_unadj$plot +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.4)) +
  annotate("text", x = 5, y = 0.37, label = label_unadj_full, hjust = -0.25, size = 3.5) +
  annotate("text", x = 0, y = 0.39, label = "A", fontface = "bold", hjust = 0, size = 5)


g_adj_plot$plot <- g_adj_plot$plot +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.4)) +
  annotate("text", x = 5, y = 0.39, label = label_adj_full, hjust = -0.25, size = 3.5) +
  annotate("text", x = 0, y = 0.39, label = "B", fontface = "bold", hjust = 0, size = 5)


# Combining plots 
fig1_final <- arrange_ggsurvplots(
  list(g_unadj, g_adj_plot),
  ncol = 1,
  nrow = 2,
  heights = c(1, 1), # equal height
  print = FALSE
)

png("Figure_1.png", units = "in", width = 6, height = 11, res = 600)
print(fig1_final)
dev.off()


# 5: Figure 2 Barplots of attack history ####
# Fit models for each sex
model_female_prev <- glm.nb(
  Number_severe_asthma_attacks_during_followup ~ 
    Any_severe_attack_previous_12m_0no_1yes + 
    offset(log(Follow_up_duration_days_notlogged)) + 
    ACQ_baseline_score_mean + 
    FEV1_preBD_PCT_Baseline + 
    Treatment_step + 
    as.factor(Enrolled_Trial_name),
  data = df_female
)

model_male_prev <- glm.nb(
  Number_severe_asthma_attacks_during_followup ~ 
    Any_severe_attack_previous_12m_0no_1yes + 
    offset(log(Follow_up_duration_days_notlogged)) + 
    ACQ_baseline_score_mean + 
    FEV1_preBD_PCT_Baseline + 
    Treatment_step + 
    as.factor(Enrolled_Trial_name),
  data = df_male
)

# Function to compute predicted risks
summarize_risk <- function(df, model, sex_label) {
  pred_data <- expand.grid(Any_severe_attack_previous_12m_0no_1yes = c(0, 1))
  
  # Fill other covariates with mean/median values
  pred_data$ACQ_baseline_score_mean <- mean(df$ACQ_baseline_score_mean, na.rm = TRUE)
  pred_data$FEV1_preBD_PCT_Baseline <- mean(df$FEV1_preBD_PCT_Baseline, na.rm = TRUE)
  pred_data$Treatment_step <- median(df$Treatment_step, na.rm = TRUE)
  pred_data$Enrolled_Trial_name <- names(which.max(table(df$Enrolled_Trial_name)))
  pred_data$Follow_up_duration_days_notlogged <- 365.25  # Standardized follow-up duration
  
  # Get predictions with confidence intervals
  preds <- predict(model, newdata = pred_data, type = "link", se.fit = TRUE)
  
  # Convert to response scale
  pred_data$predicted_risk <- exp(preds$fit)
  pred_data$lower <- exp(preds$fit - 1.96 * preds$se.fit)
  pred_data$upper <- exp(preds$fit + 1.96 * preds$se.fit)
  pred_data$sex <- sex_label
  
  return(pred_data)
}

# Compute predicted risks for each sex
risk_female <- summarize_risk(df_female, model_female_prev, "Female")
risk_male <- summarize_risk(df_male, model_male_prev, "Male")

# Combine data for plotting
risk_data <- bind_rows(risk_female, risk_male)

# Plotting
# Creating labels
risk_data <- risk_data %>%
  mutate(
    Attack_label = factor(
      Any_severe_attack_previous_12m_0no_1yes,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

max_y <- risk_data %>%
  filter(sex == "Female") %>%
  summarise(max_risk = max(predicted_risk)) %>%
  pull(max_risk) * 1.05  # slightly above the bar

# Create RR labels with shared y-position
rr_labels <- risk_data %>%
  group_by(sex) %>%
  summarise(
    RR = predicted_risk[Any_severe_attack_previous_12m_0no_1yes == 1] / 
      predicted_risk[Any_severe_attack_previous_12m_0no_1yes == 0]
  ) %>%
  mutate(
    x = 0.5,
    y = max_y,
    label = paste0("RR = ", round(RR, 1))
  )

rr_labels <- rr_labels %>%
  mutate(x = "No")

# Adjusted arrow data (manual fine-tuning)
arrow_data <- data.frame(
  sex = c("Female", "Male"),
  x = c(1.15, 1.1),       # horizontal start: just inside left bar for both
  xend = c(1.65, 1.65),    # horizontal end: just outside right bar for both
  y = c(0.5, 0.3),       # vertical start: left bar top (+ margin)
  yend = c(0.7, 0.66)     # vertical end: just below right bar top
)

# Plot
ggplot(risk_data, aes(x = Attack_label, y = predicted_risk, fill = sex)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  scale_fill_manual(values = custom_colors2) +
  facet_wrap(~sex) +
  geom_segment(
    data = arrow_data,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(0.08, "inches"), type = "closed", ends = "both"),
    color = "black",
    linewidth = 0.5
  ) +
  geom_text(
    data = rr_labels,
    aes(x = 1.5, y = y, label = label),
    inherit.aes = FALSE,
    size = 3.5,
    fontface = "bold"
  ) +
  labs(
    x = "Previous Severe Asthma Attack (Past 12 Months)",
    y = "Estimated Annual Rate of Severe Asthma Attacks",
    fill = "Sex",
    title = " "
  ) +
  theme_classic()

ggsave(
  "Figure_2.png",
  width = 6,                     
  height = 4.5,
  units = "in",
  dpi = 600
)


# 6: Figure 3 Forest plot of clinical variables ####
# Creating a function to loop over all variables at once
# --- Step 1: Transform variables and set reference level (do this once) ---
imp_data_ORACLE_final_COMP_NR$Treatment_step <- relevel(as.factor(imp_data_ORACLE_final_COMP_NR$Treatment_step), ref = "3")

imp_data_ORACLE_final_COMP_NR$FEV1_PCT_reversibility_postBD_10pct <- 
  imp_data_ORACLE_final_COMP_NR$FEV1_PCT_reversibility_postBD / 10

imp_data_ORACLE_final_COMP_NR$FEV1_preBD_PCT_Baseline_10pct_decrease <- 
  -(imp_data_ORACLE_final_COMP_NR$FEV1_preBD_PCT_Baseline) / 10

imp_data_ORACLE_final_COMP_NR$ACQ_baseline_score_mean_per_0_5 <- 
  imp_data_ORACLE_final_COMP_NR$ACQ_baseline_score_mean / 0.5

# --- Step 2: Define variable list ---
independent_vars_multi <- c("Age", 
                            "BMI", 
                            "Treatment_step", 
                            "Any_severe_attack_previous_12m_0no_1yes", 
                            "Number_severe_attack_previous_12m_con", 
                            "Number_hospitalisations_for_asthma_previous_12_months_con", 
                            "Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced",
                            "FeNO_baseline_ppb",
                            "Smoking_0never_1ex_2current", 
                            "Pack_years", 
                            "Atopy_history_0no_1yes_9999notknown", 
                            "AllergicRhinitis__0no_1yes_9999notknown", 
                            "Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown",
                            "Chronic_Rhinosinusitis_0no_1yes_9999notknown",
                            "Nasal_polyposis_0no_1yes_9999notknown",
                            "Adherence_InTrial_quantity",
                            "FEV1_preBD_PCT_Baseline_10pct_decrease",
                            "FEV1_PCT_reversibility_postBD_10pct",
                            "ACQ_baseline_score_mean_per_0_5")

# --- Step 3: Define function ---
get_pooled_RRs_by_sex <- function(imp_data, main_vars, adjust_vars, sex_var = "Gender_0Female_1Male") {
  sexes <- c(0, 1)
  names(sexes) <- c("Female", "Male")
  final_results <- list()
  
  for (sex_val in sexes) {
    sex_label <- names(sexes)[which(sexes == sex_val)]
    message("Running models for sex: ", sex_label)
    
    results_list <- list()
    
    for (main_var in main_vars) {
      message("  Variable: ", main_var)
      
      main_var_term <- if (main_var %in% c(
        "Treatment_step", "Smoking_0never_1ex_2current", 
        "Atopy_history_0no_1yes_9999notknown", 
        "AllergicRhinitis__0no_1yes_9999notknown", 
        "Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown",
        "Chronic_Rhinosinusitis_0no_1yes_9999notknown",
        "Nasal_polyposis_0no_1yes_9999notknown"))
        paste0("as.factor(", main_var, ")")
      else main_var
      
      formula_str <- paste(
        "Number_severe_asthma_attacks_during_followup ~", 
        main_var_term, "+", paste(adjust_vars, collapse = " + ")
      )
      
      models <- vector("list", length = 10)
      for (i in 1:10) {
        data_i <- subset(imp_data, .imp == i & get(sex_var) == sex_val)
        models[[i]] <- glm.nb(as.formula(formula_str), data = data_i)
      }
      
      coefs <- sapply(models, function(m) coef(m), simplify = "array")
      ses   <- sapply(models, function(m) summary(m)$coefficients[, "Std. Error"], simplify = "array")
      term_names <- rownames(coefs)
      relevant_terms <- grep(main_var, term_names, value = TRUE)
      
      for (term in relevant_terms) {
        pooled_coef <- mean(coefs[term, ])
        within_var  <- mean(ses[term, ]^2)
        between_var <- var(coefs[term, ])
        total_var   <- within_var + between_var + between_var / 10
        pooled_se   <- sqrt(total_var)
        
        RR <- exp(pooled_coef)
        CI_lower <- exp(pooled_coef - 1.96 * pooled_se)
        CI_upper <- exp(pooled_coef + 1.96 * pooled_se)
        p_value <- 2 * pt(abs(pooled_coef / pooled_se), df = Inf, lower.tail = FALSE)
        
        results_list[[term]] <- data.frame(
          Term = term,
          RR = RR,
          CI_low = CI_lower,
          CI_up = CI_upper,
          p = p_value,
          Sex = sex_label
        )
      }
    }
    final_results[[sex_label]] <- do.call(rbind, results_list)
  }
  
  combined_results <- do.call(rbind, final_results)
  rownames(combined_results) <- NULL
  combined_results[, c("RR", "CI_low", "CI_up", "p")] <- round(combined_results[, c("RR", "CI_low", "CI_up", "p")], 3)
  return(combined_results)
}

# --- Step 4: Run the function ---
RR_sex_stratified <- get_pooled_RRs_by_sex(
  imp_data = imp_data_ORACLE_final_COMP_NR,
  main_vars = independent_vars_multi,
  adjust_vars = c(
    "ACQ_baseline_score_mean",  # Keep original here for adjustment
    "Any_severe_attack_previous_12m_0no_1yes", 
    "FEV1_preBD_PCT_Baseline",  # Also keep original for adjustment
    "FeNO_baseline_ppb", 
    "Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced", 
    "offset(Follow_up_duration_days)", 
    "as.factor(Enrolled_Trial_name)"
  )
)

# Creating the Forest plot
# Clean variable names for better plotting
clean_variable_name <- function(term) {
  term <- str_replace_all(term, "as.factor\\(([^)]+)\\)(\\d+)?", "\\1 \\2")
  term <- str_replace_all(term, "_0no_1yes", " (no vs. yes)")
  term <- str_replace_all(term, "_con", "")
  term <- str_replace_all(term, "_0no_1yes_9999notknown", " (no vs. yes)")
  term <- str_replace_all(term, "__", " ")
  term <- str_replace_all(term, "_", " ")
  term <- str_replace_all(term, "Treatment step (\\d+)", function(x) {
    # Clean factor levels for treatment step
    gsub("Treatment step (\\d+)", "Treatment step \\1", x)
  })
  term <- str_replace_all(term, "Any severe attack previous 12m", "Any severe attack previous 12 months")
  term <- str_trim(term)
  return(term)
}

# Apply cleaning and construct tidy dataframe
df_forest <- RR_sex_stratified %>%
  mutate(
    Variable = clean_variable_name(Term),
    OR = RR,
    CI_lower = CI_low,
    CI_upper = CI_up,
    Sex = ifelse(Sex == 1 | Sex == "1" | Sex == "Female", "Female", "Male") # fix sex labels as needed
  ) %>%
  dplyr::select(Variable, OR, CI_lower, CI_upper, p, Sex)


# Mapping to high level terms:
group_mapping <- list(
  Demographics = c(
    "Age (per year)"
  ),
  
  `Attack History` = c(
    "Any attack past 12m",
    "Number of prev attacks",
    "Number prev hospitalizations"
  ),
  
  `Type-2 Biomarkers` = c(
    "log10(FeNO)",
    "log10(BEC)"
    # "Total IgE" is not in your current df_forest — exclude or add if available
  ),
  
  `Smoking History` = c(
    "Smoking (ex vs. non)",
    "Smoking (current vs. non)",
    "Pack-Years"
  ),
  
  Comorbidities = c(
    "Nasal polyposis",
    "Chronic rhinosinusitis",
    "Atopy History",
    "Allergic Rhinitis",
    "Airborne Allergen Sensitization"
  ),
  
  Treatment = c(
    "Treatment step (1 vs. 3)",
    "Treatment step (2 vs. 3)",
    "Treatment step (4 vs. 3)",
    "Treatment step (5 vs. 3)"
  ),
  
  `Clinical Measures` = c(
    "BMI (per kg/m2)",
    "Adherence in trial (per 10% decrease)",
    "FEV1 % prebronchodilator (per 10% decrease)",
    "FEV1 postBD reversibility (per 10% increase)",
    "ACQ-5 (per 0,5 increase)"
  )
)


df_forest$Group <- purrr::map_chr(df_forest$Variable_clean, function(var) {
  matched_group <- names(group_mapping)[sapply(group_mapping, function(vars) var %in% vars)]
  if (length(matched_group) > 0) matched_group else "Other"
})

variable_clean <- c(
  # Female — rows 1 to 23
  "Age (per year)", "BMI (per kg/m2)",
  "Treatment step (1 vs. 3)", "Treatment step (2 vs. 3)", "Treatment step (4 vs. 3)", "Treatment step (5 vs. 3)",
  "Any attack past 12m", "Number of prev attacks", "Number prev hospitalizations",
  "log10(BEC)", "log10(FeNO)",
  "Smoking (ex vs. non)", "Smoking (current vs. non)", "Pack-Years",
  "Atopy History", "Allergic Rhinitis", "Airborne Allergen Sensitization",
  "Chronic rhinosinusitis", "Nasal polyposis", "Adherence in trial (per 10% decrease)",
  "FEV1 % prebronchodilator (per 10% decrease)", "FEV1 postBD reversibility (per 10% increase)", "ACQ-5 (per 0,5 increase)",
  
  # Male — rows 24 to 46
  "Age (per year)", "BMI (per kg/m2)",
  "Treatment step (1 vs. 3)", "Treatment step (2 vs. 3)", "Treatment step (4 vs. 3)", "Treatment step (5 vs. 3)",
  "Any attack past 12m", "Number of prev attacks", "Number prev hospitalizations",
  "log10(BEC)", "log10(FeNO)",
  "Smoking (ex vs. non)", "Smoking (current vs. non)", "Pack-Years",
  "Atopy History", "Allergic Rhinitis", "Airborne Allergen Sensitization",
  "Chronic rhinosinusitis", "Nasal polyposis", "Adherence in trial (per 10% decrease)",
  "FEV1 % prebronchodilator (per 10% decrease)", "FEV1 postBD reversibility (per 10% increase)", "ACQ-5 (per 0,5 increase)"
)

# Assign it to the dataframe
df_forest$Variable_clean <- variable_clean

# Update order of variables 
custom_order <- unlist(group_mapping, use.names = FALSE)
df_forest$Variable_clean <- factor(df_forest$Variable_clean, levels = custom_order)
df_forest$Group <- factor(df_forest$Group, levels = names(group_mapping))

df_forest$label_position <- 6.5  # slightly outside the plot limit of 10

ggplot(df_forest, aes(x = Variable_clean, y = OR, color = Sex)) +
  
  # Confidence interval extensions beyond axis limits (manual segments)
  geom_segment(
    data = subset(df_forest, Variable_clean == "Treatment step (1 vs. 3)"),
    aes(
      x = Variable_clean,
      xend = Variable_clean,
      y = pmin(CI_lower, 0.03),
      yend = pmax(pmin(CI_upper, 12), 0.03),
      color = Sex
    ),
    position = position_dodge(width = 0.8),
    linetype = "dotted",
    linewidth = 0.4,
    inherit.aes = FALSE
  ) +
  
  # Main (clipped) CI and point
  geom_pointrange(
    aes(ymin = pmax(CI_lower, 0.03), ymax = pmin(CI_upper, 12)),
    position = position_dodge(width = 0.8)
  ) +
  
  # Rate ratio text labels
  ggtext::geom_richtext(
    aes(
      y = label_position,
      label = sprintf("%.2f [%.2f–%.2f]", OR, CI_lower, CI_upper)
    ),
    position = position_dodge(width = 0.8),
    hjust = 0,
    size = 3.5,
    fill = NA,
    label.color = NA
  ) +
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_flip(clip = "off") +
  labs(
    title = "",
    x = "",
    y = "Rate Ratio"
  ) +
  
  scale_y_continuous(
    trans = asym_log_trans,
    limits = c(0.03, 12),
    breaks = c(0.03, 0.1, 1, 3, 5, 10),
    labels = label_number(accuracy = 0.01)
  ) +
  
  ggh4x::facet_nested(Group ~ ., scales = "free_y", space = "free") +
  
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 10, face = "bold"),
    legend.position = "top",
    strip.text.y = element_text(size = 10, face = "bold", angle = 0),
    strip.background = element_rect(fill = "gray95"),
    panel.spacing = unit(0.5, "lines"),
    plot.title = element_text(face = "bold"),
    plot.margin = margin(r = 40)
  ) +
  
  scale_color_manual(values = c("firebrick", "deepskyblue3"))


ggsave("Figure_3.png", width = 11.5, height = 9, dpi = 600)


# 7: Figure 4 3x3 matrix of BEC and FeNO ####
# Prevalences in each group
# Remove missing FeNO and Blood Eosinophils values
demo_type2 <- demo %>%
  filter(!is.na(FeNO_baseline_ppb) & !is.na(Blood_Eos_baseline_x10_9_cells_per_L))

# Categorize FeNO based on the provided cutoffs
demo_type2 <- demo_type2 %>%
  mutate(FeNO_Category = case_when(
    FeNO_baseline_ppb < 20 ~ "<20 ppb",
    FeNO_baseline_ppb >= 20 & FeNO_baseline_ppb < 35 ~ "20-<35 ppb",
    FeNO_baseline_ppb >= 35 ~ "≥35 ppb",
    TRUE ~ NA_character_
  ))

# Categorize Blood Eosinophils based on the provided cutoffs
demo_type2 <- demo_type2 %>%
  mutate(Eos_Category = case_when(
    Blood_Eos_baseline_x10_9_cells_per_L < 0.15 ~ "<0.15",
    Blood_Eos_baseline_x10_9_cells_per_L >= 0.15 & Blood_Eos_baseline_x10_9_cells_per_L < 0.3 ~ "0.15-<0.3",
    Blood_Eos_baseline_x10_9_cells_per_L >= 0.3 ~ "≥0.3",
    TRUE ~ NA_character_
  ))

# Calculate prevalence for FeNO & Eosinophils categories
feno_eos_prevalence <- demo_type2 %>%
  dplyr::group_by(FeNO_Category, Eos_Category,Gender_0Female_1Male) %>%
  dplyr::summarise(
    Count = n(),
    Percentage = (n() / nrow(demo_type2)) * 100
  ) %>%
  dplyr::arrange(FeNO_Category, Eos_Category, Gender_0Female_1Male)

sex_totals <- demo_type2 %>%
  dplyr::group_by(Gender_0Female_1Male) %>%
  dplyr::summarise(Total = n())

# Calculate the prevalence by FeNO and Eos categories
feno_eos_prevalence <- demo_type2 %>%
  dplyr::group_by(Gender_0Female_1Male, FeNO_Category, Eos_Category) %>%
  dplyr::summarise(
    Count = n(),
    .groups = "drop"
  ) %>%
  # Merge with the sex totals to get the relative percentages
  dplyr::left_join(sex_totals, by = "Gender_0Female_1Male") %>%
  dplyr::mutate(
    Percentage = (Count / Total) * 100  # Calculate relative percentage per sex
  ) %>%
  dplyr::arrange(Gender_0Female_1Male, FeNO_Category, Eos_Category)


# Order FeNO and Eos categories
feno_eos_prevalence$FeNO_Category <- factor(feno_eos_prevalence$FeNO_Category, 
                                            levels = c("<20 ppb", "20-<35 ppb", "≥35 ppb"))
feno_eos_prevalence$Eos_Category <- factor(feno_eos_prevalence$Eos_Category, 
                                           levels = c("<0.15", "0.15-<0.3", "≥0.3"))

# Rate ratios: first, make sure there are dummy variables for FENO and BEC
# Different cutoffs for FeNO: 20ppb - 35ppb ####
# Create a dummy variable per FeNO-BEC combination (low, mid, high)
## FeNO <20, BEC <0.15
imp_data_ORACLE_final_COMP_NR <- imp_data_ORACLE_final_COMP_NR %>%
  mutate(
    FeNO_low_Eos_low = ifelse(
      FeNO_baseline_ppb < log10(20) & # the variables are log-transformed in the data set
        Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced < log10(0.15),
      1, 0
    )
  )

imp_data_ORACLE_final_COMP_NR$FeNO_low_Eos_low <- as.factor(imp_data_ORACLE_final_COMP_NR$FeNO_low_Eos_low) # make sure that the new variable is a factor

## FeNO <20, BEC 0.15-0.30

imp_data_ORACLE_final_COMP_NR <- imp_data_ORACLE_final_COMP_NR %>%
  mutate(
    FeNO_low_Eos_mid = ifelse(
      FeNO_baseline_ppb < log10(20) & 
        Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced >= log10(0.15) & 
        Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced < log10(0.30),
      1, 0
    )
  )

imp_data_ORACLE_final_COMP_NR$FeNO_low_Eos_mid <- as.factor(imp_data_ORACLE_final_COMP_NR$FeNO_low_Eos_mid)

## FeNO <20, BEC >=0.30
imp_data_ORACLE_final_COMP_NR <- imp_data_ORACLE_final_COMP_NR %>%
  mutate(
    FeNO_low_Eos_high = ifelse(
      FeNO_baseline_ppb < log10(20) & 
        Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced >= log10(0.30),
      1, 0
    )
  )

imp_data_ORACLE_final_COMP_NR$FeNO_low_Eos_high <- as.factor(imp_data_ORACLE_final_COMP_NR$FeNO_low_Eos_high)

## FeNO 20-35, BEC <0.15
imp_data_ORACLE_final_COMP_NR <- imp_data_ORACLE_final_COMP_NR %>%
  mutate(
    FeNO_mid_Eos_low = ifelse(
      FeNO_baseline_ppb >= log10(20) & 
        FeNO_baseline_ppb < log10(35) &
        Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced < log10(0.15), 
      1, 0
    )
  )

imp_data_ORACLE_final_COMP_NR$FeNO_mid_Eos_low <- as.factor(imp_data_ORACLE_final_COMP_NR$FeNO_mid_Eos_low)

## FeNO 20-35, BEC 0.15-0.30
imp_data_ORACLE_final_COMP_NR <- imp_data_ORACLE_final_COMP_NR %>%
  mutate(
    FeNO_mid_Eos_mid = ifelse(
      FeNO_baseline_ppb >= log10(20) & 
        FeNO_baseline_ppb < log10(35) &
        Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced >= log10(0.15) &
        Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced < log10(0.30), 
      1, 0
    )
  )

imp_data_ORACLE_final_COMP_NR$FeNO_mid_Eos_mid <- as.factor(imp_data_ORACLE_final_COMP_NR$FeNO_mid_Eos_mid)

## FeNO 20-35, BEC >=0.30
imp_data_ORACLE_final_COMP_NR <- imp_data_ORACLE_final_COMP_NR %>%
  mutate(
    FeNO_mid_Eos_high = ifelse(
      FeNO_baseline_ppb >= log10(20) & 
        FeNO_baseline_ppb < log10(35) &
        Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced >= log10(0.30), 
      1, 0
    )
  )

imp_data_ORACLE_final_COMP_NR$FeNO_mid_Eos_high <- as.factor(imp_data_ORACLE_final_COMP_NR$FeNO_mid_Eos_high)

## FeNO >=35, BEC <0.15
imp_data_ORACLE_final_COMP_NR <- imp_data_ORACLE_final_COMP_NR %>%
  mutate(
    FeNO_high_Eos_low = ifelse(
      FeNO_baseline_ppb >= log10(35) & 
        Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced < log10(0.15), 
      1, 0
    )
  )

imp_data_ORACLE_final_COMP_NR$FeNO_high_Eos_low <- as.factor(imp_data_ORACLE_final_COMP_NR$FeNO_high_Eos_low)

## FeNO >=35, BEC 0.15-0.30
imp_data_ORACLE_final_COMP_NR <- imp_data_ORACLE_final_COMP_NR %>%
  mutate(
    FeNO_high_Eos_mid = ifelse(
      FeNO_baseline_ppb >= log10(35) & 
        Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced >= log10(0.15) &
        Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced < log10(0.30),
      1, 0
    )
  )

imp_data_ORACLE_final_COMP_NR$FeNO_high_Eos_mid <- as.factor(imp_data_ORACLE_final_COMP_NR$FeNO_high_Eos_mid)

## FeNO >=35, BEC >0.30
imp_data_ORACLE_final_COMP_NR <- imp_data_ORACLE_final_COMP_NR %>%
  mutate(
    FeNO_high_Eos_high = ifelse(
      FeNO_baseline_ppb >= log10(35) & 
        Blood_Eos_baseline_x10_9_cells_per_L_zeroreplaced >= log10(0.30),
      1, 0
    )
  )

imp_data_ORACLE_final_COMP_NR$FeNO_high_Eos_high <- as.factor(imp_data_ORACLE_final_COMP_NR$FeNO_high_Eos_high)


# Sex-stratified loop ####
# Define categories
categories <- c("FeNO_low_Eos_low", "FeNO_low_Eos_mid", "FeNO_low_Eos_high",
                "FeNO_mid_Eos_low", "FeNO_mid_Eos_mid", "FeNO_mid_Eos_high",
                "FeNO_high_Eos_low", "FeNO_high_Eos_mid", "FeNO_high_Eos_high")

# Initialize a list to store results
results_list <- list()

# Loop over gender categories
for (gender in c(0, 1)) {
  gender_label <- ifelse(gender == 0, "Female", "Male")
  
  # Loop over FeNO-BEC categories
  for (cat in categories) {
    res_comb <- vector("list", 10)  # Preallocate list for models
    
    for (i in 1:10) {
      # Filter by BOTH gender AND imputation number
      data_subset <- subset(imp_data_ORACLE_final_COMP_NR, 
                            Gender_0Female_1Male == gender & .imp == i)
      
      res_comb[[i]] <- glm.nb(Number_severe_asthma_attacks_during_followup ~ 
                                get(cat) + 
                                offset(data_subset$linear_predictors), 
                              data = data_subset)
    }
    
    # Pool the estimates
    res_pool <- summary(pool(res_comb), conf.int = TRUE, exp = TRUE)
    
    # Store results in the output list
    results_list[[paste0(gender_label, "_", cat)]] <- res_pool
  }
}


# Add results to 3x3 matrice figures ####
category_map <- list(
  "FeNO_low_Eos_low" = c("<20 ppb", "<0.15"),
  "FeNO_low_Eos_mid" = c("<20 ppb", "0.15-<0.3"),
  "FeNO_low_Eos_high" = c("<20 ppb", "≥0.3"),
  "FeNO_mid_Eos_low" = c("20-<35 ppb", "<0.15"),
  "FeNO_mid_Eos_mid" = c("20-<35 ppb", "0.15-<0.3"),
  "FeNO_mid_Eos_high" = c("20-<35 ppb", "≥0.3"),
  "FeNO_high_Eos_low" = c("≥35 ppb", "<0.15"),
  "FeNO_high_Eos_mid" = c("≥35 ppb", "0.15-<0.3"),
  "FeNO_high_Eos_high" = c("≥35 ppb", "≥0.3")
)

# Convert results_list to a dataframe
extract_results <- function(res_list, gender) {
  res_df <- data.frame()
  for (cat in names(res_list)) {
    if (grepl(gender, cat)) {
      model_results <- res_list[[cat]]
      estimate <- model_results$estimate[model_results$term == "get(cat)1"]  # Extract estimate
      lower <- model_results$conf.low[model_results$term == "get(cat)1"]      # Lower CI
      upper <- model_results$conf.high[model_results$term == "get(cat)1"]     # Upper CI
      
      if (length(estimate) > 0) {  # Ensure non-empty values are added
        res_df <- rbind(res_df, data.frame(
          Gender_0Female_1Male = ifelse(gender == "Female", 0, 1),
          FeNO_Category = category_map[[gsub(paste0(gender, "_"), "", cat)]][1],
          Eos_Category = category_map[[gsub(paste0(gender, "_"), "", cat)]][2],
          Estimate = estimate,
          Lower_CI = lower,
          Upper_CI = upper
        ))
      }
    }
  }
  return(res_df)
}


# Extract results for both genders
results_female_full <- extract_results(results_list, "Female")
results_male_full <- extract_results(results_list, "Male")

# Merge results with prevalence data
feno_eos_prevalence_new <- feno_eos_prevalence %>%
  left_join(bind_rows(results_female_full, results_male_full), by = c("Gender_0Female_1Male", "FeNO_Category", "Eos_Category"))

# Ensure correct order of FeNO and Eos categories
feno_eos_prevalence_new <- feno_eos_prevalence_new %>%
  mutate(
    FeNO_Category = factor(FeNO_Category, levels = c("<20 ppb", "20-<35 ppb", "≥35 ppb")),  # High to Low (top-bottom)
    Eos_Category = factor(Eos_Category, levels = c("<0.15", "0.15-<0.3", "≥0.3"))  # Low to High (left-right)
  )

# Get limits for heatmap scale based on model estimates
max_estimate <- max(feno_eos_prevalence_new$Estimate, na.rm = TRUE)
min_estimate <- min(feno_eos_prevalence_new$Estimate, na.rm = TRUE)


# Define heatmap function with both model and prevalence data
plot_heatmap <- function(data, gender_label) {
  ggplot(data, aes(x = Eos_Category, y = FeNO_Category, fill = Estimate)) +  # Gradient now based on Estimate
    geom_tile(color = "white") +  
    scale_fill_gradient(low = "white", high = "salmon", name = "Estimate", 
                        limits = c(min_estimate, max_estimate)) +  # Set scale based on estimates
    geom_text(aes(label = paste0(round(Estimate, 2), " [", round(Lower_CI, 2), "-", round(Upper_CI, 2), "]\n",
                                 Count, "/", Total, " (", round(Percentage, 1), "%)")), 
              color = "black", size = 3) +  
    labs(title = gender_label, x = "Blood eosinophil counts", y = "FeNO") +
    theme_classic() +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 10)) +
    coord_fixed()
}

# Generate heatmaps
plot_female_full <- plot_heatmap(feno_eos_prevalence_new %>% filter(Gender_0Female_1Male == 0), "Females")
plot_male_full <- plot_heatmap(feno_eos_prevalence_new %>% filter(Gender_0Female_1Male == 1), "Males")

# Updated code for bold font
plot_heatmap_cols_upd <- function(data, gender_label) {
  # Calculate the midpoint for the color scale
  mid_estimate <- (min_estimate + max_estimate) / 2
  
  ggplot(data, aes(x = Eos_Category, y = FeNO_Category, fill = Estimate)) +
    geom_tile(color = "white") +  
    scale_fill_gradient2(low = "lightyellow", mid = "orange", high = "firebrick", 
                         midpoint = mid_estimate, name = "Rate ratio", 
                         limits = c(min_estimate, max_estimate)) +
    geom_richtext(
      aes(label = paste0(
        "<b><span style='font-size:12pt;'>", round(Estimate, 2), "</span></b><br>",
        round(Lower_CI, 2), "–", round(Upper_CI, 2), "<br>",
        Count, "/", Total, " (", round(Percentage, 1), "%)"
      )),
      fill = NA, label.color = NA,  # transparent background
      size = 3
    ) +  
    labs(title = gender_label, 
         x = "Blood eosinophil counts (x10^9 cells/L)",
         y = "FeNO (ppb)") +
    theme_classic() +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(size = 12),
          plot.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 10)) +
    coord_fixed()
}


plot_female_cols_upd <- plot_heatmap_cols_upd(feno_eos_prevalence_new %>% filter(Gender_0Female_1Male == 0), "Females")
plot_male_cols_upd <- plot_heatmap_cols_upd(feno_eos_prevalence_new %>% filter(Gender_0Female_1Male == 1), "Males")

# Combined plot
plot_female_cols_upd + plot_male_cols_upd + plot_layout(guides = "collect")
ggsave("FENO_BEC_3x3_bold_20_35.png",width = 10, height = 6, dpi = 600)


# Supplements ####
# Barplots of other variables (differences not significant, cfr. interaction analysis) ####
# Variables to plot
vars_to_plot <- c(
  "Smoking_0never_1ex_2current",
  "Atopy_history_0no_1yes_9999notknown",
  "Eczema_0no_1yes_9999notknown",
  "AllergicRhinitis__0no_1yes_9999notknown",
  "Airborne_allergen_sensitisation_on_testing_0no_1yes_9999notknown",
  "Chronic_Rhinosinusitis_0no_1yes_9999notknown",
  "Nasal_polyposis_0no_1yes_9999notknown",
  "Psychiatric_disease_0no_1yes_9999notknown_NOTIMPUTED"
)

# Simple prediction function (without Enrolled_Trial_name)
summarize_risk <- function(df, model, varname, sex_label) {
  levels_var <- sort(unique(df[[varname]]))
  levels_var <- levels_var[!is.na(levels_var)]  # Remove NA levels
  
  pred_data <- data.frame(var = levels_var)
  names(pred_data) <- varname
  
  # Fill covariates
  pred_data$ACQ_baseline_score_mean <- mean(df$ACQ_baseline_score_mean, na.rm = TRUE)
  pred_data$FEV1_preBD_PCT_Baseline <- mean(df$FEV1_preBD_PCT_Baseline, na.rm = TRUE)
  pred_data$Treatment_step <- median(df$Treatment_step, na.rm = TRUE)
  pred_data$Follow_up_duration_days_notlogged <- 365.25
  
  # Predict on link scale and transform
  preds <- predict(model, newdata = pred_data, type = "link", se.fit = TRUE)
  
  pred_data$predicted_risk <- exp(preds$fit)
  pred_data$lower <- exp(preds$fit - 1.96 * preds$se.fit)
  pred_data$upper <- exp(preds$fit + 1.96 * preds$se.fit)
  
  pred_data$sex <- sex_label
  pred_data$variable <- varname
  names(pred_data)[1] <- "category"
  
  return(pred_data)
}

# Process all variables
all_risks <- list()

for (var in vars_to_plot) {
  cat("Processing:", var, "\n")
  
  # Simplified formula without Enrolled_Trial_name
  formula <- as.formula(paste(
    "Number_severe_asthma_attacks_during_followup ~",
    var,
    "+ offset(log(Follow_up_duration_days_notlogged)) + ACQ_baseline_score_mean + FEV1_preBD_PCT_Baseline + Treatment_step"
  ))
  
  # Fit models
  model_female <- glm.nb(formula, data = df_female)
  model_male <- glm.nb(formula, data = df_male)
  
  # Get predictions
  risk_female <- summarize_risk(df_female, model_female, var, "Female")
  risk_male <- summarize_risk(df_male, model_male, var, "Male")
  
  all_risks[[var]] <- bind_rows(risk_female, risk_male)
}

# Combine all results
risk_data_all <- bind_rows(all_risks)

# Clean labels for variables
clean_labels <- function(x) {
  x <- gsub("_0no_1yes_9999notknown", "", x)
  x <- gsub("_0never_1ex_2current", " Smoking", x)
  x <- gsub("__", " ", x)
  x <- gsub("_", " ", x)
  x <- gsub("NOTIMPUTED", "", x)
  x <- trimws(x)
  x
}

risk_data_all$variable <- clean_labels(risk_data_all$variable)

# Clean category labels to show meaningful text
risk_data_all$category_label <- ifelse(
  grepl("Smoking", risk_data_all$variable),
  case_when(
    risk_data_all$category == 0 ~ "Never",
    risk_data_all$category == 1 ~ "Ex-smoker", 
    risk_data_all$category == 2 ~ "Current",
    TRUE ~ as.character(risk_data_all$category)
  ),
  case_when(
    risk_data_all$category == 0 ~ "No",
    risk_data_all$category == 1 ~ "Yes",
    TRUE ~ as.character(risk_data_all$category)
  )
)

# Split variables into two groups for separate plots
variables_unique <- unique(risk_data_all$variable)
n_vars <- length(variables_unique)
group1_vars <- variables_unique[2:4]
group2_vars <- variables_unique[5:n_vars]

risk_data_all <- risk_data_all %>%
  mutate(variable_label = paste0(" ", variable))  # Optional spacing tweak

# Create first plot (1x4 layout)
plot_risks_1 <- ggplot(risk_data_all[7:18,], aes(x = category_label, y = predicted_risk, fill = sex)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.7), width = 0.2) +
  facet_wrap(~ variable_label, ncol = 1, scales = "free_x", strip.position = "top") +
  scale_fill_manual(values = c("Female" = "firebrick", "Male" = "deepskyblue3")) +
  labs(
    x = "",
    y = "Estimated Annual Rate of Severe Asthma Attacks",
    fill = "Sex"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank()
  )

# Create second plot (1x4 layout)
plot_risks_2 <- ggplot(risk_data_all[19:34,], aes(x = category_label, y = predicted_risk, fill = sex)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.7), width = 0.2) +
  facet_wrap(~ variable_label, ncol = 1, scales = "free_x", strip.position = "top") +
  scale_fill_manual(values = c("Female" = "firebrick", "Male" = "deepskyblue3")) +
  labs(
    x = "",
    y = "Estimated Annual Rate of Severe Asthma Attacks",
    fill = "Sex"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank()
  )

# Print both plots
print(plot_risks_1)
ggsave("Sex_diff_other_vars_1.png", height = 8.25, width = 7, dpi = 600)

print(plot_risks_2)
ggsave("Sex_diff_other_vars_2.png", height = 11, width = 7, dpi = 600)


# Smoking
ggplot(risk_data_all[1:6,], aes(x = category_label, y = predicted_risk, fill = sex)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.7), width = 0.2) +
  facet_wrap(~ variable_label, ncol = 1, scales = "free_x", strip.position = "top") +
  scale_fill_manual(values = c("Female" = "firebrick", "Male" = "deepskyblue3")) +
  labs(
    x = "",
    y = "Estimated Annual Rate of Severe Asthma Attacks",
    fill = "Sex"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank()
  )

ggsave("Sex_diff_smoking.png", height = 5, width = 7, dpi = 600)
