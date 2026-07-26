# ==============================================================================
# Project: Statistical Analysis of the 2025/2026 County Health Rankings
# Author: Claudia Eunice dela Paz
# Purpose: Examining correlations between insurance coverage, healthcare access, 
#          and socioeconomic factors against population health outcomes.
# ==============================================================================

# ---- SECTION 1: SETUP & DATA INGESTION ----

library(tidyverse)
library(readxl)


raw_data <- read_excel("2025 County Health Rankings Data.xlsx", sheet = "Select Measure Data")

# ---- SECTION 2: DATA CLEANING & PREPROCESSING ----

# 2. Filter out state/national summary rows and fix ratio data types
clean_data <- raw_data %>%
  # Remove summary rows where County is missing
  filter(!is.na(County)) %>%
  # Convert strings like "Ratio:1" into numeric values by extracting leading digits
  mutate(
    `Primary Care Physicians Ratio` = as.numeric(str_extract(`Primary Care Physicians Ratio`, "^\\d+")),
    `Mental Health Provider Ratio`  = as.numeric(str_extract(`Mental Health Provider Ratio`, "^\\d+"))
  )

# 3. Create a complete case dataset by dropping remaining NAs
final_cleaned_data <- clean_data %>%
  drop_na()

# 4. Sanity check the cleaned structure
str(final_cleaned_data)
summary(final_cleaned_data)

# 5. Export cleaned dataset for downstream analysis and GitHub sharing
write_csv(final_cleaned_data, "Cleaned_County_Health_Data_2026.csv")


# ---- SECTION 3: NATIONAL EXPLORATORY DATA ANALYSIS (EDA) ----
# Investigating top and bottom performing counties across insurance and health outcomes

# Highest Uninsured Counties (Top 3)
highest_uninsured <- clean_data %>%
  slice_max(order_by = `% Uninsured`, n = 3) %>%
  select(State, County, `% Uninsured`)

print("--- Top 3 Highest Uninsured Counties ---")
print(highest_uninsured)

# Lowest Uninsured Counties (Top 3)
lowest_uninsured <- clean_data %>%
  slice_min(order_by = `% Uninsured`, n = 3) %>%
  select(State, County, `% Uninsured`)

print("--- Top 3 Lowest Uninsured Counties ---")
print(lowest_uninsured)

# Physically Unhealthy Days: Worst and Best Performing Counties
highest_phys <- clean_data %>%
  slice_max(order_by = `Average Number of Physically Unhealthy Days`, n = 3) %>%
  select(State, County, `Average Number of Physically Unhealthy Days`)

lowest_phys <- clean_data %>%
  slice_min(order_by = `Average Number of Physically Unhealthy Days`, n = 3) %>%
  select(State, County, `Average Number of Physically Unhealthy Days`)

print("--- Highest Average Physically Unhealthy Days ---")
print(highest_phys)
print("--- Lowest Average Physically Unhealthy Days ---")
print(lowest_phys)

# Mentally Unhealthy Days: Worst and Best Performing Counties
highest_mental <- clean_data %>%
  slice_max(order_by = `Average Number of Mentally Unhealthy Days`, n = 3) %>%
  select(State, County, `Average Number of Mentally Unhealthy Days`)

lowest_mental <- clean_data %>%
  slice_min(order_by = `Average Number of Mentally Unhealthy Days`, n = 3) %>%
  select(State, County, `Average Number of Mentally Unhealthy Days`)

print("--- Highest Average Mentally Unhealthy Days ---")
print(highest_mental)
print("--- Lowest Average Mentally Unhealthy Days ---")
print(lowest_mental)

# Fair or Poor Health Percentages: Worst and Best Performing Counties
highest_fp <- clean_data %>%
  slice_max(order_by = `% Fair or Poor Health`, n = 3) %>%
  select(State, County, `% Fair or Poor Health`)

lowest_fp <- clean_data %>%
  slice_min(order_by = `% Fair or Poor Health`, n = 3) %>%
  select(State, County, `% Fair or Poor Health`)

print("--- Highest % Fair/Poor Health ---")
print(highest_fp)
print("--- Lowest % Fair/Poor Health ---")
print(lowest_fp)


# ---- SECTION 4: WEST VIRGINIA HYPOTHESIS TESTING ----
# Research Question: Why does West Virginia exhibit a high average of mentally unhealthy days?
# Initial Hypothesis: Lack of mental health providers drives unhealthy mental days.

wv_clean <- clean_data %>%
  filter(State == "West Virginia") %>%
  rename(
    MH_Ratio = `Mental Health Provider Ratio`,
    Mentally_Unhealthy_Days = `Average Number of Mentally Unhealthy Days`
  ) %>%
  filter(!is.na(MH_Ratio), !is.na(Mentally_Unhealthy_Days))

# Test Initial Hypothesis
wv_mh_corr <- cor(wv_clean$MH_Ratio, wv_clean$Mentally_Unhealthy_Days, use = "complete.obs")
print(paste("WV Mental Health Provider Ratio vs. Unhealthy Days Correlation:", round(wv_mh_corr, 4)))
# Finding: r = -0.0409 (Near zero). This refutes the initial hypothesis; provider ratio 
# is not a strong indicator. Pivoting to test socioeconomic and environmental variables.

# Pivot: Multivariate Correlation Analysis for West Virginia
wv_predictors <- c(
  "Food Environment Index", 
  "% Severe Housing Problems", 
  "% Uninsured", 
  "80th Percentile Income", 
  "% Some College", 
  "% Completed High School"
)

# Calculate correlations across all socioeconomic predictors cleanly
wv_correlations <- sapply(wv_predictors, function(var) {
  cor(wv_clean[[var]], wv_clean$Mentally_Unhealthy_Days, use = "complete.obs")
})

wv_summary_table <- data.frame(
  Predictor_Variable = names(wv_correlations),
  Correlation_with_Mental_Health_Days = round(wv_correlations, 4)
) %>%
  arrange(Correlation_with_Mental_Health_Days)

print("--- West Virginia Socioeconomic Predictors of Mentally Unhealthy Days ---")
print(wv_summary_table)
# Key Findings:
# 1. % Completed High School (r = -0.66) and 80th Percentile Income (r = -0.62) are the strongest predictors.
# 2. % Some College (r = -0.52) and Food Environment Index (r = -0.44) show moderate inverse relationships.
# 3. % Severe Housing Problems (r = 0.10) shows no meaningful correlation.


# ---- SECTION 5: COMPARATIVE & NATIONAL CORRELATION MATRIX ----

# Targeted County Comparison: South Dakota vs. Texas outliers
comparison_social <- clean_data %>%
  filter(
    (State == "South Dakota" & County == "Lincoln") | 
    (State == "Texas" & County == "Kenedy")
  ) %>%
  select(
    State, County, `80th Percentile Income`, `Food Environment Index`, 
    `% Completed High School`, `% Unemployed`, `% Severe Housing Problems`, 
    `% Some College`, `Primary Care Physicians Ratio`, `Mental Health Provider Ratio`
  )

print("--- Targeted Social Factor Comparison: Lincoln, SD vs. Kenedy, TX ---")
print(comparison_social)
# View(comparison_social) # Uncomment for GUI viewing in RStudio

# National Correlation Matrix: Mental Health Outcomes
cor_results_mental <- clean_data %>%
  select(
    `80th Percentile Income`, `Food Environment Index`, 
    `% Completed High School`, `% Unemployed`, 
    `Average Number of Mentally Unhealthy Days`
  ) %>%
  cor(use = "complete.obs")

print("--- National Correlation Matrix: Mentally Unhealthy Days ---")
print(cor_results_mental)

# National Correlation Matrix: Physical Health Outcomes
cor_results_physical <- clean_data %>%
  select(
    `80th Percentile Income`, `Food Environment Index`, 
    `% Completed High School`, `% Unemployed`, 
    `Average Number of Physically Unhealthy Days`
  ) %>%
  cor(use = "complete.obs")

print("--- National Correlation Matrix: Physically Unhealthy Days ---")
print(cor_results_physical)
