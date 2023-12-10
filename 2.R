library(readr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(knitr)
library(broom)

# Read the csv file from github
brain_stroke <- read.table(
  file = "https://raw.githubusercontent.com/Minu-Oh/STAT_184/main/brain_stroke.csv",
  header = TRUE,
  sep = ",")

# Calculate the percentage of stroke by gender
gender_percentage_data <- brain_stroke %>%
  group_by(gender, stroke) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(gender) %>%
  mutate(percentage = count / sum(count) * 100)

# Convert numeric variable into a factor
brain_stroke$stroke <- factor(brain_stroke$stroke)

# Create the bar plot
gender_percentage_data <- gender_percentage_data %>%
  mutate(stroke = as.factor(stroke))

ggplot(gender_percentage_data, aes(x = gender, 
                                   y = percentage, 
                                   fill = stroke)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  scale_fill_manual(values = c("grey", "red"),
                    labels = c("No Stroke", "Stroke"),
                    name = "Stroke") +
  labs(title = "Percentage Distribution of Stroke by Gender",
       x = "Gender",
       y = "Percentage",
       fill = "Stroke") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Chi-Squared Test for Stroke by Gender
gender_stroke_tab <- table(brain_stroke$gender, brain_stroke$stroke)

# Perform the Chi-Squared Test (don't print the raw output)
gender_stroke_test <- chisq.test(gender_stroke_tab)

# Extract relevant information
test_results <- data.frame(
  Statistic = gender_stroke_test$statistic,
  DF = gender_stroke_test$parameter,
  P_Value = gender_stroke_test$p.value)

# Create a clean table for the test results
kable(test_results, caption = "Chi-Squared Test Results for Stroke by Gender", 
      col.names = c("Chi-Squared Statistic", "Degrees of Freedom", "P-Value"),
      align = 'c')

# Create the box plot
ggplot(brain_stroke,aes(x = stroke,
                        y = age,
                        fill = stroke)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey", "red"),
                    labels = c("No Stroke", "Stroke"),name = "Stroke") +
  labs(title = "Boxplot of Age by Stroke",
       x = "Stroke Status",
       y = "Age") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Perform T-test
t_test_age <- t.test(age ~ stroke, data = brain_stroke)

# Extract results and create a table
t_test_age_results <- data.frame(
  Statistic = t_test_age$statistic,
  P_Value = t_test_age$p.value)
kable(t_test_age_results, caption = "T-Test Results for Age Differences", 
      col.names = c("T-Statistic", "P-Value"))

# Create a summary table for counts
hypertension_table <- brain_stroke %>%
  group_by(hypertension, stroke) %>%
  summarise(count = n(), .groups = "drop")
# Calculate percentages
hypertension_table <- hypertension_table %>%
  group_by(hypertension) %>%
  mutate(percentage = count / sum(count) * 100)

# Bar plot of hypertension and stroke with percentages
ggplot(hypertension_table, aes(x = hypertension, 
                               y = percentage, 
                               fill = as.factor(stroke))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("grey", "red"), 
                    labels = c("No Stroke", "Stroke"),
                    name = "Stroke") +
  scale_x_discrete(labels = c("0" = "No Hypertension", "1" = "Hypertension"),
                   name = "Hypertension") +
  labs(title = "Percentage Distribution of Stroke by Hypertension",
       x = "",
       y = "Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create a summary table for counts
heart_disease_table <- brain_stroke %>%
  group_by(heart_disease, stroke) %>%
  summarise(count = n(), .groups = "drop")
# Calculate percentages
heart_disease_table <- heart_disease_table %>%
  group_by(heart_disease) %>%
  mutate(percentage = count / sum(count) * 100)

# Bar plot of heart_disease and stroke with percentages
ggplot(heart_disease_table, aes(x = heart_disease, 
                                y = percentage, 
                                fill = as.factor(stroke))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("grey", "red"), 
                    labels = c("No Stroke", "Stroke"),
                    name = "Stroke") +
  scale_x_discrete(labels = c("0" = "No Heart Disease", "1" = "Heart Disease"),
                   name = "Heart Disease") +
  labs(title = "Percentage Distribution of Stroke by Heart Disease",
       x = "",
       y = "Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate the counts for each combination of hypertension and heart_disease
counts <- brain_stroke %>%
  count(hypertension, heart_disease, stroke) %>%
  group_by(hypertension, heart_disease) %>%
  mutate(perc = n / sum(n))

# Bar plot of hypertension, heart disease and stroke
ggplot(counts, aes(x = interaction(hypertension, heart_disease), 
                   y = perc, 
                   fill = stroke)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(perc, accuracy = 0.1), group = stroke), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("grey", "red"), 
                    labels = c("No Stroke", "Stroke"),
                    name = "Stroke") +
  scale_x_discrete(labels = c("0.0" = "NH & NHD",
                              "1.0" = "H & NHD",
                              "0.1" = "NH & HD",
                              "1.1" = "H & HD")) +
  labs(title = "Relationship Among Hypertension, Heart Disease, and Stroke",
       x = "Hypertension & Heart Disease",
       y = "Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Perform Logistic Regression
model <- glm(stroke ~ hypertension + heart_disease, data = brain_stroke, family = "binomial")
# Extract coefficients
model_summary <- summary(model)
coef_table <- model_summary$coefficients
# Create a table for coefficients
kable(coef_table, caption = "Logistic Regression Coefficients")

# First, calculate the count of stroke occurrences by work type
work_type_counts <- brain_stroke %>%
  group_by(work_type, stroke) %>%
  summarise(count = n(), .groups = 'drop')
# Calculate percentages
work_type_counts <- work_type_counts %>%
  group_by(work_type) %>%
  mutate(percentage = count / sum(count) * 100)

# Bar plot of work_type and stroke with percentages
ggplot(work_type_counts, aes(x = work_type, 
                             y = percentage, 
                             fill = as.factor(stroke))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("grey", "red"), 
                    labels = c("No Stroke", "Stroke"),
                    name = "Stroke") +
  labs(title = "Percentage Distribution of Stroke by Work Type", 
       x = "Work Type", 
       y = "Percentage", 
       fill = "Stroke Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create a contingency table for work type and stroke occurrence
work_stroke_tab <- table(brain_stroke$work_type, brain_stroke$stroke)
# Perform the Chi-Squared Test
work_stroke_test <- chisq.test(work_stroke_tab)
# Extract results and create a table for presentation
work_stroke_results <- data.frame(
  'Chi-Squared Statistic' = work_stroke_test$statistic,
  'Degrees of Freedom' = work_stroke_test$parameter,
  'P-Value' = work_stroke_test$p.value)
# Use kable from knitr to create a nicely formatted table
kable(work_stroke_results, 
      caption = "Chi-Squared Test Results for Stroke by Work Type", 
      align = 'c')

# Calculate the count of stroke occurrences by residence type
residence_type_counts <- brain_stroke %>%
  group_by(Residence_type, stroke) %>%
  summarise(count = n(), .groups = 'drop')
# Calculate percentages
residence_type_counts <- residence_type_counts %>%
  group_by(Residence_type) %>%
  mutate(percentage = count / sum(count) * 100)

# Convert Residence Type into a Factor
brain_stroke$Residence_type <- factor(brain_stroke$Residence_type)
# Bar plot of residence type and stroke with percentages
ggplot(residence_type_counts, aes(x = Residence_type, y = percentage, fill = as.factor(stroke))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("grey", "red"), 
                    labels = c("No Stroke", "Stroke"),
                    name = "Stroke") +
  labs(title = "Percentage Distribution of Stroke by Residence Type", 
       x = "Residence Type", 
       y = "Percentage", 
       fill = "Stroke Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create a contingency table for work type and stroke occurrence
residence_stroke_tab <- table(brain_stroke$Residence_type, brain_stroke$stroke)
# Perform the Chi-Squared Test
residence_stroke_test <- chisq.test(residence_stroke_tab)
# Extract results and create a table for presentation
residence_stroke_results <- data.frame(
  'Chi-Squared Statistic' = residence_stroke_test$statistic,
  'Degrees of Freedom' = residence_stroke_test$parameter,
  'P-Value' = residence_stroke_test$p.value)
# Use kable from knitr to create a nicely formatted table
kable(residence_stroke_results, 
      caption = "Chi-Squared Test Results for Stroke by Residence Type", 
      align = 'c')

# Box plot of average glucose levels and stroke
ggplot(brain_stroke, aes(x = factor(stroke, labels = c("No Stroke", "Stroke")), 
                         y = avg_glucose_level, fill = stroke)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey", "red"), 
                    labels = c("No Stroke", "Stroke"),
                    name = "Stroke") +
  labs(title = "Distribution of Stroke by Average Glucose Level",
       x = "Stroke",
       y = "Average Glucose Level",
       fill = "Stroke") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Perform T-test
t_test_glucose <- t.test(avg_glucose_level ~ stroke, data = brain_stroke)
# Extract results and create a table
t_test_glucose_results <- data.frame(
  Statistic = t_test_glucose$statistic,
  P_Value = t_test_glucose$p.value)
kable(t_test_glucose_results, 
      caption = "T-Test Results for Average Glucose Level Differences", 
      col.names = c("T-Statistic", "P-Value"))

# Box plots of BMI and stroke
ggplot(brain_stroke, aes(x = factor(stroke, labels = c("No Stroke", "Stroke")), 
                         y = bmi, fill = stroke)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey", "red"), 
                    labels = c("No Stroke", "Stroke"),
                    name = "Stroke") +
  labs(title = "Distribution of Stroke by BMI",
       x = "Stroke",
       y = "BMI",
       fill = "Stroke") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Perform T-test
t_test_bmi <- t.test(bmi ~ stroke, data = brain_stroke)
# Extract results and create a table
t_test_bmi_results <- data.frame(
  Statistic = t_test_bmi$statistic,
  P_Value = t_test_bmi$p.value)
kable(t_test_bmi_results, caption = "T-Test Results for BMI Differences", 
      col.names = c("T-Statistic", "P-Value"))

# Create a scatter plot to visualize the relationship
ggplot(brain_stroke, aes(x = bmi, y = avg_glucose_level, color = stroke)) +
  geom_point(alpha = 0.5) + 
  scale_color_manual(values = c("0" = "grey", "1" = "red"),
                     labels = c("No Stroke", "Stroke"),
                     name = "Stroke") +
  labs(title = 'Scatter Plot of Stroke by BMI and Average Glucose Level',
       x = 'Body Mass Index (BMI)',
       y = 'Average Glucose Level',
       color = 'Stroke Status') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate the percentages of each smoking status within each stroke status
smoking_percentage_data <- brain_stroke %>%
  group_by(stroke, smoking_status) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)

# Bar plot of smoking status and stroke
ggplot(smoking_percentage_data, aes(x = factor(stroke, labels = c("No Stroke", "Stroke")), 
                                    y = percentage, fill = smoking_status)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Distribution of Stroke by Smoking Status",
       x = "Stroke Status",
       y = "Proportion",
       fill = "Smoking Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create a contingency table for smoking status and stroke occurrence
smoking_stroke_tab <- table(brain_stroke$smoking_status, brain_stroke$stroke)
# Perform the Chi-Squared Test
smoking_stroke_test <- chisq.test(smoking_stroke_tab)
# Extract results and create a table for presentation
smoking_stroke_results <- data.frame(
  'Chi-Squared Statistic' = smoking_stroke_test$statistic,
  'Degrees of Freedom' = smoking_stroke_test$parameter,
  'P-Value' = smoking_stroke_test$p.value)
# Use kable from knitr to create a nicely formatted table
kable(smoking_stroke_results, 
      caption = "Chi-Squared Test Results for Stroke by Smoking Status", 
      align = 'c')