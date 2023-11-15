setwd("Desktop/PSU_C/2023 Fall/STAT_184/FinalProject/Data/")
brain_stroke <- read.csv("brain_stroke.csv")
head(brain_stroke)
tail(brain_stroke)
summary(brain_stroke)

library(ggplot2)
library(dplyr)

# Assuming 'gender' and 'stroke' are factors
# Convert them if they are not
brain_stroke$gender <- factor(brain_stroke$gender, levels = c("Female", "Male"))
brain_stroke$stroke <- factor(brain_stroke$stroke, levels = c("0", "1"))

# Create a summary table
summary_table <- brain_stroke %>%
  group_by(gender, stroke) %>%
  summarise(count = n(), .groups = "drop")

# Bar plot for gender and stroke with counts and custom labels
ggplot(brain_stroke, aes(x = gender, fill = stroke)) +
  geom_bar(position = "dodge") +
  geom_text(data = summary_table, 
            aes(x = gender, y = count, label = count, group = stroke),
            position = position_dodge(width = 0.9), vjust = -0.25) +
  scale_fill_manual(values = c("grey", "red"),
                    labels = c("No Stroke", "Stroke"),
                    name = "Stroke") +
  labs(title = "Distribution of Stroke by Gender",
       x = "Gender",
       y = "Count") +
  theme_minimal()


# Box plot for age and stroke with custom labels
ggplot(brain_stroke, aes(x = stroke, y = age, fill = stroke)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey", "red"),
                    labels = c("No Stroke", "Stroke"),
                    name = "Stroke") +
  labs(title = "Boxplot of Age by Stroke Status",
       x = "Stroke Status",
       y = "Age") +
  theme_minimal()


library(dplyr)

# Summary statistics for age
summary(brain_stroke$age)

# Cross-tabulation of gender and stroke
table(brain_stroke$gender, brain_stroke$stroke)

# Boxplot of age by gender and stroke
ggplot(brain_stroke, aes(x = gender, y = age, fill = stroke)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey", "red"),
                    labels = c("No Stroke", "Stroke"),
                    name = "Stroke") +
  labs(title = "Boxplot of Age by Gender and Stroke Status",
       x = "Gender",
       y = "Age") +
  theme_minimal()




# Assuming 'hypertension' and 'stroke' are factors
# If not, you can uncomment the following lines to convert them
# brain_stroke$hypertension <- factor(brain_stroke$hypertension, levels = c("0", "1"))
# brain_stroke$stroke <- factor(brain_stroke$stroke, levels = c("0", "1"))

# Create a summary table for counts
summary_table <- brain_stroke %>%
  group_by(hypertension, stroke) %>%
  summarise(count = n(), .groups = "drop")

# Bar plot with counts on top and custom labels
ggplot(brain_stroke, aes(x = hypertension, fill = stroke)) +
  geom_bar(position = "dodge") +
  geom_text(data = summary_table, 
            aes(x = hypertension, y = count, label = count, group = stroke),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = c("grey", "red"), 
                    labels = c("No Stroke", "Stroke"),
                    name = "Stroke") +
  scale_x_discrete(labels = c("No Hypertension", "Hypertension"),
                   name = "Hypertension") +
  labs(title = "Relationship Between Hypertension and Stroke",
       x = "",
       y = "Count") +
  theme_minimal()




# Assuming 'heart_disease' and 'stroke' are factors
# If not, convert them
brain_stroke$heart_disease <- factor(brain_stroke$heart_disease, levels = c("0", "1"))
brain_stroke$stroke <- factor(brain_stroke$stroke, levels = c("0", "1"))

# Create a summary table for counts
summary_table <- brain_stroke %>%
  group_by(heart_disease, stroke) %>%
  summarise(count = n(), .groups = "drop")

# Bar plot with counts on top and custom labels
ggplot(brain_stroke, aes(x = heart_disease, fill = stroke)) +
  geom_bar(position = "dodge") +
  geom_text(data = summary_table, 
            aes(x = heart_disease, y = count, label = count, group = stroke),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = c("grey", "red"), 
                    labels = c("No Stroke", "Stroke"),
                    name = "Stroke") +
  scale_x_discrete(labels = c("No Heart Disease", "Heart Disease"),
                   name = "Heart Disease") +
  labs(title = "Relationship Between Heart Disease and Stroke",
       x = "",
       y = "Count") +
  theme_minimal()


summary(brain_stroke$hypertension)
summary(brain_stroke$heart_disease)
table(brain_stroke$hypertension, brain_stroke$heart_disease, brain_stroke$stroke)

library(ggplot2)
library(ggmosaic)

# Convert factors if they are not already
brain_stroke$hypertension <- factor(brain_stroke$hypertension, levels = c("0", "1"), labels = c("No Hypertension", "Hypertension"))
brain_stroke$heart_disease <- factor(brain_stroke$heart_disease, levels = c("0", "1"), labels = c("No Heart Disease", "Heart Disease"))
brain_stroke$stroke <- factor(brain_stroke$stroke, levels = c("0", "1"), labels = c("No Stroke", "Stroke"))


library(ggplot2)

# Assuming that the factors are correctly set with no NA values

# Plot 1: Heart Disease and Hypertension Distribution by Stroke Status
ggplot(brain_stroke, aes(x = heart_disease, fill = hypertension)) +
  geom_bar(position = "dodge") +
  facet_grid(~ stroke) +
  scale_fill_manual(values = c("No Hypertension" = "grey", "Hypertension" = "red")) +
  labs(title = "Heart Disease and Hypertension Distribution by Stroke Status",
       x = "Heart Disease",
       y = "Count",
       fill = "Hypertension Status") +
  theme_minimal()



library(ggplot2)
library(dplyr)

# Calculate the counts for each combination of hypertension and heart_disease
counts <- brain_stroke %>%
  count(hypertension, heart_disease, stroke) %>%
  group_by(hypertension, stroke) %>%
  mutate(perc = n / sum(n))

# Plot 2: Proportion of Heart Disease within Hypertension Categories by Stroke Status
ggplot(brain_stroke, aes(x = hypertension, fill = heart_disease)) +
  geom_bar(position = "fill") +
  geom_text(data = counts, aes(label = scales::percent(perc), y = perc), 
            position = position_fill(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("No Heart Disease" = "grey", "Heart Disease" = "orange")) +
  facet_grid(~ stroke) +
  labs(title = "Proportion of Heart Disease within Hypertension Categories by Stroke Status",
       x = "Hypertension",
       y = "Proportion",
       fill = "Heart Disease Status") +
  theme_minimal()



