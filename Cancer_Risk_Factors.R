getwd() 
setwd("/Users/kaylalu/Desktop/R project/2_Cancer Factors")
rm(BRCA_propotion, Family_History_propotion, )

#Data Preparation
library(tidyverse)
library(ggplot2)
cancer_data <- read_csv("cancer-risk-factors.csv")

##Data Inspection
head(cancer_data)
str(cancer_data)
summary(cancer_data)
table(cancer_data$Cancer_Type)
table(cancer_data$Risk_Level)

colSums(is.na(cancer_data))

##Data Cleaning
cancer_data <- cancer_data %>%
  mutate(
    Cancer_Type = factor(Cancer_Type),
    Gender = factor(Gender),
    Family_History = factor(Family_History),
    BRCA_Mutation = factor(BRCA_Mutation),
    H_Pylori_Infection = factor(H_Pylori_Infection),
    Risk_Level = factor(
      Risk_Level,
      levels = c("Low", "Medium", "High"),
    ordered = TRUE
  )
)

str(cancer_data)
num_vars <- cancer_data %>%
  select(where(is.numeric))
colnames(num_vars)
summary(num_vars)

cat_vars <- cancer_data %>%
  select(where(is.factor))
colnames(cat_vars)

#Data Visualisations and Analysis
##Sample Overview
###Age
ggplot(cancer_data, aes(x = Age)) +
  geom_histogram(fill = "skyblue", color = "white") +
  labs(title = "Age Distribution",
       x = "Age", y = "Count") +
  theme_minimal()

###Gender
ggplot(cancer_data, aes(x = factor(Gender, levels = c(0,1), labels = c("Female","Male")))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Gender Distribution",
       x = "Gender", y = "Count") +
  theme_minimal()

###BMI
ggplot(cancer_data, aes(x = BMI)) +
  geom_histogram(fill = "skyblue", color = "white") +
  labs(title = "BMI Distribution",
       x = "BMI", y = "Count") +
  theme_minimal()
  
###Cancer Type Distribution
ggplot(cancer_data, aes(x = Cancer_Type)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Cancer Type Distribution",
       x = "Cancer Type", y = "Count") +
  theme_minimal()

###Risk Level Distribution
ggplot(cancer_data, aes(x = Risk_Level)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Risk Level Distribition",
       x = "Risk Level", y = "Count") +
  theme_minimal()

##Core Analysis(Relationships with Cancer)
###Define colors
risk_colors <- c(
  "Low" = "lightgreen",
  "Medium" = "khaki",
  "High" = "lightcoral"
)
scale_fill_manual(values = risk_colors)

##Demographic Factors
###Age vs Risk Level
ggplot(cancer_data, aes(x = Risk_Level, y = Age, fill = Risk_Level)) +
  geom_boxplot() +
  scale_fill_manual(values = risk_colors) +
  labs(title = "Age by Risk Level",
       x = "Risk Level", y = "Age") +
  theme_minimal() +
  theme(legend.position = "none")

###Age vs Cancer Type
ggplot(cancer_data, aes(x = Cancer_Type, y = Age)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Age by Cancer Type",
         x = "Cancer Types", y = "Age") +
  theme_minimal()
###Gender vs Risk Level
ggplot(cancer_data, aes(x = Risk_Level, fill = factor(Gender, levels = c(0,1), labels = c("Female", "Male")))) +
  geom_bar(position = "dodge") +
  labs(title = "Gender by Risk Level",
       x = "Risk Level", y = "Count",
       fill = "Gender") +
  theme_minimal() 

###Gender vs Cancer Type
ggplot(cancer_data, aes(x = Cancer_Type, fill = factor(Gender, levels = c(0,1), labels = c("Female", "Male")))) +
  geom_bar(position = "dodge") +
  labs(title = "Gender by Cancer Type",
       x = "Cancer Types", y = "Count",
      fill = "Gender") +
  theme_minimal()


##Lifestyle Factors
###Smoking vs Cancer Type
ggplot(cancer_data, aes(x = Cancer_Type, y = Smoking)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Smoking Level by Cancer Type",
       x = "Cancer Type", y = "Smoking Level") +
  theme_minimal()

###Smoking vs Risk Level
ggplot(cancer_data, aes(x = Risk_Level, y = Smoking, fill = Risk_Level)) +
  geom_boxplot() +
  scale_fill_manual(values = risk_colors) +
  labs(title = "Smoking by Risk Level",
       x = "Risk Level", y = "Smoking") +
  theme_minimal() +
  theme(legend.position = "none")
  
###Alcohol vs Risk Level       
ggplot(cancer_data, aes(x = Risk_Level, y = Alcohol_Use, fill = Risk_Level)) +
  geom_boxplot() +
  scale_fill_manual(values = risk_colors) +
  labs(title = "Alcohol use by Risk Level",
       x = "Risk Level", y = "Alcohol Use") +
  theme_minimal() +
  theme(legend.position = "none")

###Physical Activity Level by Risk Level 
ggplot(cancer_data, aes(x = Risk_Level, y = Physical_Activity_Level, fill = Risk_Level)) +
  geom_boxplot() +
  scale_fill_manual(values = risk_colors) +
  labs(title = "Physical Activity Level by Risk Level",
       x = "Risk Level", y = "Physical Activity Level") +
  theme_minimal() +
  theme(legend.position = "none")

###Red Meat Consumption by Risk Level 
ggplot(cancer_data, aes(x = Risk_Level, y = Diet_Red_Meat, fill = Risk_Level)) +
  geom_boxplot() +
  scale_fill_manual(values = risk_colors) +
  labs(title = "Red Meat Consumption by Risk Level",
       x = "Risk Level", y = "Red Meat Consumption") +
  theme_minimal() +
  theme(legend.position = "none")

###Salted/Processed Food Consumption by Risk Level 
ggplot(cancer_data, aes(x = Risk_Level, y = Diet_Salted_Processed, fill = Risk_Level)) +
  geom_boxplot() +
  scale_fill_manual(values = risk_colors) +
  labs(title = "Salted/Processed Food Consumption by Risk Level",
       x = "Risk Level", y = "Salted/Processed Food Consumption") +
  theme_minimal() +
  theme(legend.position = "none")

###BMI by Risk Level 
ggplot(cancer_data, aes(x = Risk_Level, y = BMI, fill = Risk_Level)) +
  geom_boxplot() +
  scale_fill_manual(values = risk_colors) +
  labs(title = "BMI by Risk Level",
       x = "Risk Level", y = "BMI") +
  theme_minimal() +
  theme(legend.position = "none")

##Environmental Factors
###Air pollution
ggplot(cancer_data, aes(x = Risk_Level, y = Air_Pollution, fill = Risk_Level)) +
  geom_boxplot() +
  scale_fill_manual(values = risk_colors) +
  labs(title = "Air Pollution Risk Level",
       x = "Risk Level", y = "Air Pollution Level") +
  theme_minimal() +
  theme(legend.position = "none")

###Occupational hazard
ggplot(cancer_data, aes(x = Risk_Level, y = Occupational_Hazards, fill = Risk_Level)) +
  geom_boxplot() +
  scale_fill_manual(values = risk_colors) +
  labs(title = "Occupational Hazards by Risk Level",
       x = "Risk Level", y = "Occupational Hazards") +
  theme_minimal() +
  theme(legend.position = "none")

##Genetic/Medical Factors
###BRCA_Mutation
BRCA_percentage <- cancer_data %>%
  count(Risk_Level, BRCA_Mutation) %>%
  group_by(Risk_Level) %>%
  mutate(percentage = n / sum(n)* 100)

ggplot(BRCA_percentage, aes(x = Risk_Level, y = percentage, fill = factor(BRCA_Mutation, levels = c(0,1), labels = c("No","Yes")))) +
  geom_col() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("No" = "lightgreen", "Yes" = "lightcoral")) +
  labs(title = "Percentage of BRCA Mutation by Risk Level",
       x = "Risk Level", y = "Percentage",
       fill = "BRCA_Mutation") +
  theme_minimal()

###Family_History
Family_History_percentage <- cancer_data %>%
  count(Risk_Level, Family_History) %>%
  group_by(Risk_Level) %>%
  mutate(percentage = n / sum(n)* 100)

ggplot(Family_History_percentage, aes(x = Risk_Level, y = percentage, fill = factor(Family_History, levels = c(0,1), labels = c("No", "Yes")))) +
  geom_col() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("No" = "lightgreen", "Yes" = "lightcoral")) +
  labs(title = "Percentage of Family History by Risk Level",
       x = "Risk Level", y = "Percentage",
       fill = "Family History") +
  theme_minimal()

###H_Pylori_Infection
H_Pylori_percentage <- cancer_data %>%
  count(Risk_Level, H_Pylori_Infection) %>%
  group_by(Risk_Level) %>%
  mutate(percentage = n / sum(n)*100)

ggplot(H_Pylori_percentage, aes(x = Risk_Level, y = percentage, fill = factor(H_Pylori_Infection, levels = c(0,1), labels = c("No", "Yes")))) +
  geom_col() +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = c("No" = "lightgreen", "Yes" = "lightcoral")) +
  labs(title = "Percentage of H. Pylori Infection by Risk Level",
       x = "Risk Level", y = "Percentage",
       fill = "H. Pylori") +
  theme_minimal()

