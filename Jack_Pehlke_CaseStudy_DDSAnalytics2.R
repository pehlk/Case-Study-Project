library(dplyr)
library(tidyr)
library(stringi)
library(rvest) #html_table, html_node
library(ggplot2)
library(RCurl) #getURL
library(magrittr)
library(GGally)
library(tidyverse)
library(plotly)
library(naniar)
library(stringr)
library(mapproj)
library(class)
library(caret)
library(e1071)

df = read.csv(file.choose(),header = TRUE)
df

#Building Combined Datasets

BT_G = table(df$BusinessTravel, df$Gender)
males_travel_frequently <- D["Travel_Frequently", "Male"]
males_travel_frequently

A_BT_P = t(t(D)/colSums(D))
A_BT_P

#Variables and Attrition

A_Age = table(df$Attrition, df$Age)
A_BusinessTravel = table(df$Attrition, df$BusinessTravel)
A_DailyRate = table(df$Attrition, df$DailyRate)
A_Department = table(df$Attrition, df$Department)
A_DistanceFromHome = table(df$Attrition, df$DistanceFromHome)
A_Education = table(df$Attrition, df$Education)
A_EducationField = table(df$Attrition, df$EducationField)
A_EmployeeCount = table(df$Attrition, df$EmployeeCount)
A_EmployeeNumber = table(df$Attrition, df$EmployeeNumber)
A_EnvironmentSatisfaction = table(df$Attrition, df$EnvironmentSatisfaction)
A_Gender = table(df$Attrition, df$Gender)
A_HourlyRate = table(df$Attrition, df$HourlyRate)
A_JobInvolvement = table(df$Attrition, df$JobInvolvement)
A_JobLevel = table(df$Attrition, df$JobLevel)
A_JobRole = table(df$Attrition, df$JobRole)
A_JobSatisfaction = table(df$Attrition, df$JobSatisfaction)
A_MaritalStatus = table(df$Attrition, df$MaritalStatus)
A_MonthlyIncome = table(df$Attrition, df$MonthlyIncome)
A_MonthlyRate = table(df$Attrition, df$MonthlyRate)
A_NumCompaniesWorked = table(df$Attrition, df$NumCompaniesWorked)
A_Over18 = table(df$Attrition, df$Over18)
A_OverTime = table(df$Attrition, df$OverTime)
A_PercentSalaryHike = table(df$Attrition, df$PercentSalaryHike)
A_PerformanceRating = table(df$Attrition, df$PerformanceRating)
A_RelationshipSatisfaction = table(df$Attrition, df$RelationshipSatisfaction)
A_StandardHours = table(df$Attrition, df$StandardHours)
A_StockOptionLevel = table(df$Attrition, df$StockOptionLevel)
A_TotalWorkingYears = table(df$Attrition, df$TotalWorkingYears)
A_TrainingTimesLastYear = table(df$Attrition, df$TrainingTimesLastYear)
A_WorkLifeBalance = table(df$Attrition, df$WorkLifeBalance)
A_YearsAtCompany = table(df$Attrition, df$YearsAtCompany)
A_YearsInCurrentRole = table(df$Attrition, df$YearsInCurrentRole)
A_YearsSinceLastPromotion = table(df$Attrition, df$YearsSinceLastPromotion)
A_YearsWithCurrManager = table(df$Attrition, df$YearsWithCurrManager)

#Categorical:
A_BusinessTravel
A_Department
A_Education
A_EducationField
A_EmployeeCount
A_EnvironmentSatisfaction
A_Gender
A_JobInvolvement
A_JobLevel
A_JobRole
A_JobSatisfaction
A_MaritalStatus
A_NumCompaniesWorked
A_Over18
A_OverTime
A_PerformanceRating
A_RelationshipSatisfaction
A_StandardHours
A_StockOptionLevel
A_TrainingTimesLastYear
A_WorkLifeBalance
A_YearsInCurrentRole
A_YearsSinceLastPromotion
A_YearsWithCurrManager

Per_A_A = t(t(A_Age) / colSums (A_Age))
Per_A_BT = t(t(A_BusinessTravel) / colSums(A_BusinessTravel))
Per_A_D = t(t(A_Department) / colSums(A_Department))
Per_A_E = t(t(A_Education) / colSums(A_Education))
Per_A_EF = t(t(A_EducationField) / colSums(A_EducationField))
Per_A_EC = t(t(A_EmployeeCount) / colSums(A_EmployeeCount))
Per_A_ES = t(t(A_EnvironmentSatisfaction) / colSums(A_EnvironmentSatisfaction))
Per_A_G = t(t(A_Gender) / colSums(A_Gender))
Per_A_JI = t(t(A_JobInvolvement) / colSums(A_JobInvolvement))
Per_A_JL = t(t(A_JobLevel) / colSums(A_JobLevel))
Per_A_JR = t(t(A_JobRole) / colSums(A_JobRole))
Per_A_JS = t(t(A_JobSatisfaction) / colSums(A_JobSatisfaction))
Per_A_MS = t(t(A_MaritalStatus) / colSums(A_MaritalStatus))
Per_A_NCW = t(t(A_NumCompaniesWorked) / colSums(A_NumCompaniesWorked))
Per_A_O18 = t(t(A_Over18) / colSums(A_Over18))
Per_A_OT = t(t(A_OverTime) / colSums(A_OverTime))
Per_A_PR = t(t(A_PerformanceRating) / colSums(A_PerformanceRating))
Per_A_RS = t(t(A_RelationshipSatisfaction) / colSums(A_RelationshipSatisfaction))
Per_A_SH = t(t(A_StandardHours) / colSums(A_StandardHours))
Per_A_SOL = t(t(A_StockOptionLevel) / colSums(A_StockOptionLevel))
Per_A_TTLY = t(t(A_TrainingTimesLastYear) / colSums(A_TrainingTimesLastYear))
Per_A_WLB = t(t(A_WorkLifeBalance) / colSums(A_WorkLifeBalance))
Per_A_YIC = t(t(A_YearsInCurrentRole) / colSums(A_YearsInCurrentRole))
Per_A_YSLP = t(t(A_YearsSinceLastPromotion) / colSums(A_YearsSinceLastPromotion))
Per_A_YWCM = t(t(A_YearsWithCurrManager) / colSums(A_YearsWithCurrManager))

#Categorical
Per_A_BT
Per_A_D
Per_A_E
Per_A_EF
Per_A_EC
Per_A_ES
Per_A_G
Per_A_JI
Per_A_JL
Per_A_JR
Per_A_JS
Per_A_MS
Per_A_NCW
Per_A_O18
Per_A_OT
Per_A_PR
Per_A_RS
Per_A_SH
Per_A_SOL
Per_A_TTLY
Per_A_WLB

#COMBINING TOP SUBCATEGORIES/VARIABLES

Involvement_WLB = table(df$JobInvolvement, df$WorkLifeBalance)
LowWLB_LowJI <- Involvement_WLB["3", "3"]
LowWLB_LowJI

BT_G = table(df$BusinessTravel, df$Gender)
males_travel_frequently <- BT_G["Travel_Frequently", "Male"]
males_travel_frequently



df <- df %>%
  mutate(OverallSatisfaction = as.numeric(JobSatisfaction + RelationshipSatisfaction + EnvironmentSatisfaction + WorkLifeBalance) / 4)
df$OverallSatisfaction

df <- df %>%
  mutate(OverallDistance = as.numeric(DistanceFromHome, BusinessTravel) / 2)
df$OverallDistance

df <- df %>%
  mutate(OverallRate = as.numeric(HourlyRate, MonthlyRate) / 2)
df$OverallRate

df <- df %>%
  mutate(OverallCompensation = as.numeric(Standardized_StockOptionLevel, Standardized_PercentSalaryHike) /2)
df$OverallCompensation

ggplot(df, aes(x = Attrition, y = OverallSatisfaction)) +
  geom_violin(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Satisfaction Scores by Job Role",
       x = "Job Role",
       y = "Satisfaction Score") +
  theme_minimal()

#I'm so confused
OT_JI = table(df$OverTime, df$JobInvolvement)
OT_JI
Per_JI_OT = t(t(A_OS) / colSums (A_OS))
Per_JI_OT
HOT_LJI <- JI_OT["Yes", "1"]
HOT_LJI
df <- df %>%
  mutate(JI_OT = HOT_LJI)
df$JI_OT

#Continuous:
#Per_A_A (Age)
#Per_A_DR (DailyRate)
#Per_A_DF (DistanceFromHome)
#Per_A_EN (EmployeeNumber)
#Per_A_HR (HourlyRate)
#Per_A_MI (MonthlyIncome)
#Per_A_MR (MonthlyRate)
#Per_A_PS (PercentSalaryHike)
#Per_A_TW (TotalWorkingYears)
#Per_A_YC (YearsAtCompany)
#Per_A_YIC (YearsInCurrentRole)
#Per_A_YSLP (YearsSinceLastPromotion)
#Per_A_YWCM (YearsWithCurrManager)

df <- df %>%
  select(-c( OneHot_TravelLow, OneHot_TravelMedium, OneHot_TravelHigh))
df
              



# Create the combined feature
df$Dept_Travel <- interaction(df$Department, df$BusinessTravel)

df$Dept_Travel <- as.factor(df$Dept_Travel)
library(dplyr)
library(tidyr)

# One-hot encode the combined feature
df_combined <- df %>%
  mutate(Dept_Travel = as.character(Dept_Travel)) %>%
  pivot_wider(names_from = Dept_Travel, values_from = Dept_Travel, values_fill = list(Dept_Travel = 0), values_fn = length)

# If you want to keep the original dataframe with the new features
df <- bind_cols(df, df_combined)
df <- df %>%
  select(-c(Department, BusinessTravel))
# Scale numeric features
numeric_cols <- sapply(df, is.numeric)
df[numeric_cols] <- scale(df[numeric_cols])



                                           