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
library(ggthemes)

df = read.csv(file.choose(),header = TRUE)
df

#EDA

df$age_group <- cut(df$Age, 
                      breaks = c(18, 28, 39, 50, 61, Inf), 
                      labels = c("18-28", "29-39", "40-50", "51-61", "62+"), 
                      right = FALSE)

#GGGalley
df %>% select(Department, WorkLifeBalance, Attrition, Gender) %>%
  ggpairs(mapping=aes(color = Attrition)) + theme_solarized()

#Attrition by Business Travel
df %>%
  ggplot(mapping=aes(fill=Attrition, x=BusinessTravel)) +
  geom_bar() + 
  labs(title="Attrition Rates by Business Travel") + theme_solarized() + xlab("Business Travel") + 
  ylab ("Percentage")

#Attrition by Department
df %>%
  ggplot(mapping=aes(x=Attrition, fill=Department)) +
  geom_bar(position = "fill") +
  labs(title="Attrition Rates by Department")

#Age Distribution by Attrition
df %>%
  ggplot(mapping=aes(x=Age, fill=Attrition)) +
  geom_histogram(binwidth=5, position="fill", alpha=0.5) +
  labs(title="Age Distribution by Attrition Status")

#Distance from Home vs. Attrition
df %>%
  ggplot(mapping=aes(x=DistanceFromHome, fill=Attrition)) +
  geom_bar(position="dodge") +
  labs(title="Distance from Home by Attrition") + ylab("Count of")

#Job Satisfaction vs. Attrition
df %>%
  ggplot(mapping=aes(x=JobSatisfaction, fill=Attrition)) +
  geom_bar(position="dodge") +
  labs(title="Job Satisfaction Levels by Attrition")

#Education Level and Attrition
df %>%
  ggplot(mapping=aes(x=Education, fill=Attrition)) +
  geom_bar(position="dodge") +
  labs(title="Attrition Rates by Education Level")

#Work-Life Balance vs. Attrition
df %>%
  ggplot(mapping=aes(x=WorkLifeBalance, fill=Attrition)) +
  geom_bar(position="fill") +
  labs(title="Work-Life Balance by Attrition Status")

#Performance Rating by Attrition
df %>%
  ggplot(mapping=aes(x=PerformanceRating, fill=Attrition)) +
  geom_bar(position="dodge") +
  labs(title="Performance Ratings by Attrition Status")

#Years at Company vs. Attrition
df %>%
  ggplot(mapping=aes(x=YearsAtCompany, fill=Attrition)) +
  geom_bar(position="fill") +
  labs(title="Years at Company by Attrition")

#Job Role vs. Attrition
df %>%
  ggplot(mapping=aes(x=JobRole, fill=Attrition)) +
  geom_bar(position="dodge") +
  labs(title="Attrition Rates by Job Role")

#Monthly Income vs. Attrition
df %>%
  ggplot(mapping=aes(x=MonthlyIncome, fill=Attrition)) +
  geom_histogram(bins=30, position="fill", alpha=0.5) +
  labs(title="Monthly Income Distribution by Attrition Status")


#Overtime Impact on Attrition
df %>%
  ggplot(mapping=aes(x=OverTime, fill=Attrition)) +
  geom_bar(position="fill") +
  labs(title="Attrition Rates with Overtime Consideration")

#Training Times Last Year vs. Attrition
df %>%
  ggplot(mapping=aes(x=TrainingTimesLastYear, fill=Attrition)) +
  geom_bar(position="fill") +
  labs(title="Training Times Last Year and Attrition Rates")

#Gender and Attrition Rates
df %>%
  ggplot(mapping=aes(x=Gender, fill=Attrition)) +
  geom_bar(position="dodge") +
  labs(title="Attrition Rates by Gender")

#Num Companies Worked vs. Attrition
df %>%
  ggplot(mapping=aes(x=NumCompaniesWorked, fill=Attrition)) +
  geom_bar(position="dodge") +
  labs(title="Number of Companies Worked vs. Attrition Rates")
                                                                                                    
#TILE PLOT: Department/Work-Life Balance + highest rates of attrition
a = df %>% count(Department, WorkLifeBalance) %>%
  ggplot(aes(x = Department, y = WorkLifeBalance)) +
  geom_tile(mapping = aes(fill = n))
ggplotly(a)



#Turning the Attrition y/n into 1/0
df = df %>%
  mutate(BinaryAttrition = ifelse(Attrition == "Yes", 1, 0))
df$BinaryAttrition
                                          

#Categorical;
df$BusinessTravel
df$Department
df$EducationField
df$Gender
df$JobRole
df$MaritalStatus
df$OverTime

#Making Categorical Binary/Numeric
# Convert BusinessTravel to numeric
df$PrenumericBusinessTravel = factor(df$BusinessTravel)
df$NumericBusinessTravel = as.numeric(df$PrenumericBusinessTravel)

# Convert Department to numeric
df$PrenumericDepartment = factor(df$Department)
df$NumericDepartment = as.numeric(df$PrenumericDepartment)

# Convert EducationField to numeric
df$PrenumericEducationField = factor(df$EducationField)
df$NumericEducationField = as.numeric(df$PrenumericEducationField)

# Convert Gender to binary
df <- df %>%
  mutate(BinaryGender = ifelse(Gender == "Female", 1, 0))

# Convert JobRole to numeric
df$PrenumericJobRole = factor(df$JobRole)
df$NumericJobRole = as.numeric(df$PrenumericJobRole)

# Convert MaritalStatus to numeric
df$PrenumericMaritalStatus = factor(df$MaritalStatus)
df$NumericMaritalStatus = as.numeric(df$PrenumericMaritalStatus)

# Convert OverTime to binary
df <- df %>%
  mutate(BinaryOverTime = ifelse(OverTime == "Yes", 1, 0))

df

#Determining the Rate of Attrition across Variables
#Determining which columns only have one value
as.list(df$StandardHours)
categories <- unique(df$StandardHours) 
numberOfCategories <- length(categories)
categories
numberOfCategories


as.list(df$EmployeeCount)
categories <- unique(df$EmployeeCount) 
numberOfCategories <- length(categories)
categories
numberOfCategories

#List of Continuous
#c("Cor_Age", "Cor_DailyRate", "Cor_DistanceFromHome", "Cor_Education",
#  "Cor_EmployeeNumber", "Cor_EnvironmentSatisfaction", "Cor_HourlyRate",
#  "Cor_JobInvolvement", "Cor_JobLevel", "Cor_JobSatisfaction",
#  "Cor_MonthlyIncome", "Cor_MonthlyRate", "Cor_NumCompaniesWorked",
#  "Cor_PercentSalaryHike", "Cor_PerformanceRating", "Cor_RelationshipSatisfaction",
#  "Cor_StockOptionLevel", "Cor_TotalWorkingYears", "Cor_TrainingTimesLastYear",
#  "Cor_WorkLifeBalance", "Cor_YearsAtCompany", "Cor_YearsInCurrentRole",
#  "Cor_YearsSinceLastPromotion", "Cor_YearsWithCurrManager")

#Standardizing Continuous Variables
variables_to_standardize <- c("Age", "DailyRate", "DistanceFromHome", "Education",
                              "EmployeeNumber", "EnvironmentSatisfaction", "HourlyRate",
                              "JobInvolvement", "JobLevel", "JobSatisfaction",
                              "MonthlyIncome", "MonthlyRate", "NumCompaniesWorked",
                              "PercentSalaryHike", "PerformanceRating", "RelationshipSatisfaction",
                              "StockOptionLevel", "TotalWorkingYears", "TrainingTimesLastYear",
                              "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole",
                              "YearsSinceLastPromotion", "YearsWithCurrManager", )

for (var in variables_to_standardize) {
  standardized_var_name <- paste0("Standardized_", var)
  df[[standardized_var_name]] <- scale(df[[var]])
}
df

#Correlation of Continuous Standardized Variables and Attrition
#Correlation Coefficient with Attrition will show us the strength of the linear relationship to Attrition rate. Ideally, this will demonstrate a variable's connection to the rate of attrition

standardized_variables <- c("Standardized_Age",
  "Standardized_DailyRate",
  "Standardized_DistanceFromHome",
  "Standardized_Education",
  "Standardized_EmployeeNumber",
  "Standardized_EnvironmentSatisfaction",
  "Standardized_HourlyRate",
  "Standardized_JobInvolvement",
  "Standardized_JobLevel",
  "Standardized_JobSatisfaction",
  "Standardized_MonthlyIncome",
  "Standardized_MonthlyRate",
  "Standardized_NumCompaniesWorked",
  "Standardized_PercentSalaryHike",
  "Standardized_PerformanceRating",
  "Standardized_RelationshipSatisfaction",
  "Standardized_StockOptionLevel",
  "Standardized_TotalWorkingYears",
  "Standardized_TrainingTimesLastYear",
  "Standardized_WorkLifeBalance",
  "Standardized_YearsAtCompany",
  "Standardized_YearsInCurrentRole",
  "Standardized_YearsSinceLastPromotion",
  "Standardized_YearsWithCurrManager")

Standardized_Correlation_Values <- data.frame(Variable = character(),
                                              Correlation = numeric(),
                                              stringsAsFactors = FALSE)

#Loop through each standardized variable to calculate correlations
for (var in standardized_variables) {
  standardized_Correlation_Value <- paste0("Cor_", var)
  
#Calculate the correlation
  correlation_value <- cor(df$BinaryAttrition, df[[var]], use = "complete.obs")
  
#Append the results to the data frame
  Standardized_Correlation_Values = rbind(Standardized_Correlation_Values, 
                                           data.frame(Variable = standardized_Correlation_Value, 
                                                      Correlation = correlation_value))
}

# View the correlation results
str(Standardized_Correlation_Values)


Standardized_Correlation_Values = Standardized_Correlation_Values%>%arrange(desc)

# Create the line graph
Standardized_Correlation_Values %>% ggplot(mapping = aes(y = Variable, x = Correlation, fill = Variable)) +
  geom_bar(stat = "identity") + 
  labs(title = "Correlation Values with Binary Attrition",
       y = "Correlation",
       x = "Attributes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

)

scaled_df <- scale(df[, -which(names(df) == "Attrition")])
#THE TOP 3 ARE: OVERTIME, MARITAL STATUS, AND JOB INVOLVEMENT



#Overtime and Attrition
df %>%
  ggplot(mapping=aes(x=OverTime, fill=Attrition)) +
  geom_bar(position = "fill") +
  labs(title="Attrition Rates by Overtime") + ylab("Percentage of Attrition") + facet_grid(vars(OverTime))

#Marital Status and Attrition
df %>%
  ggplot(mapping=aes(x=MaritalStatus, fill=Attrition)) +
  geom_bar(position = "fill") +
  labs(title="Attrition Rates by Marital Status") + ylab("Percentage of Attrition") + facet_grid(vars(MaritalStatus))

#Job Involvement and Attrition
df %>%
  ggplot(mapping=aes(x=JobInvolvement, fill=Attrition)) +
  geom_bar(position = "fill") +
  labs(title="Attrition Rates by Job Involvement") + ylab("Percentage of Attrition") + facet_grid(vars(JobInvolvement))

marital_status_summary <- df %>%
  group_by(Department, MaritalStatus, Attrition) %>%
  summarise(Count = n()) %>%
  ungroup()

marital_status_summary
#Marital Status/Dept and Attrition
df %>%
  ggplot(mapping=aes(x=JobInvolvement, fill=Attrition)) +
  geom_bar(position = "fill") +
  labs(title="Attrition Rates by Job Involvement") + ylab("Percentage of Attrition") + facet_grid(vars(JobInvolvement))

#JOB ROLE TRENDS

#JobRole vs. Attrition
df %>%
  ggplot(mapping=aes(x=JobRole, fill=Attrition)) +
  geom_bar(position="fill") +
  labs(title="Job Role vs. Attrition Rates")


#Environment Satisfaction vs. Job Role
df %>%
  ggplot(mapping=aes(x=JobRole, y=EnvironmentSatisfaction)) +
  geom_boxplot() +
  labs(title="Job Role vs. Environment Satisfaction")
df %>%
  group_by(JobRole) %>%
  summarize(median = median(EnvironmentSatisfaction))

#Job Satisfaction vs. DailyRate
df %>%
  ggplot(mapping=aes(x=JobRole, y=JobSatisfaction)) +
  geom_boxplot() +
  labs(title="Job Role vs. Job Satisfaction")
df %>%
  group_by(JobRole) %>%
  summarize(median = median(JobSatisfaction))


#Job Role vs. Employee Number
df %>%
  ggplot(mapping=aes(x=JobRole, y=EmployeeNumber)) +
  geom_boxplot() +
  labs(title="Job Role vs. Employee Number")
df %>%
  group_by(JobRole) %>%
  summarize(median = median(EmployeeNumber))

#Job Role vs. Job Involvement
df %>%
  ggplot(mapping=aes(x=JobRole, fill=JobInvolvement)) +
  geom_bar() + facet_grid(vars(JobInvolvement))
  labs(title="Job Role vs. Job Involvement")
df %>%
  group_by(JobRole) %>%
  summarize(median = median(JobInvolvement))

#Job Role vs. Marital Status
df %>%
  ggplot(mapping=aes(x=JobRole, fill=MaritalStatus)) +
  geom_bar(position="fill") +
  labs(title="Job Role vs. Marital Status") 
df %>%
  group_by(JobRole) %>%
  summarize(sd = 1 - sd(NumericMaritalStatus))

# Job Role vs. NumCompaniesWorked
df %>%
  ggplot(mapping = aes(x = JobRole, y = NumCompaniesWorked)) +
  geom_boxplot() +
  labs(title = "Job Role vs. Num Companies Worked")
df %>%
  group_by(JobRole) %>%
  summarize(mean = mean(NumCompaniesWorked))

# Job Role vs. OverTime
df %>%
  ggplot(mapping = aes(x = JobRole, fill = OverTime)) +
  geom_bar(position = "fill") +
  labs(title = "Job Role vs. OverTime")

# Job Role vs. PercentSalaryHike
df %>%
  ggplot(mapping = aes(x = JobRole, y = PercentSalaryHike)) +
  geom_boxplot() +
  labs(title = "Job Role vs. Percent Salary Hike")
df %>%
  group_by(JobRole) %>%
  summarize(mean = mean(PercentSalaryHike))

# Job Role vs. Relationship Satisfaction
df %>%
  ggplot(mapping = aes(x = JobRole, y = RelationshipSatisfaction)) +
  geom_boxplot() +
  labs(title = "Job Role vs. Relationship Satisfaction")

# Job Role vs. StockOptionLevel
df %>%
  ggplot(mapping = aes(x = JobRole, y = StockOptionLevel)) +
  geom_boxplot() +
  labs(title = "Job Role vs. Stock Option Level")

# Job Role vs. TotalWorkingYears
df %>%
  ggplot(mapping = aes(x = JobRole, y = TotalWorkingYears)) +
  geom_boxplot() +
  labs(title = "Job Role vs. Total Working Years")

# Job Role vs. TrainingTimesLastYear
df %>%
  ggplot(mapping = aes(x = JobRole, y = TrainingTimesLastYear)) +
  geom_boxplot() +
  labs(title = "Job Role vs. Training Times Last Year")

# Job Role vs. WorkLifeBalance
df %>%
  ggplot(mapping = aes(x = JobRole, y = WorkLifeBalance)) +
  geom_boxplot() 
  labs(title = "Job Role vs. Work Life Balance")

# Job Role vs. YearsAtCompany
df %>%
  ggplot(mapping = aes(x = JobRole, y = YearsAtCompany)) +
  geom_boxplot() +
  labs(title = "Job Role vs. Years At Company")

# Job Role vs. YearsInCurrentRole
df %>%
  ggplot(mapping = aes(x = JobRole, y = YearsInCurrentRole)) +
  geom_boxplot() +
  labs(title = "Job Role vs. Years In Current Role")

# Job Role vs. YearsSinceLastPromotion
df %>%
  ggplot(mapping = aes(x = JobRole, y = YearsSinceLastPromotion)) +
  geom_boxplot() +
  labs(title = "Job Role vs. Years Since Last Promotion")

# Job Role vs. YearsWithCurrManager
df %>%
  ggplot(mapping = aes(x = JobRole, y = YearsWithCurrManager)) +
  geom_boxplot() +
  labs(title = "Job Role vs. Years With Current Manager")

#CONTINGENCY TABLES

# Proportions of Attrition by Age
Age_Table <- prop.table(table(df$Attrition, df$Age), margin = 2)

# Proportions of Attrition by Daily Rate
DailyRate_Table <- prop.table(table(df$Attrition, df$DailyRate), margin = 2)

# Proportions of Attrition by Distance From Home
DistanceFromHome_Table <- prop.table(table(df$Attrition, df$DistanceFromHome), margin = 2)

# Proportions of Attrition by Education Level
Education_Table <- prop.table(table(df$Attrition, df$Education), margin = 2)

# Proportions of Attrition by Employee Number
EmployeeNumber_Table <- prop.table(table(df$Attrition, df$EmployeeNumber), margin = 2)

# Proportions of Attrition by Hourly Rate
HourlyRate_Table <- prop.table(table(df$Attrition, df$HourlyRate), margin = 2)

# Proportions of Attrition by Job Involvement
JobInvolvement_Table <- prop.table(table(df$Attrition, df$JobInvolvement), margin = 2)

# Proportions of Attrition by Job Level
JobLevel_Table <- prop.table(table(df$Attrition, df$JobLevel), margin = 2)

# Proportions of Attrition by Job Satisfaction
JobSatisfaction_Table <- prop.table(table(df$Attrition, df$JobSatisfaction), margin = 2)

# Proportions of Attrition by Monthly Income
MonthlyIncome_Table <- prop.table(table(df$Attrition, df$MonthlyIncome), margin = 2)

# Proportions of Attrition by Monthly Rate
MonthlyRate_Table <- prop.table(table(df$Attrition, df$MonthlyRate), margin = 2)

# Proportions of Attrition by Number of Companies Worked
NumCompaniesWorked_Table <- prop.table(table(df$Attrition, df$NumCompaniesWorked), margin = 2)

# Proportions of Attrition by Percent Salary Hike
PercentSalaryHike_Table <- prop.table(table(df$Attrition, df$PercentSalaryHike), margin = 2)

# Proportions of Attrition by Performance Rating
PerformanceRating_Table <- prop.table(table(df$Attrition, df$PerformanceRating), margin = 2)

# Proportions of Attrition by Relationship Satisfaction
RelationshipSatisfaction_Table <- prop.table(table(df$Attrition, df$RelationshipSatisfaction), margin = 2)

# Proportions of Attrition by Stock Option Level
StockOptionLevel_Table <- prop.table(table(df$Attrition, df$StockOptionLevel), margin = 2)

# Proportions of Attrition by Total Working Years
TotalWorkingYears_Table <- prop.table(table(df$Attrition, df$TotalWorkingYears), margin = 2)

# Proportions of Attrition by Training Times Last Year
TrainingTimesLastYear_Table <- prop.table(table(df$Attrition, df$TrainingTimesLastYear), margin = 2)

# Proportions of Attrition by Work-Life Balance
WorkLifeBalance_Table <- prop.table(table(df$Attrition, df$WorkLifeBalance), margin = 2)

# Proportions of Attrition by Years at Company
YearsAtCompany_Table <- prop.table(table(df$Attrition, df$YearsAtCompany), margin = 2)

# Proportions of Attrition by Years in Current Role
YearsInCurrentRole_Table <- prop.table(table(df$Attrition, df$YearsInCurrentRole), margin = 2)

# Proportions of Attrition by Years Since Last Promotion
YearsSinceLastPromotion_Table <- prop.table(table(df$Attrition, df$YearsSinceLastPromotion), margin = 2)

# Proportions of Attrition by Years with Current Manager
YearsWithCurrManager_Table <- prop.table(table(df$Attrition, df$YearsWithCurrManager), margin = 2)



# Print column names with their corresponding numbers
for (i in seq_along(colnames(df))) {
  cat(i, ":", colnames(df)[i], "\n")
}

#CLASSIFICATION MODEL

#k-NN: evaluating the model 
comp = read.csv(file.choose(),header = TRUE)
set.seed(6)
splitPerc = .75 


trainIndices = sample(1:dim(df)[1],round(splitPerc * dim(df)[1]))
train = df[trainIndices,] 
test = df[-trainIndices,] 
dim(test)
df$MV = as.numeric(as.numeric(factor(df$MaritalStatus)))

# k = 13
classifications = knn(train[,c(51, 2, 62)],test[,c(51, 2, 62)],train$Attrition, prob = TRUE, k = 13)
classifications #25 probabilities and assignments - right number for each data point
table(classifications,test$Attrition)
confusionMatrix(table(classifications,test$Attrition))

df


#Determining best k value
iterations = 500
numks = 30

masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(30), k = numeric(30))
  trainIndices = sample(1:dim(df)[1],round(splitPerc * dim(df)[1]))
  train = df[trainIndices,]
  test = df[-trainIndices,] #comp data here 
  for(i in 1:numks)
  {
    classifications = knn(train[,c(51, 2, 62)],test[,c(51, 2, 62)],train$Attrition, prob = TRUE, k = i)
    table(classifications,test$Attrition)
    CM = confusionMatrix(table(classifications,test$Attrition))
    masterAcc[j,i] = CM$overall[1]
  }
  
}

MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")

# k = 13
classifications = knn(train[,c(37,15)],test[,c(37,15)],train$Attrition, prob = TRUE, k = 13)
classifications 
table(classifications,test$Attrition)
confusionMatrix(table(classifications,test$Attrition))

install.packages("randomForest")
library(randomForest)
#random forest modeling is a type of classification model that helps us determine the importance of each variable as it relates to attrition. 
set.seed(123)
rf_model <- randomForest(Attrition ~ ., data = df)
importance(rf_model)
order_RF = importance(rf_model)
order = data.frame(Feature = rownames(order_RF), Importance = order_RF[,1])
order <- order[!rownames(order) %in% "BinaryAttrition", ]
ggplot(order, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature Importance", x = "Features", y = "Importance Score")

predict(rf_model, df)
confusion_matrix <- table(Actual = df$Attrition, Predicted = predict(rf_model, df))
confusion_matrix



set.seed(90)
splitPerc = .75
trainIndices = sample(1:dim(df)[1],round(splitPerc * dim(df)[1]))
train = df[trainIndices,]
test = df[-trainIndices,]
knn_model = train(Attrition ~ OverTime + MaritalStatus + JobRole, data = dataTrain, method = "knn", tuneGrid = expand.grid(k=13))
predictions = predict (knn_model, newdata = test)
cm = confusionMatrix(predictions, test$Attrition)
cm



#old one with bigger columns

#Determining best k value


iterations = 500
numks = 30

masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(30), k = numeric(30))
  trainIndices = sample(1:dim(df)[1],round(splitPerc * dim(df)[1]))
  train = df[trainIndices,]
  test = df[-trainIndices,] #comp data here 
  for(i in 1:numks)
  {
    classifications = knn(train[,c(51,50,59)],test[,c(51,50,59)],train$Attrition, prob = TRUE, k = i)
    table(classifications,test$Attrition)
    CM = confusionMatrix(table(classifications,test$Attrition))
    masterAcc[j,i] = CM$overall[1]
  }
  
}

MeanAcc = colMeans(masterAcc)
plot(seq(1,numks,1),MeanAcc, type = "l")

# k = 13
classifications = knn(train[,c(51,50,59)],test[,c(51,50,59)],train$Attrition, prob = TRUE, k = 5)
classifications 
table(classifications,test$Attrition)
confusionMatrix(table(classifications,test$Attrition))


#NAIVE BAYES CLASSIFICATION
comp2 = read.csv(file.choose(),header = TRUE)
comp2

summary(df)
model = naiveBayes(df[,c(2,4:22, 24:27, 29:36)],df$Attrition, type = "class") #explanatory variables at the start, response variables in the second part
model
table(predict(model,df[,c(2,4:22, 24:27, 29:36)],df$Attrition, type = "class"))
CM = confusionMatrix(table(predict(model,df[,c(2,4:22, 24:27, 29:36)]),factor(df$Attrition)))
masterAcc
CM

#comp
comp2 = read.csv(file.choose(),header = TRUE)
comp2
summary(df)
model = naiveBayes(df[,c(51,50,59)],df$Attrition) #explanatory variables at the start, response variables in the second part
model
table(predict(model,df[,c(51,50,59)],df$Attrition) 
      CM = confusionMatrix(table(predict(model,df[,c(51,50,59)]),factor(df$Attrition))) 
      masterAcc
      CM
      

Male = D(1)


#Split Data
titanicClean = titanic %>% filter(!is.na(Age) & !is.na(Pclass))
set.seed(15) #put in the for loop; make sure it's before sampling. Set seed for i instead of a number 
trainIndices = sample(seq(1:length(titanicClean$Age)),round(.7*length(titanicClean$Age)))
trainTitanic = titanicClean[trainIndices,]
testTitanic = titanicClean[-trainIndices,]
head(trainTitanic)
head(testTitanic)

#Age/Pclass NB model
model2 = naiveBayes(titanicClean[,c(3,6)],titanicClean$Survived) #explanatory variables at the start, response variables in the second part
#model2 = naiveBayes(Survived ~ Age + Pclass, data = titanicClean) <- attempt at using a diff format for NB function
model2
table(predict(model2,titanicClean[,c(3,6)]),titanicClean$Survived) 
CM = confusionMatrix(table(predict(model2,titanicClean[,c(3,6)]),factor(titanicClean$Survived))) 
masterAcc
CM





#Categorical;
as.factor(df$BusinessTravel)
as.factor(df$Department)
as.factor(df$EducationField)
as.factor(df$Gender)
as.factor(df$JobRole)
as.factor(df$MaritalStatus)
as.factor(df$OverTime)

df <- df %>%
  mutate(OneHot_TravelNo = as.numeric(BusinessTravel == "Non_Travel"),
         OneHot_TravelRare = as.numeric(BusinessTravel == "Travel_Rarely"),
         OneHot_TravelFrequently = as.numeric(BusinessTravel == "Travel_Frequently"))
df


#Continuous:
df$Age
df$DailyRate
df$DistanceFromHome
df$Education
df$EmployeeNumber
df$EnvironmentSatisfaction
df$HourlyRate
df$JobInvolvement
df$JobLevel
df$JobSatisfaction
df$MonthlyIncome
df$MonthlyRate
df$NumCompaniesWorked
df$PercentSalaryHike
df$PerformanceRating
df$RelationshipSatisfaction
df$StockOptionLevel
df$TotalWorkingYears
df$TrainingTimesLastYear
df$WorkLifeBalance
df$YearsAtCompany
df$YearsInCurrentRole
df$YearsSinceLastPromotion
df$YearsWithCurrManager



table(df$OverTime, df$Attrition)



#COMING BACK FROM VACATION

#Nest these categories with each other: i.e. non-travel+single so the variables within each column
#highly correlated variables (higher horsepower car, larger weight, etc.) <- just use one or combine in a certain way
#create new variable/category

D = table(df$BusinessTravel, df$Gender)
D
males_travel_frequently <- D["Travel_Frequently", "Male"]
males_travel_frequently
#if males who travel frequently quit, then create a binary dataset or numerical dataset. new categories if you find certain combinations do better at predicting who quits/who stays
#pull out incorrect observations, covariates you can feed back into the model from there


#OLD STUFF

#Continuous_CorrelationData = 
#Cor_Age = cor(df$BinaryAttrition, df$Age)
#Cor_DailyRate = cor(df$BinaryAttrition, df$DailyRate)
#Cor_DistanceFromHome = cor(df$BinaryAttrition, df$DistanceFromHome)
#Cor_Education = cor(df$BinaryAttrition, df$Education)
#Cor_EmployeeNumber = cor(df$BinaryAttrition, df$EmployeeNumber)
#Cor_EnvironmentSatisfaction = cor(df$BinaryAttrition, df$EnvironmentSatisfaction)
#Cor_HourlyRate = cor(df$BinaryAttrition, df$HourlyRate)
#Cor_JobInvolvement = cor(df$BinaryAttrition, df$JobInvolvement)
#Cor_JobLevel = cor(df$BinaryAttrition, df$JobLevel)
#Cor_JobSatisfaction = cor(df$BinaryAttrition, df$JobSatisfaction)
#Cor_MonthlyIncome = cor(df$BinaryAttrition, df$MonthlyIncome)
#Cor_MonthlyRate = cor(df$BinaryAttrition, df$MonthlyRate)
#Cor_NumCompaniesWorked = cor(df$BinaryAttrition, df$NumCompaniesWorked)
#Cor_PercentSalaryHike = cor(df$BinaryAttrition, df$PercentSalaryHike)
#Cor_PerformanceRating = cor(df$BinaryAttrition, df$PerformanceRating)
#Cor_RelationshipSatisfaction = cor(df$BinaryAttrition, df$RelationshipSatisfaction)
#Cor_StockOptionLevel = cor(df$BinaryAttrition, df$StockOptionLevel)
#Cor_TotalWorkingYears = cor(df$BinaryAttrition, df$TotalWorkingYears)
#Cor_TrainingTimesLastYear = cor(df$BinaryAttrition, df$TrainingTimesLastYear)
#Cor_WorkLifeBalance = cor(df$BinaryAttrition, df$WorkLifeBalance)
#Cor_YearsAtCompany = cor(df$BinaryAttrition, df$YearsAtCompany)
#Cor_YearsInCurrentRole = cor(df$BinaryAttrition, df$YearsInCurrentRole)
#Cor_YearsSinceLastPromotion = cor(df$BinaryAttrition, df$YearsSinceLastPromotion)
#Cor_YearsWithCurrManager = cor(df$BinaryAttrition, df$YearsWithCurrManager)

correlations_df <- data.frame(
    Variable = c("Cor_Age", "Cor_DailyRate", "Cor_DistanceFromHome", "Cor_Education",
              "Cor_EmployeeNumber", "Cor_EnvironmentSatisfaction", "Cor_HourlyRate",
               "Cor_JobInvolvement", "Cor_JobLevel", "Cor_JobSatisfaction",
               "Cor_MonthlyIncome", "Cor_MonthlyRate", "Cor_NumCompaniesWorked",
               "Cor_PercentSalaryHike", "Cor_PerformanceRating", "Cor_RelationshipSatisfaction",
               "Cor_StockOptionLevel", "Cor_TotalWorkingYears", "Cor_TrainingTimesLastYear",
               "Cor_WorkLifeBalance", "Cor_YearsAtCompany", "Cor_YearsInCurrentRole",
               "Cor_YearsSinceLastPromotion", "Cor_YearsWithCurrManager"),
  
  Correlation = c(
    Cor_Age,
    Cor_DailyRate,
    Cor_DistanceFromHome,
    Cor_Education,
    Cor_EmployeeNumber,
    Cor_EnvironmentSatisfaction,
    Cor_HourlyRate,
    Cor_JobInvolvement,
    Cor_JobLevel,
    Cor_JobSatisfaction,
    Cor_MonthlyIncome,
    Cor_MonthlyRate,
    Cor_NumCompaniesWorked,
    Cor_PercentSalaryHike,
    Cor_PerformanceRating,
    Cor_RelationshipSatisfaction,
    Cor_StockOptionLevel,
    Cor_TotalWorkingYears,
    Cor_TrainingTimesLastYear,
    Cor_WorkLifeBalance,
    Cor_YearsAtCompany,
    Cor_YearsInCurrentRole,
    Cor_YearsSinceLastPromotion,
    Cor_YearsWithCurrManager
  )
)

correlations_df = correlations_df[order(correlations_df$Correlation, decreasing = TRUE), ]

correlations_df$Variable <- factor(correlations_df$Variable, 
                                   levels = correlations_df$Variable[order(correlations_df$Correlation, decreasing = TRUE)])

correlations_df

# Create the line graph
correlations_df %>% ggplot(mapping = aes(x = Variable, y = Correlation)) +
  geom_line() +
  geom_point() + 
  labs(title = "Correlation Values with Binary Attrition",
       x = "Variables",
       y = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility





#Worthless
cor(df$BinaryAttrition, df$EmployeeCount)
cor(df$BinaryAttrition, df$BinaryOver18)
cor(df$BinaryAttrition, df$StandardHours)













                                                         