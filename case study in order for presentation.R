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


#Random Forest
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

#Monthly Income

#Monthly Income vs. Attrition
df %>%
  ggplot(mapping=aes(x=MonthlyIncome, fill=Attrition)) +
  geom_histogram(bins=30, position="fill", alpha=0.5) +
  labs(title="Monthly Income Distribution by Attrition Status")

#Years at Company vs. Attrition; just to show that it could be more junior employees
df %>%
  ggplot(mapping=aes(x=YearsAtCompany, fill=Attrition)) +
  geom_bar(position="fill") +
  labs(title="Years at Company by Attrition")

#Overtime Impact on Attrition
df %>%
  ggplot(mapping=aes(x=OverTime, fill=Attrition)) +
  geom_bar(position="fill") +
  labs(title="Attrition Rates with Overtime Consideration")

#Work-Life Balance vs. Attrition
df %>%
  ggplot(mapping=aes(x=WorkLifeBalance, fill=Attrition)) +
  geom_bar(position="fill") +
  labs(title="Work-Life Balance by Attrition Status")




#Age Distribution by Attrition
df %>%
  ggplot(mapping=aes(x=Age, fill=Attrition)) +
  geom_histogram(binwidth=5, position="fill", alpha=0.5) +
  labs(title="Age Distribution by Attrition Status")

#Years at Company vs. Attrition
df %>%
  ggplot(mapping=aes(x=age_group, fill=Attrition)) +
  geom_bar(position="fill") + xlab ("Age Groups") +
  labs(title="Attrition Rates per Age Group")

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




#Job Role Trends

#Job Role vs Monthly Income
df %>%
  ggplot(mapping=aes(y=MonthlyIncome, x=JobRole)) +
  geom_boxplot() + xlab ("Age Groups") +
  labs(title="Monthly Income per Job Role")

# Job Role vs. OverTime
df %>%
  ggplot(mapping = aes(x = JobRole, fill = OverTime)) +
  geom_bar(position = "fill") +
  labs(title = "Job Role vs. OverTime")

#Job Role vs Age Group
df %>%
  ggplot(mapping=aes(x=JobRole, fill=age_group)) +
  geom_bar(position="dodge") + xlab ("Job Titles") +
  labs(title="Job Roles by Age Group") + facet_wrap(vars(JobRole))

#sales rep skews younger and is on lower end of salary. 

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



#Naive Bayes Model

model = naiveBayes(df[,c(2,4:22, 24:27, 29:36)],df$Attrition, type = "class") #explanatory variables at the start, response variables in the second part
model
table(predict(model,df[,c(2,4:22, 24:27, 29:36)],df$Attrition, type = "class"))
CM = confusionMatrix(table(predict(model,df[,c(2,4:22, 24:27, 29:36)]),factor(df$Attrition)))
masterAcc
CM

ggplot(data = confusion_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  labs(title = "Confusion Matrix", x = "Actual Species", y = "Predicted Species") +
  theme_minimal()
