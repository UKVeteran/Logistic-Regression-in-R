mydata <- read.csv("C:/Users/jau19/OneDrive/Desktop/Data Analyst/Logistic Regression R/Dataset 4/WA_Fn-UseC_-HR-Employee-Attrition.csv")
library(dplyr)
library(ggplot2)
library(ggthemes)

str(mydata)

dim(mydata)

numeric_mydata <- mydata[,c(1,4,6,7,10,11,13,14,15,17,19,20,21,24,25,26,28:35)]
numeric_Attrition = as.numeric(mydata$Attrition)- 1
numeric_mydata = cbind(numeric_mydata, numeric_Attrition)
str(numeric_mydata)
library(corrplot)
M <- cor(numeric_mydata)
corrplot(M, method="circle")

### Overtime vs Attiriton
l <- ggplot(mydata, aes(OverTime,fill = Attrition))
l <- l + geom_histogram(stat="count")
print(l)
tapply(as.numeric(mydata$Attrition) - 1 ,mydata$OverTime,mean)

### MaritalStatus vs Attiriton
l <- ggplot(mydata, aes(MaritalStatus,fill = Attrition))
l <- l + geom_histogram(stat="count")
print(l)
tapply(as.numeric(mydata$Attrition) - 1 ,mydata$MaritalStatus,mean)

###JobRole vs Attrition
l <- ggplot(mydata, aes(JobRole,fill = Attrition))
l <- l + geom_histogram(stat="count")
print(l)
tapply(as.numeric(mydata$Attrition) - 1 ,mydata$JobRole,mean)
mean(as.numeric(mydata$Attrition) - 1)

### x=Overtime, y= Age, z = MaritalStatus , t = Attrition
ggplot(mydata, aes(OverTime, Age)) +  
  facet_grid(.~MaritalStatus) +
  geom_jitter(aes(color = Attrition),alpha = 0.4) +  
  ggtitle("x=Overtime, y= Age, z = MaritalStatus , t = Attrition") +  
  theme_light()

### MonthlyIncome vs. Age, by  color = Attrition
ggplot(mydata, aes(MonthlyIncome, Age, color = Attrition)) + 
  geom_jitter() +
  ggtitle("MonthlyIncome vs. Age, by  color = Attrition ") +
  theme_light()








library(caTools)
library(e1071)
library(glmnet)

mydatanew = mydata[,-c(6,9,22)]
str(mydatanew)


#Splitting data
split <- sample.split(mydatanew$Attrition, SplitRatio = 0.80) 
train <- subset(mydatanew, split == T) 
test <- subset(mydatanew, split == F)

model_glm <- glm(as.factor(Attrition) ~ ., data=train, family="binomial") 
predicted_glm <- predict(model_glm, test, type='response')
predicted_glm <- ifelse(predicted_glm > 0.5,1,0)

table(test$Attrition, predicted_glm)

