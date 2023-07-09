#First, let's load the sexy data and take a look at it
data <- read.csv("C:\\Users\\jau19\\OneDrive\\Desktop\\Data Analyst\\Logistic Regression R\\Dataset 5\\heart_failure_clinical_records_dataset.csv")
library(dplyr)  #this library let's us use the sexy glimpse function to "peak" at our data
glimpse(data)

sapply(data, function(x) sum(is.na(x)))

#Next, create a train and test data set using a 60% split.  This # is arbitrary.
dt = sort(sample(nrow(data), nrow(data)*.6))
train <- data[dt,]
test <- data[-dt,]

#Let's create our first model using the train data set.
train_model <- glm(DEATH_EVENT ~ ., data = train, family = "binomial")
summary(train_model)

#Next, we create our predictions below.  
#Note that I create a threshold of .5
results <- train %>% 
  mutate(pred_prob_model = predict(train_model, newdata = train, type = "response")) %>% 
  mutate(pred_outcome_model = ifelse(pred_prob_model >= 0.5, 1,0))


#Now, let's convert "DEATH_EVENT" and our outcomes to factors.
results$DEATH_EVENT <- as.factor(results$DEATH_EVENT)
results$pred_outcome_model <- as.factor(results$pred_outcome_model)

#the confusionMatrix() takes the prediction first, and the actual results second
library(caret)  #this library has the confusionMatrix() function
confusionMatrix(results$pred_outcome_model, results$DEATH_EVENT)




#Let's create the predictions 
final_results <- test %>% 
  mutate(pred_prob_model = predict(train_model, newdata = test, type = "response")) %>% 
  mutate(pred_outcome_model = ifelse(pred_prob_model >= 0.5, 1,0)) 

#Let's turn the "DEATH_EVENT" and the "pred_outcome_model" to factors 
final_results$DEATH_EVENT <- as.factor(final_results$DEATH_EVENT)
final_results$pred_outcome_model <- as.factor(final_results$pred_outcome_model)

#Let's see how our model did with the test data set
confusionMatrix(final_results$pred_outcome_model, final_results$DEATH_EVENT)

#Let's create a sexy ROC chart to see how we did.
#First, we create a prediction object in R.
library(ROCR)
pred <- prediction(final_results$pred_prob_model, final_results$DEATH_EVENT) 
class(pred)
pred

#Next let's plot the ROC Curve
perf <- performance(pred, "tpr", "fpr") # tpr and fpr are true and false positive rates
plot(perf, colorize=T)


#Finally, let's see the area under the ROC curve.  
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values  #the output is .86, or 86%.

