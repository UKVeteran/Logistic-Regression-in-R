library(ggplot2)
library(caret)

sigmoid <- function(t){ 1/(1+exp(-t)) }     # Define the sigmoid function

dummy_frame <- data.frame(x=c(-6,6))

ggplot(data=dummy_frame) +                  # Plot the function 
  stat_function(fun=sigmoid) +
  xlim(-6,6)

titanic_train <- read.csv("C:\\Users\\jau19\\OneDrive\\Desktop\\Data Analyst\\Logistic Regression R\\Dataset 2\\train.csv")

titanic_train$PassengerId  <- NULL             # Remove PassengerId
titanic_train$Ticket  <- NULL                  # Remove Ticket
titanic_train$Name <- as.character(titanic_train$Name)    # Convert name to character

titanic_train$Pclass <- ordered(titanic_train$Pclass,     # Convert to ordered factor
                                levels=c("3","2","1"))  

# Reduce cabin factor levels
char_cabin <- as.character(titanic_train$Cabin)     

new_Cabin <- ifelse(char_cabin == "",          
                    "",                        
                    substr(char_cabin,1,1))    

new_Cabin <- factor(new_Cabin )                
titanic_train$Cabin <- new_Cabin

impute <- preProcess(titanic_train[,c(5:8)],  # Impute missing ages*
                     method=c("knnImpute"))

titanic_train_imp <- predict(impute, titanic_train[,c(5:8)])     

titanic_train <- cbind(titanic_train[,c(1:4)], titanic_train_imp, titanic_train[,c(9:10)])


summary(titanic_train)



titanic_model <- glm(Survived ~ Sex,        # Formula
                     data= titanic_train,    # Data set
                     family="binomial")      # family="binomial" for binary logistic

summary(titanic_model)                      # Check model summary


train_preds <- predict(titanic_model,              # Model to use
                       newdata=titanic_train,      # Data to use for predictions
                       type="response")            # Return predicted probabilities

table(train_preds, titanic_train$Sex)

