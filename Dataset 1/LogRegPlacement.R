library(dplyr)
library(ggplot2)
location <- "C:/Users/jau19/OneDrive/Desktop/Data Analyst/Logistic Regression R/Dataset 1/Placement_Data_Full_Class.csv"
placement.df <- read.csv(location)
# select only relevant columns
placement.lr <- placement.df %>% select(ends_with("_p"), -etest_p, status)
table(placement.lr$status)
# we need Not Placed class to be coded as 1 (positive)
placement.lr$status <- ifelse(placement.lr$status == "Not Placed", 1, 0)
table(placement.lr$status)

# Train and Test data
library(caTools) # to split data into train and test
set.seed(101)
sample <- sample.split(placement.lr$status, SplitRatio = 0.80)
train.lr = subset(placement.lr, sample == TRUE)
test.lr = subset(placement.lr, sample == FALSE)
#check the splits
prop.table(table(train.lr$status))
prop.table(table(test.lr$status))

# Train the model
model.lr <- glm(status ~ degree_p, family = binomial, data = train.lr)
summary(model.lr)


# prediction
lr.pred <- predict(model.lr, newdata = test.lr, type = "response")
head(lr.pred)
# The probabilities always refer to the class dummy-coded as “1”
head(test.lr$status)



# Classification Table
# categorize into groups based on the predicted probability
lr.pred.class <- ifelse(lr.pred>=0.5, 1, 0)
head(lr.pred.class)
table(lr.pred.class)
table(test.lr$status)
conf.matrix <- table(test.lr$status, lr.pred.class)
conf.matrix
rownames(conf.matrix) <- c("Placed", "Not Placed")
colnames(conf.matrix) <- c("Placed", "Not Placed")
addmargins(conf.matrix)

# model accuracy
mean((test.lr$status == lr.pred.class))

# different cut-off
lr.pred.class1 <- ifelse(lr.pred>=0.35, 1, 0)
conf.matrix1 <- table(test.lr$status, lr.pred.class1)
conf.matrix1