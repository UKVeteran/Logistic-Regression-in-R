library(dplyr)
library(ggplot2)

# load in student data set
student <- read.csv("C:\\Users\\jau19\\OneDrive\\Desktop\\Data Analyst\\Logistic Regression R\\Dataset 8\\StudentsPerformance.csv",header= T)

#create pass or fail math test variable - this will be what we try to predict
student <- student %>% mutate(pass = ifelse(math.score > 60,1,0) %>% as.factor())

#view table
head(student)

# set random seed
set.seed(1)

# create training subset
train <- sample(1:nrow(student),nrow(student)*.8,rep = F)

#create model
lr.model <- glm(pass ~ . -math.score - reading.score - writing.score,data = student, family = binomial,subset = train)

# view summary of model
summary(lr.model)

# create prediction vector
probs <- predict(lr.model,student[-train,], type = 'response')

# assign class of pass = 1 or fail = 0
preds <- ifelse(probs > 0.5,1,0)

#populate confusion matrix
conf <- table(preds,student$pass[-train])

conf

# over all model accuracy
mean(preds ==student$pass[-train])

# misclassification rate
mean(preds !=student$pass[-train])

# false positives
conf[2,1] / sum(conf[,1])

# false negatives
conf[1,2] / sum(conf[,2])




library(pROC)

roc(pass ~ probs,data = student[-train,]) %>% plot(asp = NA)






library(MASS)

lda.model = lda(pass ~ . - math.score - reading.score - writing.score, data = student,subset = train)

probs = predict(lda.model,student[-train,])

table(probs$class, student$pass[-train])

mean(probs$class == student$pass[-train])



