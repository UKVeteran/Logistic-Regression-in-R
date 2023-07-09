#Read the input csv file and check the high level details
inputdf = read.csv("C:\\Users\\jau19\\OneDrive\\Desktop\\Data Analyst\\Logistic Regression R\\Dataset 6\\defaultcredit.csv")
str(inputdf)
summary(inputdf)

#Check if there are any missing value in the data
colSums(is.na(inputdf))

#Perform data visualization to better understand the data
library(ggplot2)

p <- ggplot(data=inputdf, aes(default))+
  geom_bar(stat = 'count',color = "darkblue",fill = "lightblue", width = 0.3)
p + labs(title = "Bar Plot for default")

p <- ggplot(data=inputdf, aes(student))+
  geom_bar(stat = 'count',color = "darkblue",fill = "lightblue", width = 0.3)
p + labs(title = "Bar Plot for student")

#Histogram of the Balance Field
p <- ggplot(data=inputdf, aes(x = balance))+
  geom_histogram(color = "darkblue",fill = "lightblue", bins = 20)
p + labs(title = "Histogram for Balance")

#Histogram of Income field
p <- ggplot(data=inputdf, aes(x = income))+
  geom_histogram(color = "darkblue",fill = "lightblue", bins = 20)
p + labs(title = "Histogram for Income")

# As we do not have any missing value,  lets move ahead and split our data in train and test sets
sampleindex=sample(1:nrow(inputdf),size=0.8*nrow(inputdf))
trainData=inputdf[sampleindex,]
testData=inputdf[-sampleindex,]
dim(trainData)
dim(testData)

#Now lets build the model using our train set
logistic_model=glm(as.factor(default)~.,data=trainData,family="binomial")
summary(logistic_model)

#Test the model on our test data using predict function
logistic_pred1=predict(logistic_model,testData, type="response")
#Set the threshold or cutoff probability value for our prediction
logistic_pred=ifelse(logistic_pred1>0.4,1,0)

#Based on above threshold create the confusion matrix
CM = table(testData$default,logistic_pred)
print(CM)

#Calculate the  TN  FN  FP TP values based on above confusion matrix
TN = CM[1,'0']
FN = CM[2,'0']
FP = CM[1,'1']
TP = CM[2,'1']
print(paste("TN : ",TN))
print(paste("FN : ",FN))
print(paste("FP : ",FP))
print(paste("TP : ",TP))

#Calculate various evaluting creteria of the model
print(paste("The Accuracy is:",(sum(diag(table(testData$default,logistic_pred)))/nrow(testData))*100))
print(paste("The Precision is:",((TP/(TP+FP))*100)))
print(paste("The Recall is:",((TP/(TP+FN))*100)))
print(paste("The Specificity is:",((TN/(TN+FP))*100)))

#Plot the ROC forr the above model
library(ROCR)
ROC_perf <- performance(prediction(logistic_pred1,testData$default),"tpr","fpr")
plot(ROC_perf,colorize=T,print.cutoffs.at=seq(0,1,by=0.1))

