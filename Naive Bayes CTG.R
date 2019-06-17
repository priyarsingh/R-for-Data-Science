#_________________Naive Bayes__________________________________
install.packages("e1071")
library(e1071)
library(dplyr)

CTG1 <-read.csv("CTG.csv",na.strings = "")
View(CTG1)
dim(CTG1)

#Description:----------------------------------------------------------------
#LB-Beats per second
#AC-Acceleration per second
#FM-fetal per second
#NSP"y class"-Normal,suspect,pathalogical-3 class and 3rd class should be of very good accuracy.

#Data preparation:-----------------------------------------------------------
CTG1$NSP <- factor(CTG1$NSP)
summary(CTG1)


#sampling:-------------------------------------------------------------------
CTGsampl <- sample(2,nrow(CTG1),replace = TRUE,prob = c(.8,.2))
CTGsampl

#train data:
train_CTGsampl <-CTG1[CTGsampl==1, ]
train_CTGsampl

#test data:
test_CTGsampl <-CTG1[CTGsampl==2, ]
test_CTGsampl


#Model Building-----------------------------------------------------------------
#building model on train data:
Modl_CTG <- naiveBayes(NSP~.,data = train_CTGsampl)
summary(Modl_CTG)

#predicting on test data:
predict_va <-predict(Modl_CTG,test_CTGsampl,type ="class")
predict_va

#Skip the step of creating data frame for comapring predicted and actual value and the mutate function to replace values above .5 to 1 and below .5 to 0(as we do not have probabilty value):

#confusion matrix
Table1 <-table(CTG1$NSP)
Table1
