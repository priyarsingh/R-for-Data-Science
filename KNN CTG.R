
#____________________KNN_________________________________________
install.packages("e1071")
library(e1071)
library(dplyr)
library(class)

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

Modl_CTG <- knn(train=train_CTGsampl  ,test= test_CTGsampl ,cl = train_CTGsampl$NSP,k=5)
summary(Modl_CTG)

df1 <-data.frame(Modl_CTG,test_CTGsampl$NSP)

#Skip predicting value line as it is already done with model buildng in same line in KNN.

#confusion matrix
table <-table(Modl_CTG,test_CTGsampl$NSP)
table
