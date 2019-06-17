Credit_Risk1 <-read.csv("CreditRisk.csv",na.strings = "")
View(Credit_Risk1)
library(dplyr)
#data preparation 
var1 <-mean(Credit_Risk1$LoanAmount ,na.rm = TRUE)
var1
Credit_Risk1$LoanAmount[is.na(Credit_Risk1$LoanAmount)] <-var1 
var1
is.na(Credit_Risk1$LoanAmount)

var4 <-mean(Credit_Risk1$ApplicantIncome ,na.rm = TRUE) 
Credit_Risk1$ApplicantIncome[is.na(Credit_Risk1$ApplicantIncome)] <-var4 
Credit_Risk1$Gender[is.na(Credit_Risk1$Gender)] <- "Male"

Credit_Risk1 <-mutate(Credit_Risk1,Loan_Status1= ifelse(Loan_Status=="Y",1,0))
Credit_Risk1 <-mutate(Credit_Risk1,Education1= ifelse(Education=="Graduate",1,0))
Credit_Risk1 <-mutate(Credit_Risk1,Self_Employed1= ifelse(Self_Employed=="YES",1,0))
Credit_Risk1 <-mutate(Credit_Risk1,newarea= ifelse(Property_Area=="Urban",2,ifelse(Property_Area=="Rural",0,1)))
Credit_Risk1 <-mutate(Credit_Risk1,Gender1= ifelse(Gender=="Female",1,0))
Credit_Risk1 <-mutate(Credit_Risk1,Married1= ifelse(Married=="No",1,0))

Credit_Risk1 <-select(Credit_Risk1,4,7,8,9,10,11,14,15,16,17,18,19)

Credit_Risk$Loan_Status1 <- factor(Credit_Risk1$Loan_Status1)
Credit_Risk$Education1 <- factor(Credit_Risk1$Education1)
Credit_Risk$Self_Employed1 <- factor(Credit_Risk1$Self_Employed1)
Credit_Risk$newarea <- factor(Credit_Risk1$newarea)
Credit_Risk$Gender1 <- factor(Credit_Risk1$Gender1)
Credit_Risk$Married1 <- factor(Credit_Risk1$Married1)

summary(Credit_Risk1)

#sampling
CRS <- sample(2,nrow(Credit_Risk1),replace = TRUE,prob = c(.8,.2))
CRS
train_CRS<-Credit_Risk1[CRS==1, ]
train_CRS

test_CRS <-Credit_Risk1[CRS==2, ]
test_CRS

#over sampling
train_sample <-sample(2,nrow(Credit_Risk1),replace = TRUE,prob =c(.8,.2))
train_crs <-Credit_Risk1[train_sample==1,]
test_crs <-Credit_Risk1[train_sample==2,]
aa <-filter(train_crs,Loan_Status1==0)
train_crs <-rbind(train_crs,aa,aa,aa,aa)
Model_CR1 <- glm(Loan_Status1~.,family = binomial,data = train_crs)

#logistic regression-glm
Model_CR1 <- glm(Loan_Status1~.,family = binomial,data = train_crs)
dim(Credit_Risk1)
head(train_crs)




predi_val <-predict(Model_CR1,test_crs,type ="response")
predi_val

predi_actual_df <-data.frame(predi_val,test_crs$Loan_Status1)
predi_actual_df

predi_actual_df <- mutate(predi_actual_df,predi_val = ifelse(predi_val > .5,1,0))

colnames(predi_actual_df) <-c("predict","actual")

tab <-table(predi_actual_df$predict,predi_actual_df$actual)
tab




#using credit history
Model_CR1 <- glm(Loan_Status1~Credit_History,family = binomial,data = train_crs)

predi_val <-predict(Model_CR1,test_crs,type ="response")
predi_val

predi_actual_df <-data.frame(predi_val,test_crs$Loan_Status1)
predi_actual_df

predi_actual_df <- mutate(predi_actual_df,predi_val = ifelse(predi_val > .5,1,0))

colnames(predi_actual_df) <-c("predict","actual")

tab <-table(predi_actual_df$predict,predi_actual_df$actual)
tab

#all the variable except credit history
Model_CR1 <- glm(Loan_Status1~.,family = binomial,data = train_crs[,c(1:5,7:12)])

predi_val <-predict(Model_CR1,test_crs,type ="response")
predi_val

predi_actual_df <-data.frame(predi_val,test_crs$Loan_Status1)
predi_actual_df

predi_actual_df <- mutate(predi_actual_df,predi_val = ifelse(predi_val > .5,1,0))

colnames(predi_actual_df) <-c("predict","actual")

tab <-table(predi_actual_df$predict,predi_actual_df$actual)
tab
