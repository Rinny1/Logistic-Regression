
#------------------------------Preparing the environment for Logistic Regression---------------------------------------#

list.of.packages <- c("caret", "ggplot2","MASS","car","mlogit","caTools","sqldf","Hmisc","aod","BaylorEdPsych","ResourceSelection","pROC","ROCR")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)
library(aod)
library(BaylorEdPsych)
library(ResourceSelection)
library(pROC)
library(ROCR)
library(caTools)
#===========================Setting Working Directory==================================#
Path<-"C:/Users/raveesh/Desktop/Data_for_Logistic_Regression.csv"
setwd(Path)
getwd()


data<-read.csv("Data_for_Logistic_Regression.csv",header = TRUE)
data1=data
head(data1)

#=================================Basic Exploration of the data=========================# 
str(data1)
summary(data1)
dim(data1)

#=============================Missing Value Treatment (if any)=========================-#
data.frame(colSums(is.na(data1)))

#===================== Creating data set for categorical values

## Data set with Categorical Data Frame
num<-data1[,-c(2,4:5,7:8,10:11,13,15:16,18,20:21)]
cat <- data1[,c(2,4:5,7:8,10:11,13,15:16,18,20:21,22)]
head(cat)
head(num)
str(num)
str(cat)


#================================IV for numeric data===============================#

IVCal <- function(variable, target,data1,groups)
{
  data[,"rank"] <- cut2(data[,variable],g=groups)
  tableOutput <-sqldf(sprintf("select rank, 
                              count(%s) n,
                              sum(%s) good
                              from data 
                              group by rank",target,target))
  tableOutput <- sqldf("select *,
                       (n - good) bad
                       from tableOutput")
  tableOutput$bad_rate<- tableOutput$bad/sum(tableOutput$bad)*100
  tableOutput$good_rate<- tableOutput$good/sum(tableOutput$good)*100
  tableOutput$WOE<- (log(tableOutput$good_rate/tableOutput$bad_rate))*100
  tableOutput$IV <- (log(tableOutput$good_rate/tableOutput$bad_rate))*(tableOutput$good_rate-tableOutput$bad_rate)/100
  IV <- sum(tableOutput$IV[is.finite(tableOutput$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}

a1<- IVCal("Customer_ID","Default_On_Payment",num,groups=10)
a2<- IVCal("Duration_in_Months","Default_On_Payment",num,groups=10)
a3<-IVCal("Credit_Amount","Default_On_Payment",num,groups=10)
a4<-IVCal("Inst_Rt_Income","Default_On_Payment",num,groups=10)
a5<- IVCal("Current_Address_Yrs","Default_On_Payment",num,groups=10)
a6<- IVCal("Age","Default_On_Payment",num,groups=10)
a7<-IVCal("Num_CC","Default_On_Payment",num,groups=10)
a8<-IVCal("Dependents","Default_On_Payment",num,groups=10)
a9<-IVCal("Count","Default_On_Payment",num,groups=10)

IV_num<- data.frame(rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9))
IV_num

#============================Information Value for categorical data==========================#
CA <- function(target, variable, data1) {
  A1<- fn$sqldf("select $variable,count($target)n, sum($target)good from data group by $variable")
  
  A1<- fn$sqldf("select *, (n-good) bad from A1")
  A1$bad_rate <- A1$bad/sum(A1$bad)*100
  
  A1$good_rate<- A1$good/sum(A1$good)*100
  A1$WOE<- (log(A1$good_rate/A1$bad_rate))*100
  A1$IV <- (log(A1$good_rate/A1$bad_rate))*(A1$good_rate-A1$bad_rate)/100
  IV <- sum(A1$IV[is.finite(A1$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}
A<- CA("Default_On_Payment","Status_Checking_Acc",cat)
B<- CA("Default_On_Payment","Credit_History",cat)
C<- CA("Default_On_Payment","Purposre_Credit_Taken",cat)
D<- CA("Default_On_Payment","Savings_Acc",cat)
E<- CA("Default_On_Payment","Years_At_Present_Employment",cat)
F<- CA("Default_On_Payment","Marital_Status_Gender",cat)

G<- CA("Default_On_Payment","Other_Debtors_Guarantors",cat)
H<- CA("Default_On_Payment","Property",cat)
I<- CA("Default_On_Payment","Other_Inst_Plans",cat)
J<- CA("Default_On_Payment","Housing",cat)
K<- CA("Default_On_Payment","Job",cat)
L<- CA("Default_On_Payment","Telephone",cat)
M<- CA("Default_On_Payment","Foreign_Worker",cat)


IV_cat<- data.frame(rbind(A,B,C,D,E,F,G,H,I,J,K,L,M))
IV_cat

write.csv(IV_cat,"IV_cat.csv")



#=============================Splitting data into Training and Test data=================#
set.seed(144)
spl = sample.split(data1$Default_On_Payment, 0.7)
data.train = subset(data1, spl == TRUE)
str(data.train)
dim(data.train)


data.test = subset(data1, spl == FALSE)
str(data.test)
dim(data.test)

#===============================Logistic Regression Model Building========================#



Model<- glm(Default_On_Payment~Status_Checking_Acc+
               Duration_in_Months+Credit_History+Purposre_Credit_Taken+
               Credit_Amount+Savings_Acc+Years_At_Present_Employment+
               Inst_Rt_Income+Marital_Status_Gender+Other_Debtors_Guarantors+Property+Age+Other_Inst_Plans+
               Housing+Foreign_Worker,
             data=data.train, family=binomial())
summary(Model)

#Removing Property==A122
model <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12")+
               I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")+
               Duration_in_Months+I(Credit_History=="A31")+I(Credit_History=="A32")+
               I(Credit_History=="A33")+I(Credit_History=="A34")+I(Purposre_Credit_Taken=="A41")+
               I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+
               I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")+
               I(Purposre_Credit_Taken=="A45")+I(Purposre_Credit_Taken=="A46")+
               I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")+
               Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A63")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")+I(Years_At_Present_Employment=="A72")+I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+
               Inst_Rt_Income+I(Marital_Status_Gender=="A92")+I(Marital_Status_Gender=="A93")+
               I(Marital_Status_Gender=="A94")+I(Other_Debtors_Guarantors=="A102")+
               I(Other_Debtors_Guarantors=="A103")+
               I(Property=="A123")+I(Property=="A124")+
               Age+I(Other_Inst_Plans=="A142")+I(Other_Inst_Plans=="A143")+
               I(Housing=="A152")+I(Housing=="A153")+I(Foreign_Worker=="A202"),
               data=data.train, family=binomial())
summary(model)

#Remove Other_Inst_Plans==A142
model <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12")+
               I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")+
               Duration_in_Months+I(Credit_History=="A31")+I(Credit_History=="A32")+
               I(Credit_History=="A33")+I(Credit_History=="A34")+I(Purposre_Credit_Taken=="A41")+
               I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+
               I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")+
               I(Purposre_Credit_Taken=="A45")+I(Purposre_Credit_Taken=="A46")+
               I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")+
               Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A63")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")+I(Years_At_Present_Employment=="A72")+I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+
               Inst_Rt_Income+I(Marital_Status_Gender=="A92")+I(Marital_Status_Gender=="A93")+
               I(Marital_Status_Gender=="A94")+I(Other_Debtors_Guarantors=="A102")+
               I(Other_Debtors_Guarantors=="A103")+
               I(Property=="A123")+I(Property=="A124")+
               Age+I(Other_Inst_Plans=="A143")+
               I(Housing=="A152")+I(Housing=="A153")+I(Foreign_Worker=="A202"),
             data=data.train, family=binomial())
summary(model)

#Remove Purposure_Credit_Taken==A45
model <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12")+
               I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")+
               Duration_in_Months+I(Credit_History=="A31")+I(Credit_History=="A32")+
               I(Credit_History=="A33")+I(Credit_History=="A34")+I(Purposre_Credit_Taken=="A41")+
               I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+
               I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")+
               I(Purposre_Credit_Taken=="A46")+
               I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")+
               Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A63")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")+I(Years_At_Present_Employment=="A72")+I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+
               Inst_Rt_Income+I(Marital_Status_Gender=="A92")+I(Marital_Status_Gender=="A93")+
               I(Marital_Status_Gender=="A94")+I(Other_Debtors_Guarantors=="A102")+
               I(Other_Debtors_Guarantors=="A103")+
               I(Property=="A123")+I(Property=="A124")+
               Age+I(Other_Inst_Plans=="A143")+
               I(Housing=="A152")+I(Housing=="A153")+I(Foreign_Worker=="A202"),
             data=data.train, family=binomial())
summary(model)

#Remove Years_At_Employment==A72
model <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12")+
               I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")+
               Duration_in_Months+I(Credit_History=="A31")+I(Credit_History=="A32")+
               I(Credit_History=="A33")+I(Credit_History=="A34")+I(Purposre_Credit_Taken=="A41")+
               I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+
               I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")+
               I(Purposre_Credit_Taken=="A46")+
               I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")+
               Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A63")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")+I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+
               Inst_Rt_Income+I(Marital_Status_Gender=="A92")+I(Marital_Status_Gender=="A93")+
               I(Marital_Status_Gender=="A94")+I(Other_Debtors_Guarantors=="A102")+
               I(Other_Debtors_Guarantors=="A103")+
               I(Property=="A123")+I(Property=="A124")+
               Age+I(Other_Inst_Plans=="A143")+
               I(Housing=="A152")+I(Housing=="A153")+I(Foreign_Worker=="A202"),
             data=data.train, family=binomial())
summary(model)

#Remove Credit HHistory==A31 andPurposre_Credit  Taken==A44and A46
model <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12")+
               I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")+
               Duration_in_Months+I(Credit_History=="A32")+
               I(Credit_History=="A33")+I(Credit_History=="A34")+I(Purposre_Credit_Taken=="A41")+
               I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+
               I(Purposre_Credit_Taken=="A43")+
               I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")+
               Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A63")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")+I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+
               Inst_Rt_Income+I(Marital_Status_Gender=="A92")+I(Marital_Status_Gender=="A93")+
               I(Marital_Status_Gender=="A94")+I(Other_Debtors_Guarantors=="A102")+
               I(Other_Debtors_Guarantors=="A103")+
               I(Property=="A123")+I(Property=="A124")+
               Age+I(Other_Inst_Plans=="A143")+
               I(Housing=="A152")+I(Housing=="A153")+I(Foreign_Worker=="A202"),
             data=data.train, family=binomial())
summary(model)


#Remmove Property==A123 and Marital_Status_Gender==A92
model <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12")+
               I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")+
               Duration_in_Months+I(Credit_History=="A32")+
               I(Credit_History=="A33")+I(Credit_History=="A34")+I(Purposre_Credit_Taken=="A41")+
               I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+
               I(Purposre_Credit_Taken=="A43")+
               I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")+
               Credit_Amount+I(Savings_Acc=="A62")+I(Savings_Acc=="A63")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")+I(Years_At_Present_Employment=="A73")+I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+
               Inst_Rt_Income+I(Marital_Status_Gender=="A93")+
               I(Marital_Status_Gender=="A94")+I(Other_Debtors_Guarantors=="A102")+
               I(Other_Debtors_Guarantors=="A103")+
              I(Property=="A124")+
               Age+I(Other_Inst_Plans=="A143")+
               I(Housing=="A152")+I(Housing=="A153")+I(Foreign_Worker=="A202"),
             data=data.train, family=binomial())
summary(model)

#Final Model
model <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12")+
               I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")+
               Duration_in_Months+
          I(Credit_History=="A34")+I(Purposre_Credit_Taken=="A41")+
               I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+
               I(Purposre_Credit_Taken=="A43")+
               I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")+
               I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")+I(Years_At_Present_Employment=="A74")+
               Inst_Rt_Income+I(Marital_Status_Gender=="A93")+
               
               I(Other_Debtors_Guarantors=="A103")+
               
               Age+I(Other_Inst_Plans=="A143")+
               I(Housing=="A152")+I(Foreign_Worker=="A202"),
             data=data.train, family=binomial())
summary(model)

vif(model)
wald.test(b=coef(model), Sigma= vcov(model), Terms=1:22)

#------------------->Lagrange Multiplier or Score Test (Assess wether the current variable 
#significantly improves the model fit or not)


# Difference betweene null deviance and deviance
modelChi <- model$null.deviance - model$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- model$df.null - model$df.residual
chidf


# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)

#------------------------------------Predicting power of the model using R2----------------------------#
PseudoR2(model)


#--------------------Lackfit Deviance for assessing wether the model where
#Ho: Observed Frequencies/probabilties =Expected FRequencies/probabilties ----------------------------------------#
residuals(model) # deviance residuals
residuals(model, "pearson") # pearson residuals

sum(residuals(model, type = "pearson")^2)
deviance(model)

#########Large p value indicate good model fit
1-pchisq(deviance(model), df.residual(model))
#Thus, we accept the Ho



################################################################################################################
# How to use the function. Data.train is the name of the dataset. model is name of the glm output
## This is not a very useful test. Some authors have suggested that sometimes it produces wrong result##
## High p value incidates the model fits well


# Hosmer and Lemeshow test 


hl <- hoslem.test(as.integer(data.train$Default_On_Payment), fitted(model), g=10)
hl


#####################################################################################################################
# Coefficients (Odds)
model$coefficients
# Coefficients (Odds Ratio)
exp(model$coefficients)


# Variable Importance of the model
varImp(model)

# Predicted Probabilities
prediction <- predict(model,newdata = data.train,type="response")
prediction

write.csv(prediction,"pred.csv")


rocCurve   <- roc(response = data.train$Default_On_Payment, predictor = prediction, 
                  levels = rev(levels(data.train$Default_On_Payment)))
rocCurve
data.train$Default_On_Payment <- as.factor(data.train$Default_On_Payment)

#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.train$Default_On_Payment)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)



#########################################################################################################################
### KS statistics calculation
data.train$m1.yhat <- predict(model, data.train, type = "response")
m1.scores <- prediction(data.train$m1.yhat, data.train$Default_On_Payment)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 40 - 70

############################################################################################################


###################### Residual Analysis ################################################################################


logistic_data <- data.train

logistic_data$predicted.probabilities<-fitted(model)

logistic_data$standardized.residuals<-rstandard(model)
logistic_data$studentized.residuals<-rstudent(model)
logistic_data$dfbeta<-dfbeta(model)
logistic_data$dffit<-dffits(model)
logistic_data$leverage<-hatvalues(model)

logistic_data[, c("leverage", "studentized.residuals", "dfbeta")]
write.csv(logistic_data, "Res.csv")

###########################################   Model has been build  ##############################################
###########################################   Testing on the test dataset  #######################################




# Logistic Regression on full data


modelt <- glm(Default_On_Payment~I(Status_Checking_Acc=="A12")+
                I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")+
                Duration_in_Months+
                I(Credit_History=="A34")+I(Purposre_Credit_Taken=="A41")+
                I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+
                I(Purposre_Credit_Taken=="A43")+
                I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")+
                I(Savings_Acc=="A62")+I(Savings_Acc=="A64")+I(Savings_Acc=="A65")+I(Years_At_Present_Employment=="A74")+
                Inst_Rt_Income+I(Marital_Status_Gender=="A93")+
                
                I(Other_Debtors_Guarantors=="A103")+
                
                Age+I(Other_Inst_Plans=="A143")+
                I(Housing=="A152")+I(Foreign_Worker=="A202"),
              data=data.test, family=binomial())
summary(modelt)

#Removing Age and SavingS_Acc==A62
modelt<-glm(Default_On_Payment~I(Status_Checking_Acc=="A12")+
      I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")+
      Duration_in_Months+
      I(Credit_History=="A34")+I(Purposre_Credit_Taken=="A41")+
      I(Purposre_Credit_Taken=="A410")+I(Purposre_Credit_Taken=="A42")+
      I(Purposre_Credit_Taken=="A43")+
      I(Purposre_Credit_Taken=="A48")+I(Purposre_Credit_Taken=="A49")+
      I(Savings_Acc=="A64")+I(Savings_Acc=="A65")+I(Years_At_Present_Employment=="A74")+
      Inst_Rt_Income+I(Marital_Status_Gender=="A93")+
      
      I(Other_Debtors_Guarantors=="A103")+
      
      I(Other_Inst_Plans=="A143")+
      I(Housing=="A152")+I(Foreign_Worker=="A202"),
    data=data.test, family=binomial())
summary(modelt)

#Removing Status_CHecking_Acc==A12 AND Purposre_Credit_Taken==A48,A42,Duration_In_Months
modelt<-glm(Default_On_Payment~
              I(Status_Checking_Acc=="A13")+I(Status_Checking_Acc=="A14")+
              
              I(Credit_History=="A34")+I(Purposre_Credit_Taken=="A41")+
              I(Purposre_Credit_Taken=="A410")+
              I(Purposre_Credit_Taken=="A43")+
              I(Purposre_Credit_Taken=="A49")+
              I(Savings_Acc=="A64")+I(Savings_Acc=="A65")+I(Years_At_Present_Employment=="A74")+
              Inst_Rt_Income+I(Marital_Status_Gender=="A93")+
              
              I(Other_Debtors_Guarantors=="A103")+
              
              I(Other_Inst_Plans=="A143")+
              I(Housing=="A152")+I(Foreign_Worker=="A202"),
            data=data.test, family=binomial())
summary(modelt)


vif(modelt)




library(car)
library(mlogit)

# Difference between -2LL of Null model and model with variables
modelChi <- modelt$null.deviance - modelt$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- modelt$df.null - modelt$df.residual
chidf

# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)





######### Lackfit Deviance ######################################################
residuals(modelt) # deviance residuals
residuals(modelt, "pearson") # pearson residuals

sum(residuals(modelt, type = "pearson")^2)
deviance(modelt)

#########Large p value indicate good model fit
1-pchisq(deviance(modelt), df.residual(modelt))

#######################################################################################
#Function - HS Test

hosmerlem <- function (y, yhat, g = 10) {
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 1, 1/g)),
                 include.lowest = TRUE)
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2 / expect)
  P <- 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}
################################################################################################################
# How to use the function. Data.train is the name of the dataset. model is name of the glm output
## High p value incidates the model fits well

hosmerlem(y = data.test$Default_On_Payment, yhat = fitted(modelt))
################################################################################################################
# Hosmer and Lemeshow test in a different way
## High p value incidates the model fits well

library(ResourceSelection)
hl <- hoslem.test(data.test$Default_On_Payment, fitted(modelt), g=10)
hl
#####################################################################################################################
# Coefficients (Odds)
modelt$coefficients
# Coefficients (Odds Ratio)
exp(modelt$coefficients)

# Predicted Probabilities
prediction <- predict(modelt,newdata = data.test,type="response")
prediction

#write.csv(prediction, file = "C:\\Users\\Subhojit\\Desktop\\Logistic Regression\\Prepared by me\\pred.csv")


rocCurve   <- roc(response = data.test$Default_On_Payment, predictor = prediction, 
                  levels = rev(levels(data.test$Default_On_Payment)))
data.test$Default_On_Payment <- as.factor(data.test$Default_On_Payment)


#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.test$Default_On_Payment)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)



#########################################################################################################################
### KS statistics calculation
data.test$m1.yhat <- predict(modelt, data.test, type = "response")

library(ROCR)
m1.scores <- prediction(data.test$m1.yhat, data.test$Default_On_Payment)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 40 - 70

############################################################################################################

#########################################################################################################################
###################### Residual Analysis ################################################################################


logistic_data <- data.test

logistic_data$predicted.probabilities<-fitted(modelt)
logistic_data$standardized.residuals<-rstandard(modelt)
logistic_data$studentized.residuals<-rstudent(modelt)
logistic_data$dfbeta<-dfbeta(modelt)
logistic_data$dffit<-dffits(modelt)
logistic_data$leverage<-hatvalues(modelt)

#logistic_data[, c("leverage", "studentized.residuals", "dfbeta")]
#write.csv(logistic_data)



#######################################################################################################

##########################################





