library(corrplot)
library(caTools)
library(fastDummies)
library(HH)
library(ROCR)

setwd("D:/Term 2/Machine Learning/Working Directory")
loan <- read.csv("HousingFinance1.csv",stringsAsFactors = TRUE)
summary(loan)
View(loan)

#change into factor

loan$Tier <- as.factor(loan$Tier)

#dummies

#Manual Method → In same column
loan$Build_Selfcon <- ifelse(loan$Build_Selfcon=="Builder",1,0)
loan$Marital_Status <- ifelse(loan$Marital_Status=="Married",1,0)
#fast dummies → separate column
loan <- dummy_cols(loan,select_columns = c("Tier","Employer_Type"))
View(loan)

#correlation
cr <- cor(loan[sapply(loan,is.numeric)])
corrplot.mixed(cr)
cr
write.csv(cr,"loan_cor.csv")

#Convert DV into factor
loan$Decision <- as.factor(loan$Decision)

#Split the data
split <- sample.split(loan$Decision,SplitRatio = 0.7)
loanTR <- subset(loan, split=="TRUE")
loanTS <- subset(loan, split=="FALSE")

#Building the model
#glm function for logistic regression
model1 <- glm(Decision~IAR, data = loan, family=binomial) #logistic regression type is binomial logit
summary(model1)

model2 <- glm(Decision~IAR+MarVal, data = loan, family=binomial)
summary(model2)

model3 <- glm(Decision~IAR+MarVal+BankSave_d, data = loan, family=binomial)
summary(model3)

model4 <- glm(Decision~IAR+MarVal+BankSave_d+OldEmi_d, data = loan, family=binomial)
summary(model4)

model5 <- glm(Decision~IAR+MarVal+BankSave_d+OldEmi_d+FOIR, data = loan, family=binomial)
summary(model5)

model6 <- glm(Decision~IAR+MarVal+BankSave_d+OldEmi_d+FOIR+LTV, data = loan, family=binomial)
summary(model6)
vif(model6)

model7 <- glm(Decision~IAR+MarVal+BankSave_d+OldEmi_d+FOIR+LTV+YrsAdd, data = loan, family=binomial)
summary(model7)

model8 <- glm(Decision~IAR+MarVal+BankSave_d+OldEmi_d+FOIR+LTV+YrsAdd+YrsJob, data = loan, family=binomial)
summary(model8)
vif(model8)

model9 <- glm(Decision~IAR+MarVal+BankSave_d+OldEmi_d+FOIR+LTV+YrsAdd+YrsJob+Age, data = loan, family=binomial)
summary(model9)
vif(model9)

model10 <- glm(Decision~IAR+MarVal+BankSave_d+OldEmi_d+FOIR+LTV+YrsAdd+YrsJob+Tier_2+Employer_Type_Business, data = loanTR, family=binomial)
summary(model10)
vif(model10)

1-pchisq(299.1,10)
### chi square value = null - residual
## DOF → 10 -- NO. OF IDV

## PREDICTION ##
res <- predict(model10,loanTS,type="response") #probability is stored here
head(res)
head(loanTS$Decision)
View(loanTS)

table(ActualValue=loanTS$Decision,PredictedValue=res>0.5)

#ROCR
ROCRpred <- prediction(res, loanTS$Decision) #loanTS$Decision is the actual values - Class
ROCRpref <- performance(ROCRpred,"tpr","fpr")
plot(ROCRpref,colorize=TRUE,print.cutoffs.at=seq(0.2,by=0.1)) #Threshold lies where green and aqua colour comes.
table(ActualValue=loanTS$Decision,PredictedValue=res>0.5) #0.5 is the cutoff value
table(ActualValue=loanTS$Decision,PredictedValue=res>0.6)
table(ActualValue=loanTS$Decision,PredictedValue=res>0.7) #We want more TP and Less (FN+FP)

#Generate PDF of code and result → to share results