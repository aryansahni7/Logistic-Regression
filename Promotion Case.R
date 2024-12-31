library(ggplot2)
library(corrplot)
library(caTools)

##__________________IMPORT THE DATASET___________________##
promotiontr <- read.csv("promotion_tr_MLB.csv", stringsAsFactors = TRUE)
summary(promotiontr)
View(promotiontr)
promotiontr <- promotiontr[-c(1)]

#outlier
promotiontr <- promotiontr[promotiontr$awards_won. !=10, ]

#missing value
a <- subset(promotiontr, length_of_service ==1 & KPIs_met..80.==1 & previous_year_rating!= "NA")

promotiontr$previous_year_rating[is.na(promotiontr$previous_year_rating) & promotiontr$KPIs_met..80.==1] <- 
  round(mean(a$previous_year_rating,na.rm = TRUE))

b <- subset(promotiontr , length_of_service ==1 & KPIs_met..80.==0 & previous_year_rating!= "NA")

promotiontr$previous_year_rating[is.na(promotiontr$previous_year_rating) & promotiontr$KPIs_met..80.==0] <- 
  round(mean(b$previous_year_rating,na.rm = TRUE))

##__________________SPLITTING THE DATASET___________________##
split <- sample.split(promotiontr$is_promoted,SplitRatio = 0.7)
training_data <- subset(promotiontr,split =="TRUE")
testing_data <- subset(promotiontr, split =="FALSE")


##__________________LOGISTIC REGRESSION___________________##
model1 <- glm(is_promoted ~ no_of_trainings, data = training_data,family = binomial)
summary(model1)

model2 <- glm(is_promoted ~ no_of_trainings+age, data = training_data,family = binomial)
summary(model2)

model3 <- glm(is_promoted ~ no_of_trainings+age+previous_year_rating, data = training_data,family = binomial)
summary(model3)

model4 <- glm(is_promoted ~ no_of_trainings+age+previous_year_rating+length_of_service, data = training_data,family = binomial)
summary(model4)

model5 <- glm(is_promoted ~ no_of_trainings+age+previous_year_rating+length_of_service+KPIs_met..80., data = training_data,family = binomial)
summary(model5)

model7 <- glm(is_promoted ~ no_of_trainings+age+previous_year_rating+length_of_service+KPIs_met..80.+awards_won.+avg_training_score, data = training_data,family = binomial)
summary(model7)
vif(model11)

#Better Model that I created
#model11 <- glm(is_promoted ~ KPIs_met..80. + awards_won. + avg_training_score + previous_year_rating + department_Technology + department_Analytics + department_Procurement + `department_Sales & Marketing`, data = promoTR, family = binomial)
#summary(model11)
#vif(model11)

##__________________LOGISTIC REGRESSION-----PREDICTION ON TESTING DATA___________________##
res <- predict(model7,testing_data,type = "response")
head(res)

##__________________LOGISTIC REGRESSION-----ROCR CURVE___________________##
library(ROCR)
ROCRpred<-prediction(res,testing_data$is_promoted)
ROCRperf<-performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0.2,by=0.1))

##__________________LOGISTIC REGRESSION-----CONFUSION MATRIX___________________##
table(Actualvalue=testing_data$is_promoted,predictvalue=res>0.3)


##__________________LOGISTIC REGRESSION-----PREDICTION ON promotion_ts_N file___________________##
promotion_ts <- read.csv("promotion_ts_MLB.csv",stringsAsFactors = TRUE)
Pred <- predict(model7, newdata = promotion_ts, type= "response") #uses trained model to make predictions on new data
promotion_ts$LR_is_promoted <- Pred #assigns predictions to a new column
promotion_ts$LR_is_promoted <- ifelse(promotion_ts$LR_is_promoted > 0.3,1,0) #converts probabilities to binary predictions
table(promotion_ts$LR_is_promoted) #prediction


# Decision Tree 


##__________________DECISION TREES___________________##
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(e1071)
library(caret)

##__________________DECISION TREES-GINI___________________##
DTGini <- rpart(is_promoted~., data = training_data, method = "class", parms = list(split = "gini")) #rpart used to create Decision Tree
#"class" indicates that it's a classification problem and splitting criterion is Gini Impurity

#Prediction of DT-GINI
Pred <- predict(DTGini, newdata=testing_data, type="class") #we want class predictions
table(Actualvalue=testing_data$is_promoted, Pred)
fancyRpartPlot(DTGini) #graphical representation of the decision tree model
testing_data$is_promoted <- as.factor(testing_data$is_promoted)
confusionMatrix(data=Pred,reference=testing_data$is_promoted,positive = '1') #calculates the confusion matrix for the predictions. Positive 1 specifies the positive class for which we want to compute metrics

##__________________DECISION TREES-INFORMATION GAIN___________________##
DTInformation<- rpart(is_promoted~., data = training_data, method = "class", parms = list(split = "information"))
#Prediction of DT-Information
Pred1 <- predict(DTInformation, newdata=testing_data, type="class")
table(Actualvalue=testing_data$is_promoted, Pred1)
fancyRpartPlot(DTInformation)
confusionMatrix(data=Pred1,reference=testing_data$is_promoted,positive = '1')

##__________________DECISION TREE-----PREDICTION ON promotion_ts_N file___________________##
Pred_ts <- predict(DTGini, newdata = promotion_ts, type= "class") #prediction on new data using Gini impurity
promotion_ts$DT_is_promoted <- Pred_ts #stores prediction in a new column
table(promotion_ts$DT_is_promoted) #frequency table of each class

write.csv2(promotion_ts, file= "PROMOTION_PREDICTION.csv")