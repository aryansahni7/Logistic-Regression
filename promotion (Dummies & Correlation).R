library(corrplot)
library(caTools)
library(fastDummies)
library(HH)
library(ROCR)

#Loading the data set
setwd("D:/Term 2/Machine Learning/Working Directory")
promotion_tr <- read.csv("promotion_tr_MLB.csv",stringsAsFactors = TRUE)
summary(promotion_tr)
View(promotion_tr)

#removing NA's from Previous year Rating
#promotion_tr <- na.omit(promotion_tr) → have already done directly in excel 


#Creating Dummies
promotion_tr$gender <- ifelse(promotion_tr$gender=="f",0,1)

#fast dummies
promotion_tr <- dummy_cols(promotion_tr, select_columns = c("department","education", "recruitment_channel"))


#Converting to factors
#promotion_tr$no_of_trainings <- as.factor(promotion_tr$no_of_trainings)
#promotion_tr$previous_year_rating <- as.factor(promotion_tr$previous_year_rating)
#promotion_tr$length_of_service <- as.factor(promotion_tr$length_of_service)
#promotion_tr$KPIs_met..80. <- as.factor(promotion_tr$KPIs_met..80.)
#promotion_tr$awards_won. <- as.factor(promotion_tr$awards_won.)

#Correlation
cr <- cor(promotion_tr[sapply(promotion_tr,is.numeric)]) #Apply numeric columns
corrplot.mixed(cr)
write.csv(cr,"Corr.csv")