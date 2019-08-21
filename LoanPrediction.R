

#################### Import data by setting Working directory ###########
#########################################################################

setwd("C:/Sourav/Technology/DataScience Projects/Machine Learning/R/LoanPrediction/Data/")
loanTrain <- read.csv("LoanPrediction.csv", na.strings = c(""," ","NA"))

####################### Data Manipulation ###############################
#########################################################################

##########################  1. Check for duplicates #####################
#########################################################################

loanTrain<-loanTrain[!duplicated(loanTrain),]

##########################  2. Missing Values treatment #################
#########################################################################

### (a.)  Visualize Na terms..........
library(Amelia)
missmap(loanTrain)
sapply(loanTrain,function(x) sum(is.na(x)))

### (b.)  Delete Obervations with NA values........ (optional, if needed)
#compTrain <- na.omit(loanTrain)

### (c.)  Impute mean/median/mode .....

### Impute data or replacing missing values using Base package
##############################################################

### Analyzing indepedent variables (Eg: Loan Amount) using boxplot and histogram
### Check the existance of outliers to determine between Mean and Median 
### for data replacement

library(ggplot2)
ggplot(loanTrain, aes(1, LoanAmount)) + geom_boxplot()
hist(loanTrain$LoanAmount)

# Impute continuous variables by Median (as outliers exist)
loanTrain$LoanAmount[is.na(loanTrain$LoanAmount)]<-
  median(loanTrain$LoanAmount, na.rm = T)

## Impute Categorical variables (Credit History) by Mode
ggplot(loanTrain, aes(Credit_History)) + geom_bar()
# Mode function
mode <- function(x){
  t <- as.data.frame(table(loanTrain$Credit_History))  
  return(as.character(t$Var1[which.max(t$Freq)]))
}

loanTrain$Credit_History[is.na(loanTrain$Credit_History)]<-mode(loanTrain$Credit_History)

############ Alternate Approach :: Impute mean/median/mode #############
########################################################################

### Impute data or replacing missing values for all variables ##########
### using imputeMissings package  ######################################
########################################################################

## Verify missing value count
sapply(loanTrain,function(x) sum(is.na(x)))

# install.packages("imputeMissings")
library(imputeMissings)
l<-impute(loanTrain, method = "median/mode")

## Verify missing value count
sapply(l,function(x) sum(is.na(x)))

### Predicting missing values using Mice package (instead of replacement)
#########################################################################

#install.packages("mice")
library(mice)
d<-loanTrain[,c(2:12)]
imputed_Data <- mice(d, m=5, maxit = 50, method = 'pmm', seed = 500)

##########################  3. Outliers Treatment #########################

################### (a.1) Multivariate Model Approach #####################
###########################################################################
### The cook's distance for each observation i measures the change in Yhat (fitted Y) 
### for all observations with and without the presence of observation i, 
### so we know how much the observation i impacted the fitted values.
### Those observations that have a cook's distance greater than 4 times the mean may be 
### classified as influential. This is not a hard boundary.

mod <- lm(LoanAmount ~ .-Loan_ID, data=l)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", main="Influential Obs by Cooks distance")  # plot cook's distance
#influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance

########################## Labeling Outliers ############################
#########################################################################

abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

##################### (a.2) boxplot approach ############################
#########################################################################

library(ggplot2)
ggplot(l, aes(1,LoanAmount)) + geom_boxplot(outlier.colour = "red",
                                            outlier.shape = 2)

########################## Labeling Outliers ############################
#########################################################################
 
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

library(dplyr)
l %>%
  mutate(outlier = ifelse(is_outlier(LoanAmount), LoanAmount, as.numeric(NA))) %>%
  ggplot(.,aes(1,LoanAmount)) + geom_boxplot(fill = "steelblue",outlier.colour = "red",
                                             outlier.shape = 2)+
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3)


########################## (b)  Capping #################################
#########################################################################

boxplot(l$LoanAmount)
qnt <- quantile(l$LoanAmount, 0.75, na.rm = T)
caps <- quantile(l$LoanAmount, 0.95, na.rm = T)
H <- 1.5 * IQR(l$LoanAmount, na.rm = T)
l$LoanAmount[l$LoanAmount > (qnt +  H)] <- caps

## CoapplicantIncome
boxplot(l$CoapplicantIncome)
ggplot(l, aes(1,CoapplicantIncome)) + geom_boxplot(outlier.colour = "red",                                                   outlier.shape = 2)
qnt <- quantile(l$CoapplicantIncome, 0.75, na.rm = T)
caps <- quantile(l$CoapplicantIncome, 0.95, na.rm = T)
H <- 1.5 * IQR(l$CoapplicantIncome, na.rm = T)
l$CoapplicantIncome[l$CoapplicantIncome > (qnt +  H)] <- caps

### Applicant Income
ggplot(l, aes(1,ApplicantIncome)) + geom_boxplot(outlier.colour = "red", outlier.shape = 2)
qnt <- quantile(l$ApplicantIncome, 0.75, na.rm = T)
caps <- quantile(l$ApplicantIncome, 0.95, na.rm = T)
H <- 1.5 * IQR(l$ApplicantIncome, na.rm = T)
l$ApplicantIncome[l$ApplicantIncome > (qnt +  H)] <- caps

########################## Capping using function #######################
#########################################################################
# An outlier is not any point over the 95th percentile or below the 5th percentile. 
# Instead, an outlier is considered so if it is below the first quartile - 1.5·IQR or 
# above third quartile + 1.5·IQR.

capOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}
#View(l)
l$LoanAmount =capOutlier(l$LoanAmount) #### repeat this for all the columns

## Outlier treatment completed for below variables...
# l$ApplicantIncome =capOutlier(l$ApplicantIncome)
# ggplot(l, aes(1,ApplicantIncome)) + geom_boxplot(outlier.colour = "red",
#                                             outlier.shape = 2)

# l$CoapplicantIncome =capOutlier(l$CoapplicantIncome)
# ggplot(l, aes(1,CoapplicantIncome)) + geom_boxplot(outlier.colour = "red",
#                                                    outlier.shape = 2)


################ Bivariate Analysis for Continuous Variable #############
################ using Scatter plot & Box plot ##########################
#########################################################################

library(corrplot)
contVars<-c("ApplicantIncome","CoapplicantIncome","LoanAmount",
            "Loan_Amount_Term")
cont_df<-l[,names(l) %in% contVars]

pairs(cont_df)
corrplot(cor(cont_df), type = "full", "ellipse")

ggplot(l, aes(Property_Area, ApplicantIncome)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Gender, ApplicantIncome)) + geom_boxplot(fill = "steelblue")
ggplot(l, aes(Dependents, ApplicantIncome)) + geom_boxplot(fill = "steelblue")

############################ Data Modelling #############################
#########################################################################

################### (a.) hot encoding categorical variables #############
#########################################################################

l$Gender<- ifelse(l$Gender == "Female",0,1)
l$Married<- ifelse(l$Married == "No",0,1)
l$Education <- ifelse(l$Education == "Not Graduate",0,1)
l$Self_Employed <- ifelse(l$Self_Employed == "No",0,1)
l$Loan_Status <- ifelse(l$Loan_Status == "N",0,1)
l$Loan_Amount_Term<-as.numeric(l$Loan_Amount_Term)

### Many columns are numeric that must be converted to factor

col_list <- c("Gender","Married","Dependents","Education","Self_Employed","Credit_History","Loan_Status")
l[col_list] <- lapply(l[col_list], factor)

### stripping + sign from dependents 3+ categorydf$Dependents
l$Dependents<- substr(l$Dependents, 1, 1)
l$Dependents  <- as.factor(l$Dependents)# converting to a factor

####################### (b.) creating train and test data ###############
#########################################################################

# install.packages("caret")
library(caret)
index <- createDataPartition(l$Loan_Status,p = .75,list = F) # creating partion based on Loanststaus
train <- l[index,] # creatingtrain data
test <- l[-index,] # creating test data

### removing Loan ID from  train and test data

train <- subset(train,select = -c(Loan_ID))
test <- subset(test,select = -c(Loan_ID))

##################### (c.) Logistic Regression ##########################
#########################################################################

str(train)
logistic<-glm(Loan_Status~., family = "binomial", data = train)
summary(logistic)

##################### (d.) Multicollinearity Diagnostic #################
#########################################################################

### Multicollinearity Diagnostic using VIF :  
### as there are many independent variables which are not significant
#install.packages("car")
library(car)
vif(logistic)

### removing ApplicantIncome from train and test data
train <- subset(train,select = -c(ApplicantIncome))
#train <- subset(train,select = -c(ApplicantIncome, Married, Dependents))
test <- subset(test,select = -c(ApplicantIncome))

### Repeat: logistic regression and multicollinearity diagnostic
logistic<-glm(Loan_Status~., family = "binomial", data = train)
summary(logistic)
vif(logistic)

### prediction
prediction <- predict(logistic, newdata=test, type='response')
length(prediction)
pred <- ifelse(prediction > 0.5,1,0)

########## (e.) Calculate Accurary using Confusion Matrix ###############
#########################################################################

table(pred, test$Loan_Status)
###   0   1
### 0 TN  TP
### 1 FP  FN

############################## Check Accurary ###########################
#########################################################################

### Accuracy = (TN+FN)/Total

#install.packages("e1071")
library(caret)
pred <- factor(prediction, levels = c(0,1)) # since confusionMatrix accepts factor variables
confusionMatrix(pred, test$Loan_Status)# ~82% accuaracy

#########################################################################################

train$Loan_Status <- as.factor(train$Loan_Status)
# gradient boosting
control <- trainControl(method = 'repeatedcv',
                        number = 5,
                        repeats = 3,
                        search = 'grid')
seed <- 7
library(C50)
set.seed(seed)
metric <- 'Accuracy'
gbm_mod <- train(Loan_Status~., 
                 data = train,
                 method = 'gbm',
                 metric = metric,
                 trControl = control)
plot(gbm_mod)
summary(gbm_mod)

# make predictions
predictions<- predict(gbm_mod,test)

# summarize results
confusionMatrix<- confusionMatrix(predictions,test$Loan_Status)
confusionMatrix

