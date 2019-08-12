#############################################
#=============Read in Libraries=============#
# Read in the necessary libraries.          #
#############################################
#install.packages("caret")

library(psych)
library(ggplot2)
library(sqldf)
library(Hmisc)
library(SamplingStrata)
library(caret)

#####################################################
#============Setup the Working Directory============#
# Set the working directory to the project folder by#
# running the appropriate code below.               #
#####################################################

working_directory = "D:\\Priyank\\OSU\\Data\\Spring 2018\\Programming in R and Python"
setwd(working_directory)

#############################################
#===============Read in data================#
#Read in the data for both train and test	  #
#data sets.	  					  #
#############################################

#Convert empty fields, and strings NAs to proper NA fields while reading data
credit_train = read.table("loan_data_train.csv", header=T, sep=",", na.strings=c("", " ", "NA"))
credit_test = read.table("loan_data_test.csv", header=T, sep=",")

names(credit_train)
nrow(credit_train) #68816

names(credit_test)
nrow(credit_test) #31184

#############################################
#============Data Consolidation=============#
#Consolidate both data files for data	  #
#cleaning and transformation  		  #
#############################################

loan_data <- rbind(credit_train, credit_test)
nrow(loan_data)

#Renaming column names in dataset for simplicity
colnames(loan_data) = c('loan_id', 'customer_id', 'loan_status', 'current_loan_amount', 'term', 'credit_score', 'annual_income', 'years_in_current_job', 'home_ownership', 'purpose', 'monthly_debt', 'years_of_credit_history', 'months_since_last_delinquent', 'number_of_open_accounts', 'number_of_credit_problems', 'current_credit_balance', 'maximum_open_credits', 'bankruptcies', 'tax_liens')
names(loan_data)

#############################################
#===============Data Cleaning===============#
#Clean the data to make it ready for 	  #
#analysis  					        #
#############################################

#Datatypes for the loan data
str(loan_data)

#Remove duplicate rows from the dataframe, if any
loan_data <- unique(loan_data)
nrow(loan_data) #89785

#Performing data cleaning for other factor variables
unique(loan_data$loan_status)
unique(loan_data$term)
unique(loan_data$purpose)
unique(loan_data$years_in_current_job) #contains n/a level

#Converting n/a level in Years in Current Job to a proper NA record
loan_data$years_in_current_job[loan_data$years_in_current_job == 'n/a'] <- NA
sum(is.na(loan_data$years_in_current_job)) #3802
#Removing empty fields in years in current job
loan_data <- loan_data[!is.na(loan_data$years_in_current_job),]
nrow(loan_data) #85983

unique(loan_data$home_ownership)
plot(loan_data$home_ownership)

nrow(loan_data[loan_data$home_ownership == 'HaveMortgage',]) #176

#Merging HaveMortgage to Home Mortgage due to less sample size
lo



an_data$home_ownership[loan_data$home_ownership == 'HaveMortgage'] <- 'Home Mortgage'
loan_data$home_ownership <- factor(loan_data$home_ownership)
plot(loan_data$home_ownership)

#Checking for empty records in numeric variables
sum(is.na(loan_data$current_loan_amount))
sum(is.na(loan_data$monthly_debt)) 
sum(is.na(loan_data$years_of_credit_history))
sum(is.na(loan_data$months_since_last_delinquent)) #46286 - most of the values are null, this column is not included in analysis

sum(is.na(loan_data$number_of_open_accounts))
sum(is.na(loan_data$number_of_credit_problems))
sum(is.na(loan_data$current_credit_balance))

sum(is.na(loan_data$maximum_open_credits)) #2
#Removing empty fields in maximum open credits
loan_data <- loan_data[!is.na(loan_data$maximum_open_credits),]
nrow(loan_data) #85981

sum(is.na(loan_data$bankruptcies)) #190
#Removing empty fields in bankruptcies
loan_data <- loan_data[!is.na(loan_data$bankruptcies),]
nrow(loan_data) #85791

sum(is.na(loan_data$tax_liens))

sum(is.na(loan_data$credit_score)) #18301
boxplot(loan_data$credit_score)

#Since the sample with NA values is high, it is better to replace it with median than removing it
#Before replacement, we need to take care of the credit score range i.e. 300 to 850
summary(loan_data$credit_score)
sqldf("select count(*) from loan_data where credit_score > 850") #4294
#Replacing the NA values in credit score with the median value of the remaining records
loan_data$credit_score[is.na(loan_data$credit_score)] <- median(loan_data$credit_score[loan_data$credit_score < 850],na.rm=TRUE)
#Removing the rows with invalid credit score
loan_data <- loan_data[!(loan_data$credit_score > 850),]

nrow(loan_data) #81497
boxplot(loan_data$credit_score)

sum(is.na(loan_data$annual_income)) #18301
boxplot(loan_data$annual_income)
#Since the sample with NA values is high, it is better to replace it with median than removing it
loan_data$annual_income[is.na(loan_data$annual_income)] <- median(loan_data$annual_income, na.rm=TRUE)
boxplot(loan_data$annual_income)

###########################################
# Exporting Clean Data as a separate file #
#                                         #
###########################################

write.table(loan_data,'clean_loan_data.csv',sep=',', row.names=FALSE)

###########################################
# Data Transformation                     #
#                                         #
###########################################

#Years in current job
unique(loan_data$years_in_current_job)
#Convert into numeric variable for better use in analysis
#First handle <1 year and 10+ year levels
levels(loan_data$years_in_current_job)[levels(loan_data$years_in_current_job)=="< 1 year"] <- "1 year"
levels(loan_data$years_in_current_job)[levels(loan_data$years_in_current_job)=="10+ years"] <- "10 years"
loan_data$years_in_current_job <- factor(loan_data$years_in_current_job)
summary(loan_data$years_in_current_job)

#Transforming the factor variable to a separate numeric variable
loan_data$years_in_current_job <- as.numeric(substr(loan_data$years_in_current_job,1,2))
summary(loan_data$years_in_current_job)
hist(loan_data$years_in_current_job)
summary(loan_data$years_in_current_job)

###########################################
# Final Transformed dataset               #
#                                         #
###########################################
write.table(loan_data,'final_loan_data.csv',sep=',', row.names=FALSE)

### Credit Score
p1 <- qplot(x = credit_score, data = loan_data, bins = 50)
summary(loan_data$credit_score)
p2 <- qplot(x = log10(credit_score), data = loan_data)
summary(log10(loan_data$credit_score))
p3 <- qplot(x = sqrt(credit_score), data = loan_data)
summary(sqrt(loan_data$credit_score))

### Current loan amount
p4 <- qplot(x = current_loan_amount, data = loan_data, bins = 1000)
summary(loan_data$current_loan_amount)
p5 <- qplot(x = log10(current_loan_amount), data = loan_data)
summary(log10(loan_data$current_loan_amount))
p6 <- qplot(x = sqrt(current_loan_amount), data = loan_data, bins = 1000)
summary(sqrt(loan_data$current_loan_amount))

### Annual Income
p7 <- qplot(x = annual_income, data = loan_data, bins =1000)
summary(loan_data$annual_income)
p8 <- qplot(x = log10(annual_income), data = loan_data)
summary(log10(loan_data$annual_income))
p9 <- qplot(x = sqrt(annual_income), data = loan_data, bins = 1000)
summary(sqrt(loan_data$annual_income))

### Monthly Debt
p10 <- qplot(x = monthly_debt, data = loan_data, bins =1000)
summary(loan_data$monthly_debt)
p11 <- qplot(x = log10(monthly_debt), data = loan_data)
summary(log10(loan_data$monthly_debt))
p12 <- qplot(x = sqrt(monthly_debt), data = loan_data, bins = 1000)
summary(sqrt(loan_data$monthly_debt))

### Current Credit Balance
p13 <- qplot(x = current_credit_balance, data = loan_data, bins =1000)
summary(loan_data$current_credit_balance)
p14 <- qplot(x = log10(current_credit_balance), data = loan_data)
summary(log10(loan_data$current_credit_balance))
p15 <- qplot(x = sqrt(current_credit_balance), data = loan_data, bins = 1000)
summary(sqrt(loan_data$current_credit_balance))

### Maximum Open Credits
p16 <- qplot(x = maximum_open_credits, data = loan_data, bins =1000)
summary(loan_data$maximum_open_credits)
p17 <- qplot(x = log10(maximum_open_credits), data = loan_data)
summary(log10(loan_data$maximum_open_credits))
p18 <- qplot(x = sqrt(maximum_open_credits), data = loan_data, bins = 1000)
summary(sqrt(loan_data$maximum_open_credits))

### Years in current job
p19 <- qplot(x = years_in_current_job, data = loan_data, bins =10)
summary(loan_data$years_in_current_job)
p20 <- qplot(x = log10(years_in_current_job), data = loan_data)
summary(log10(loan_data$maximum_open_credits))
p21 <- qplot(x = sqrt(years_in_current_job), data = loan_data, bins = 10)
summary(sqrt(loan_data$maximum_open_credits))


###########################################
# Data Reduction                          #
#                                         #
###########################################

str(loan_data)

################Taking all the Numeric and Integer values for PCA###############

pca_Data <- loan_data[,sapply(loan_data,is.numeric)]

str(pca_Data)

##################Removing Null data for PCA########################

pca_model <-princomp(na.omit(pca_Data),cor = TRUE )

print(pca_model)

pca_model$sdev^2

##########################Plotting ScreePlot###########################

screeplot(pca_model,npcs = 12,type = c("barplot","lines"))


###########################################
# Descriptive Statistics                  #
#                                         #
###########################################

str(credit_train)

summary(credit_train)

describe(credit_train$credit_score)

describe(credit_train$current_loan_amount)

describe(log10(credit_train$annual_income))

describe(log10(credit_train$monthly_debt))

describe(log10(credit_train$current_credit_balance))

describe(credit_train$maximum_open_credits)

ct = table(credit_train$loan_status)

L_Status = as.data.frame(ct)

L_Status

pt = table(credit_train$purpose)

purp = as.data.frame(pt)

purp

str(loan_data)
numeric_data = loan_data[c('current_loan_amount','credit_score','annual_income','years_in_current_job','monthly_debt','years_of_credit_history','number_of_open_accounts','current_credit_balance','number_of_credit_problems','maximum_open_credits','bankruptcies','tax_liens')]
ncol(numeric_data)

pairs(numeric_data,panel=panel.smooth)

barplot(loan_data, xlab='credit_score', ylab='loan_status')
rcorr(as.matrix(numeric_data))

counts <- table(loan_data$loan_status, loan_data$term)
barplot(counts, main="Term Vs Loan Status", xlab="term", col=c("orange","blue"), legend = rownames(counts), beside=TRUE)

counts <- table(loan_data$loan_status, loan_data$home_ownership)
barplot(counts, main="Home Ownership Vs Loan Status", xlab="home_ownership", col=c("orange","blue"), legend = rownames(counts), beside=TRUE)

###########################################
# Data Splitting				      #
###########################################

summary(loan_data$loan_status)--17021
#Splitting the data on target variable
loan_data_split <- split(loan_data,loan_data$loan_status)
loan_data_split.notpaid <- loan_data_split[[1]]
loan_data_split.paid <- loan_data_split[[2]]

loan_data_split.paid_sample <- loan_data_split.paid[sample(1:nrow(loan_data_split.paid), 17021, replace=F),]
describe(loan_data_split.paid_sample)

#For Charged Off/Not Paid
set.seed(123)
notpaid_sample <- sample.int(n=nrow(loan_data_split.notpaid), size=floor(0.7*nrow(loan_data_split.notpaid)), replace = F)
notpaid_train <- loan_data_split.notpaid[notpaid_sample, ]
notpaid_test <- loan_data_split.notpaid[-notpaid_sample, ]

describe(notpaid_train)--11914
describe(notpaid_test)--5107

#For Fully Paid
set.seed(124)
paid_sample <- sample.int(n=nrow(loan_data_split.paid_sample), size=floor(0.7*nrow(loan_data_split.paid_sample)), replace = F)
paid_train <- loan_data_split.paid_sample[paid_sample, ]
paid_test <- loan_data_split.paid_sample[-paid_sample, ]

describe(paid_train)--11914
describe(paid_test)--5107

######################################
# Combining the test/train data
######################################

loan_data.train <- rbind(notpaid_train, paid_train)
loan_data.test <- rbind(notpaid_test, paid_test)

nrow(loan_data.train)
nrow(loan_data.test)

#########################################
# Summary of test and train data sets
#########################################

summary(loan_data.train)
summary(loan_data.test)

write.table(loan_data.train,'loan_data_train.csv',sep=',', row.names=FALSE)
write.table(loan_data.test,'loan_data_test.csv',sep=',', row.names=FALSE)
str(loan_data.train)


loan_data.train$loan_status[loan_data.train$loan_status == "Charged Off"] <- "0"
loan_data.train$loan_status[loan_data.train$loan_status == 'Fully Paid'] <- 1

#Logistic Regression
loan_reg1 = glm(loan_status~current_loan_amount+term+credit_score+annual_income, family=binomial, data=loan_data.train)
summary(loan_reg1)$r.squared

loan_reg2 = glm(loan_status~current_loan_amount+term+credit_score+annual_income+years_in_current_job+home_ownership+monthly_debt+years_of_credit_history+number_of_open_accounts+number_of_credit_problems+current_credit_balance+maximum_open_credits+bankruptcies+tax_liens, family = binomial, data=loan_data.train)
summary(loan_reg2)

loan_reg3 = glm(loan_status~current_loan_amount+term+credit_score+annual_income+home_ownership+monthly_debt+years_of_credit_history+number_of_open_accounts, family = binomial, data=credit_test)
summary(loan_reg3)

predictions <- predict.lm(loan_reg3, credit_test)
rmse <- mean((credit_test$credit_test - predictions)^2)
print(rmse)

loan_reg4 = glm(loan_status~current_loan_amount+term+credit_score+annual_income+home_ownership+monthly_debt+years_of_credit_history, family = binomial, data=loan_data.train)
summary(loan_reg4)

############################################
# Predicting on test data			 #
############################################
pred <- predict(loan_reg3, loan_data.test)

confusionMatrix(pred, loan_data.test$loan_status)

install.packages("tree")
library(tree)
loan_tree = tree(loan_data.train1)
loan_data.train1 <- loan_data.train[c('loan_status','current_loan_amount','term','credit_score','annual_income','home_ownership','monthly_debt','years_of_credit_history','number_of_open_accounts')]

loan_tree1 = tree(credit_train)
plot(loan_tree1)
text(loan_tree1)

loan_tree2 = tree(loan_status~current_loan_amount+term+credit_score+annual_income+home_ownership+monthly_debt+years_of_credit_history+number_of_open_accounts, credit_train)
plot(loan_tree2)
text(loan_tree2)

loan_data.test1 <- loan_data.test[c('loan_status','current_loan_amount','term','credit_score','annual_income','home_ownership','monthly_debt','years_of_credit_history','number_of_open_accounts')]

loan_tree2 = tree(loan_data.test1)
plot(loan_tree2)
text(loan_tree2)

decision.tree.train <- data.frame(predict(loan_tree2, credit_train, type = "class" ))
decisions.tree.train <- data.frame(Prediction = as.numeric(decision.tree.train[,1])-1,Actual = as.numeric(credit_train$loan_status)-1)
decisions.tree.train$Correct <- (decisions.tree.train$Actual == decisions.tree.train$Prediction)
Tree_Accuracy <- table(decisions.tree.train$Correct)/length(decisions.tree.train$Correct)*100
Tree_Accuracy
loan_tree2
head()

prediction.tree.test <- data.frame(predict(loan_tree2, credit_test, type = "class"))
predictions.tree.t <- data.frame(Prediction = as.numeric(prediction.tree.test[,1])-1, Actual = as.numeric(credit_test$loan_status)-1)
predictions.tree.t$Correct <- (predictions.tree.t$Actual == predictions.tree.t$Prediction)
Tree_Accuracy.t <- table(predictions.tree.t$Correct)/length(predictions.tree.t$Correct)*100
Tree_Accuracy.t

install.packages("caret")
library(caret)
pred = predict(loan_tree2, newdata=credit_test, type = "class")
install.packages('e1071', dependencies = TRUE)
confusionMatrix(pred, credit_test$loan_status)

mse <- mean((credit_train$loan_test - pred)^2)
install.packages("ROCR")
library(ROCR)
roc_pred <- prediction(predictions.tree.t$Actual, predictions.tree.t$Prediction)
plot(performance(roc_pred,measure="trp", x.measure = "fpr"))
auc<- performance(roc_pred, measure = "auc")
auc = auc@y.values[[1]]
auc
rocplot(loan_reg3)
