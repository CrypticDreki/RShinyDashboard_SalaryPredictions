#Libraries
library(shiny)
library(shinydashboard)
library(xgboost)
library(caret)
library(ggplot2) 
library(gridExtra)
library(dplyr)
library(forecast)


final_data <- read.csv("final_data.csv")
salaries <- data.frame(company = final_data$company, job_title = final_data$title, 
                       salary = final_data$totalyearlycompensation, time = final_data$timestamp)
salaries$time <- as.Date(salaries$time, format="%m/%d/%Y %H:%M:%S")

#xgboost prep
## Dummy Variable for tag
cleaning_tag <- tolower(final_data$tag)#consistent formatting

final_data$analytics_tag <- rep(0, length(cleaning_tag))
final_data$analytics_tag[grep('analytic', cleaning_tag)] <-  1 
final_data$analytics_tag[grep('data', cleaning_tag)] <-  1 
final_data$analytics_tag[grep('dev', cleaning_tag)] <-  1 
#analytics_tag[grep('', cleaning_tag)] <-  1 #analytics, data, data development, data science?
table(final_data$analytics_tag)

final_data$finance_tag <- rep(0, length(cleaning_tag))
final_data$finance_tag[grep('finance', cleaning_tag)] <-  1 
final_data$finance_tag[grep('fin', cleaning_tag)] <-  1 
table(final_data$finance_tag) #not a ton of finance

final_data$engineering_tag <- rep(0, length(cleaning_tag)) 
final_data$engineering_tag[grep('eng', cleaning_tag)] <-  1 #any language
final_data$engineering_tag[grep('ml', cleaning_tag)] <-  1 
final_data$engineering_tag[grep('ai', cleaning_tag)] <-  1 
table(final_data$engineering_tag)

final_data$economics_tag <- rep(0, length(cleaning_tag)) 
final_data$economics_tag[grep('econ', cleaning_tag)] <-  1 #econmics + e commerce
final_data$economics_tag[grep('e-com', cleaning_tag)] <-  1
final_data$economics_tag[grep('e com', cleaning_tag)] <-  1
table(final_data$economics_tag) #that really isn't as expansive as I thought

final_data$marketing_tag <- rep(0, length(cleaning_tag)) 
final_data$marketing_tag[grep('market', cleaning_tag)] <-  1 
final_data$marketing_tag[grep('adver', cleaning_tag)] <-  1
final_data$marketing_tag[grep('strat', cleaning_tag)] <-  1
table(final_data$marketing_tag)

final_data$director_tag <- rep(0, length(cleaning_tag)) 
final_data$director_tag[grep('director', cleaning_tag)] <-  1
final_data$director_tag[grep('dir', cleaning_tag)] <-  1
table(final_data$director_tag)

final_data$UX_tag <- rep(0, length(cleaning_tag)) 
final_data$UX_tag[grep('ux', cleaning_tag)] <-  1
final_data$UX_tag[grep('user exp', cleaning_tag)] <-  1
table(final_data$UX_tag)

final_data$mobile_tag <- rep(0, length(cleaning_tag)) 
final_data$mobile_tag[grep('mobile', cleaning_tag)] <-  1
final_data$mobile_tag[grep('ios', cleaning_tag)] <-  1
final_data$mobile_tag[grep('android', cleaning_tag)] <-  1
table(final_data$mobile_tag)

final_data$security_tag <- rep(0, length(cleaning_tag)) 
final_data$security_tag[grep('security', cleaning_tag)] <-  1
final_data$security_tag[grep('sit reli', cleaning_tag)] <-  1
table(final_data$security_tag)

final_data$production_tag <- rep(0, length(cleaning_tag)) 
final_data$production_tag[grep('production', cleaning_tag)] <-  1
final_data$production_tag[grep('product', cleaning_tag)] <-  1
final_data$production_tag[grep('front', cleaning_tag)] <-  1
table(final_data$production_tag)

model_data <- final_data[,-c(1:2,5,9:12,14,25:27)]
model_data$title <- as.numeric(as.factor(model_data$title)) 
model_data$gender <- ifelse(model_data$gender == "Male", 1, 0)   
model_data$state <- as.numeric(as.factor(model_data$state))
model_data$company <- as.numeric(as.factor(model_data$company))

#xgboost model
# Data Partition
set.seed(7)
total_obs <- dim(model_data)[1]
train_data_indices <- sample(1:total_obs, 0.8*total_obs)
train_data <- model_data[train_data_indices,]
test_data <- model_data[-train_data_indices,]

# Create xgboost data
dtrain <- xgb.DMatrix(data = as.matrix(train_data[, c(1:2, 4:ncol(train_data))]), label = train_data$totalyearlycompensation)
dtest <- xgb.DMatrix(data = as.matrix(test_data[, c(1:2, 4:ncol(test_data))]), label = test_data$totalyearlycompensation)

# Model!
set.seed(111111)
bst <- xgb.train(data = dtrain, 
                 max.depth = 5, 
                 watchlist=list(train=dtrain, test=dtest), 
                 nrounds = 40)
#pred_y <-  predict(bst, dtest) #just a test

