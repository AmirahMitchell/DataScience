#Project 1
#Amirah Mitchell
#Age Specific fertility rate (ASFR) estmiates 1950-2021 and forecasts 2022-2100

rm(list=ls())
setwd("/Users/amirahmitchell/Documents/DSCI 293A/Week 8")

#for lasso model
library(glmnet)

fert_data <- read.csv("~/Documents/DSCI 293A/Week 8/IHME_GBD_2021_FERTILITY_1950_2100_ASFR_Y2024M03D15.CSV")

--------------------------------------------------------------------------------

#finding the best model 
summary(fert_data)

#unique values
unique(fert_data$location_name)

unique(fert_data$scenario_name)

unique(fert_data$age_group_name)

#rows of the total dataset
n <- nrow(fert_data)

#random seed
set.seed(3438)

#random sample for training and test datasets
mysamp <- sample(1:n, round(.8*n))

#create testing dataset
mytrain <- fert_data[mysamp,]
testdata <- fert_data[-mysamp,]

#create training dataset
#Get the number of rows from the mytrain subset
i <- nrow(mytrain)

#draw the random smaple for the validation data 
valsamp <- sample(1:i, round(0.8*i))

#Do the final construction of the training and validation data sets
traindata <- mytrain[valsamp,]
valdata <- mytrain[-valsamp,]

--------------------------------------------------------------------------------------------

#linear model
linear_m <- lm(lower ~ age_group_id, location_id, year_id, data=traindata)

#polynomial regression
poly_m <- lm(lower ~ poly(age_group_id, 3), location_id, year_id, data=traindata)

#Lasso Model
#using K-fold = 10, CV to find best lambda value 
pred_v <- data.matrix(traindata[ ,c('location_id', 'year_id','age_group_id', 'scenario_id')])
resp_v <- traindata$lower

lasso_a <- cv.glmnet(pred_v,resp_v, alpha=1)

best_lambda <- lasso_a$lambda.min

lasso_m <- glmnet(pred_v,resp_v, alpha = 1, lambda = best_lambda, type.measure='deviance')

#Mean model 
mean_m <- traindata$val

--------------------------------------------------------------------------------
#MSE VALIDATION
#Predictions with validation dataset
pred1 <- predict(linear_m, newdata=valdata)
pred2 <- predict(poly_m, newdata=valdata)
newx_v <- data.matrix(valdata[ ,c('location_id', 'year_id','age_group_id', 'scenario_id')])
pred3 <- predict(lasso_m, newx=newx_v)
pred4 <- valdata$val

#MSE for val dataset
mse1 <- sum((pred1-valdata$lower)^2)/nrow(valdata)
mse2 <- sum((pred2-valdata$lower)^2)/nrow(valdata)
mse3 <- sum((pred3-valdata$lower)^2)/nrow(valdata)
mse4 <- sum((pred4-valdata$lower)^2)/nrow(valdata)

#Best model on val data
#make a table
row_1 <- c(mse1)
row_2 <- c(mse2)
row_3 <- c(mse3)
row_4 <- c(mse4)

matrix_data <- rbind(row_1,row_2,row_3,row_4)

colnames(matrix_data) <- c('Validation MSE')
rownames(matrix_data) <- c('LM','PM', 'LAM', 'MM')

table = as.table(matrix_data)

table
--------------------------------------------------------------------------------
#Re-doing the Lasso Model
#using K-fold = 10, CV to find best lambda value 
pred_v1 <- data.matrix(traindata[ ,c('location_id','age_group_id','year_id')])
resp_v1 <- traindata$lower

lasso_a1 <- cv.glmnet(pred_v1,resp_v1, alpha=1)

best_lambda1 <- lasso_a1$lambda.min

lasso_m1 <- glmnet(pred_v1,resp_v1, alpha = 1, lambda = best_lambda1, type.measure='deviance')
  
newx_v1 <- data.matrix(valdata[ ,c('location_id','age_group_id','year_id')])

pred5 <- predict(lasso_m1, newx=newx_v1)
  
mse5 <- sum((pred5-valdata$lower)^2)/nrow(valdata)

row_5 <- c(mse5)

matrix_data <- rbind(row_1,row_2,row_3,row_4,row_5)

colnames(matrix_data) <- c('Validation MSE')
rownames(matrix_data) <- c('LM','PM', 'LAM1', 'MM','LAM2')

table = as.table(matrix_data)

table
--------------------------------------------------------------------------------
#r^2 validation for all models 
  
R_squared1 <- 1-((sum((pred1-valdata$lower)^2))/(sum(pred1-(valdata$lower/nrow(valdata)))^2))
R_squared2 <- 1-((sum((pred2-valdata$lower)^2))/(sum(pred2-(valdata$lower/nrow(valdata)))^2))
R_squared3 <- 1-((sum((pred3-valdata$lower)^2))/(sum(pred3-(valdata$lower/nrow(valdata)))^2))
R_squared4 <- 1-((sum((pred4-valdata$lower)^2))/(sum(pred4-(valdata$lower/nrow(valdata)))^2))
R_squared5 <- 1-((sum((pred5-valdata$lower)^2))/(sum(pred5-(valdata$lower/nrow(valdata)))^2))

matrix_data1 <- rbind(R_squared1,R_squared2,R_squared3,R_squared4,R_squared5)

colnames(matrix_data1) <- c('Validation r^2')
rownames(matrix_data1) <- c('LM','PM', 'LAM1', 'MM','LAM2')

table1 = as.table(matrix_data1)

table1
--------------------------------------------------------------------------------
#MSE TEST DATA
#Predictions with test dataset
pred11 <- predict(linear_m, newdata=testdata)
pred22 <- predict(poly_m, newdata=testdata)
newx_t <- data.matrix(testdata[ ,c('location_id', 'year_id','age_group_id', 'scenario_id')])
pred33 <- predict(lasso_m, newx=newx_t)
pred44 <- testdata$val
newx_t1 <- data.matrix(testdata[ ,c('location_id','age_group_id','year_id')])
pred55 <- predict(lasso_m1, newx=newx_t1)

#MSE for test dataset
mse11 <- sum((pred11-testdata$lower)^2)/nrow(testdata)
mse22 <- sum((pred22-testdata$lower)^2)/nrow(testdata)
mse33 <- sum((pred33-testdata$lower)^2)/nrow(testdata)
mse44 <- sum((pred44-testdata$lower)^2)/nrow(testdata)
mse55 <- sum((pred55-testdata$lower)^2)/nrow(testdata)

matrix_data11 <- rbind(mse11,mse22, mse33, mse44, mse55)

colnames(matrix_data11) <- c('Test MSE')
rownames(matrix_data11) <- c('LM','PM', 'LAM1', 'MM','LAM2')

table11 = as.table(matrix_data11)

table11

--------------------------------------------------------------------------------
#r^2 validation for all models 
  
R_squared11 <- 1-((sum((pred11-testdata$lower)^2))/(sum(pred11-(testdata$lower/nrow(testdata)))^2))
R_squared22 <- 1-((sum((pred22-testdata$lower)^2))/(sum(pred22-(testdata$lower/nrow(testdata)))^2))
R_squared33 <- 1-((sum((pred33-testdata$lower)^2))/(sum(pred33-(testdata$lower/nrow(testdata)))^2))
R_squared44 <- 1-((sum((pred44-testdata$lower)^2))/(sum(pred44-(testdata$lower/nrow(testdata)))^2))
R_squared55 <- 1-((sum((pred55-testdata$lower)^2))/(sum(pred55-(testdata$lower/nrow(testdata)))^2))

matrix_data111 <- rbind(R_squared11,R_squared22,R_squared33,R_squared44,R_squared55)

colnames(matrix_data111) <- c('Test r^2')
rownames(matrix_data111) <- c('LM','PM', 'LAM1', 'MM','LAM2')

table22 = as.table(matrix_data111)

table22
--------------------------------------------------------------------------------
#Make a barplot for MSE
data = c(mse1,mse11, mse2,mse22, mse3,mse33, mse4, mse44, mse5, mse55)
names = c('LM V', 'LM T','PM V','PM T', 'LAM1 V', 'LAM1 T', 'MM V', 'MM T', 'LAM1 V', 'LAM2 T')
barplot(data, names.arg = names, ylab = "MSE",ylim=c(0.0001,0.0029),main = 'MSE for Out-of-Sample and In-Sample Data')


--------------------------------------------------------------------------------
  
#Make a barplot for r^2
data = c(R_squared1,R_squared11,R_squared2,R_squared22,R_squared3,R_squared33,R_squared4,R_squared44,R_squared5,R_squared55)
names = c('LM V', 'LM T','PM V','PM T', 'LAM1 V', 'LAM1 T', 'MM V', 'MM T', 'LAM1 V', 'LAM2 T')
barplot(data, names.arg = names, ylab = "R^2",ylim=c(0.97,1),main = 'R^2 for Out-of-Sample and In-Sample Data')

