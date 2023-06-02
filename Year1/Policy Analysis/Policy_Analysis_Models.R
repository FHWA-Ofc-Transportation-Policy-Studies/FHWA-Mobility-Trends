
#This script is used to recreate the results and graphics reported in 
#the Mobility Trends Task 6b - Policy Analysis Report 

#Note: you will need to reset the working directories throughout the script.

##### TELECOMMUTE #####

### Setup ###

#clean space
remove(list=ls())

# Loading the library
library(glmnet); library(dplyr); library(caret)

#set working directory
setwd("C:/Users/zapate/Documents/task6_6_2/FHWA-Mobility-Trends/Year1/Policy Analysis")

#load the data
dp<-read.csv("Data 20230509 - DataP.csv", header=TRUE)
df<-read.csv("Data 20230509 - DataF.csv", header=TRUE)
summary(dp)
summary(df)

##### 1) VMT ######

# Variables: VMT and X's
y_var = dp[,2]; 

### a. With EVs - EV Station Locations ####
x_vars = dp[,-c(1:5,6,13,14,15)]; 

lambda_seq=10^seq(20,5,by=-.1)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_vars), 18)
train = 1:18
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(as.matrix(x_vars[train,]), y_var[train], alpha = 1, family="gaussian", lambda = lambda_seq, 
                       nfolds = 3)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, family="gaussian", lambda = best_lam)

#Table 3-5 Coefficents 
coef(lasso_best)

pred <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars[x_test,]))
predall <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars))

#Test Predictions
final <- cbind(y_var[x_test], pred)
head(final)

#R-Square Computations
actual <- y_var
preds <- predall
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq

#Out of sample testing
d = y_test - pred
mse.out = mean((d)^2)
mae.out = mean(abs(d))
rmse.out = sqrt(mse.out)

cat(" MAE:", mae.out, "\n", "MSE:", mse.out, "\n", 
    "RMSE:", rmse.out, "\n")

#in sample testing
d = y_var[train] - predall[1:18]
mse.in = mean((d)^2)
mae.in = mean(abs(d))
rmse.in = sqrt(mse.in)

cat(" MAE:", mae.in, "\n", "MSE:", mse.in, "\n", 
    "RMSE:", rmse.in, "\n")

#prediction interval unscaled
X.X<-t(as.matrix(cbind(rep(1,18), x_vars[train,]))) %*% as.matrix(cbind(rep(1,18), x_vars[train,]))
x0<-cbind(1, as.matrix(x_vars[19,]))
x1<-cbind(1, as.matrix(x_vars[20,]))

#Plot the best model in scaled form (not in report)
#prediction interval calculation
lo.1<-pred[1,1]-2.16*sqrt(mse.in*(1+as.matrix(x0) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x0))))
lo.2<-pred[2,1]-2.16*sqrt(mse.in*(1+as.matrix(x1) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x1))))
hi.1<-pred[1,1]+2.16*sqrt(mse.in*(1+as.matrix(x0) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x0))))
hi.2<-pred[2,1]+2.16*sqrt(mse.in*(1+as.matrix(x1) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x1))))
lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)

#prediction interval plot
plot(dp$Year[1:18],dp$VMT[1:18], xlim=c(2000,2020), ylim=c(2.745396e+12,3.443086e+12), xlab="Year", ylab="VMT")
lines(dp$Year[1:18],predall[1:18],  xlim=c(2000,2020), ylim=c(2.745396e+12,3.443086e+12), col="blue")
lines(dp$Year[19:20], dp$VMT[19:20], xlim=c(2000,2020),ylim=c(2.745396e+12,3.443086e+12), type="p", pch=16)
lines(dp$Year[19:20], pred, xlim=c(2000,2020), ylim=c(2.745396e+12,3.443086e+12), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=lo.1, y1=hi.1, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=lo.2, y1=hi.2, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=lo.1, y1=lo.1, col="red")
segments(x0=2017.6,x1=2018.6, y0=hi.1, y1=hi.1, col="red")
segments(x0=2018.6,x1=2019.4, y0=lo.2, y1=lo.2, col="red")
segments(x0=2018.6,x1=2019.6, y0=hi.2, y1=hi.2, col="red")



#Future Forecast
x_vars_f = df[,c(4,5,6,7,8,9,18:22)]; #x_vars_f = x_vars_f[,-5]
pred_f <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:5,6)]))

#Future Forecasts of the 5 Scenarios
pred_f1 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:5,7)]))
pred_f2 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:5,8)]))
pred_f3 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:5,9)]))
pred_f4 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:5,10)]))
pred_f5 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:5,11)]))

#Figure 3-2: Plots of the Future Forecasts of the 5 Scenarios
plot(c(2000:2019), y_var, col='red', xlim=c(2000,2050), ylim=c(0,max(pred_f5)), 
     xlab = "", ylab = "VMT");
#lines(c(2000:2019), predall,col='green');
#lines(c(2000:2019),y_var, col='blue');
lines(c(2023:2050),pred_f1, col='red');
lines(c(2023:2050),pred_f2, col='orange');
lines(c(2023:2050),pred_f3, col='yellow');
lines(c(2023:2050),pred_f4, col='cyan');
lines(c(2023:2050),pred_f5, col='green');
legend("topleft", 
       legend = c("Scenario A", "Scenario B", "Scenario C", "Scenairo D", "Scenario E"), 
       pch = 15,
       col = c('green', 'cyan', 'yellow', 'orange', 'red'))

#==================================================================#

### b. With EVs - EV Sales ####
x_vars = dp[,-c(1:5,6,11,14,15)]; 

lambda_seq=10^seq(20,5,by=-.1)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_vars), 18)
train = 1:18
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(as.matrix(x_vars[train,]), y_var[train], alpha = 1, family="gaussian", lambda = lambda_seq, 
                       nfolds = 3)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, family="gaussian", lambda = best_lam)

# Table 3-6 coeffiecnts
coef(lasso_best)

pred <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars[x_test,]))
predall <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars))

#Test Predictions
final <- cbind(y_var[x_test], pred)
head(final)

#R-Square Computations
actual <- y_var
preds <- predall
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq


#Out of sample testing
d = y_test - pred
mse.out = mean((d)^2)
mae.out = mean(abs(d))
rmse.out = sqrt(mse.out)

cat(" MAE:", mae.out, "\n", "MSE:", mse.out, "\n", 
    "RMSE:", rmse.out, "\n")

#in sample testing
d = y_var[train] - predall[1:18]
mse.in = mean((d)^2)
mae.in = mean(abs(d))
rmse.in = sqrt(mse.in)

cat(" MAE:", mae.in, "\n", "MSE:", mse.in, "\n", 
    "RMSE:", rmse.in, "\n")

#prediction interval unscaled
X.X<-t(as.matrix(cbind(rep(1,18), x_vars[train,]))) %*% as.matrix(cbind(rep(1,18), x_vars[train,]))
x0<-cbind(1, as.matrix(x_vars[19,]))
x1<-cbind(1, as.matrix(x_vars[20,]))

#Plot the best model in scaled form (not in report)
#prediction interval calculation
lo.1<-pred[1,1]-2.16*sqrt(mse.in*(1+as.matrix(x0) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x0))))
lo.2<-pred[2,1]-2.16*sqrt(mse.in*(1+as.matrix(x1) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x1))))
hi.1<-pred[1,1]+2.16*sqrt(mse.in*(1+as.matrix(x0) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x0))))
hi.2<-pred[2,1]+2.16*sqrt(mse.in*(1+as.matrix(x1) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x1))))
lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)

#prediction interval plot
plot(dp$Year[1:18],dp$VMT[1:18], xlim=c(2000,2020), ylim=c(2.745396e+12,3.443086e+12), xlab="Year", ylab="VMT")
lines(dp$Year[1:18],predall[1:18],  xlim=c(2000,2020), ylim=c(2.745396e+12,3.443086e+12), col="blue")
lines(dp$Year[19:20], dp$VMT[19:20], xlim=c(2000,2020),ylim=c(2.745396e+12,3.443086e+12), type="p", pch=16)
lines(dp$Year[19:20], pred, xlim=c(2000,2020), ylim=c(2.745396e+12,3.443086e+12), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=lo.1, y1=hi.1, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=lo.2, y1=hi.2, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=lo.1, y1=lo.1, col="red")
segments(x0=2017.6,x1=2018.6, y0=hi.1, y1=hi.1, col="red")
segments(x0=2018.6,x1=2019.4, y0=lo.2, y1=lo.2, col="red")
segments(x0=2018.6,x1=2019.6, y0=hi.2, y1=hi.2, col="red")


#Future Forecast
x_vars_f = df[,c(4,5,6,7,9,10,18:22)]; #x_vars_f = x_vars_f[,-5]
pred_f <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:4,5,6)]))

#Future Forecasts of the 5 Scenarios
pred_f1 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:4,7,6)]))
pred_f2 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:4,8,6)]))
pred_f3 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:4,9,6)]))
pred_f4 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:4,10,6)]))
pred_f5 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:4,11,6)]))

#Figure 3-3 : Plots of the Future Forecasts of the 5 Scenarios
plot(c(2000:2019), y_var, col='red', xlim=c(2000,2050), ylim=c(0,max(pred_f5)), 
     xlab = '', ylab = "VMT");
#lines(c(2000:2019), predall,col='green');
#lines(c(2000:2019),y_var, col='blue');
lines(c(2023:2050),pred_f1, col='red');
lines(c(2023:2050),pred_f2, col='orange');
lines(c(2023:2050),pred_f3, col='yellow');
lines(c(2023:2050),pred_f4, col='cyan');
lines(c(2023:2050),pred_f5, col='green');
legend("bottomright", 
       legend = c("Scenario A", "Scenario B", "Scenario C", "Scenairo D", "Scenario E"), 
       pch = 15,
       col = c('green', 'cyan', 'yellow', 'orange', 'red'))

#==================================================================#

### c. W/O EVs ####
x_vars = dp[,-c(1:5,6,11,13,14,15)]; 

lambda_seq=10^seq(20,5,by=-.1)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_vars), 18)
train = 1:18
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(as.matrix(x_vars[train,]), y_var[train], alpha = 1, family="gaussian", lambda = lambda_seq, 
                       nfolds = 3)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, family="gaussian", lambda = best_lam)

# Table 3-7 coefficents
coef(lasso_best)

pred <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars[x_test,]))
predall <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars))

#Test Predictions
final <- cbind(y_var[x_test], pred)
head(final)

#R-Square Computations
actual <- y_var
preds <- predall
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq

#Out of sample testing
d = y_test - pred
mse.out = mean((d)^2)
mae.out = mean(abs(d))
rmse.out = sqrt(mse.out)

cat(" MAE:", mae.out, "\n", "MSE:", mse.out, "\n", 
    "RMSE:", rmse.out, "\n")

#in sample testing
d = y_var[train] - predall[1:18]
mse.in = mean((d)^2)
mae.in = mean(abs(d))
rmse.in = sqrt(mse.in)

cat(" MAE:", mae.in, "\n", "MSE:", mse.in, "\n", 
    "RMSE:", rmse.in, "\n")

#prediction interval unscaled
X.X<-t(as.matrix(cbind(rep(1,18), x_vars[train,]))) %*% as.matrix(cbind(rep(1,18), x_vars[train,]))
x0<-cbind(1, as.matrix(x_vars[19,]))
x1<-cbind(1, as.matrix(x_vars[20,]))

#Plot the best model in scaled form (not in report)
#prediction interval calculation
lo.1<-pred[1,1]-2.16*sqrt(mse.in*(1+as.matrix(x0) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x0))))
lo.2<-pred[2,1]-2.16*sqrt(mse.in*(1+as.matrix(x1) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x1))))
hi.1<-pred[1,1]+2.16*sqrt(mse.in*(1+as.matrix(x0) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x0))))
hi.2<-pred[2,1]+2.16*sqrt(mse.in*(1+as.matrix(x1) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x1))))
lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)

#prediction interval plot
plot(dp$Year[1:18],dp$VMT[1:18], xlim=c(2000,2020), ylim=c(2.745396e+12,3.443086e+12), xlab="Year", ylab="VMT")
lines(dp$Year[1:18],predall[1:18],  xlim=c(2000,2020), ylim=c(2.745396e+12,3.443086e+12), col="blue")
lines(dp$Year[19:20], dp$VMT[19:20], xlim=c(2000,2020),ylim=c(2.745396e+12,3.443086e+12), type="p", pch=16)
lines(dp$Year[19:20], pred, xlim=c(2000,2020), ylim=c(2.745396e+12,3.443086e+12), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=lo.1, y1=hi.1, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=lo.2, y1=hi.2, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=lo.1, y1=lo.1, col="red")
segments(x0=2017.6,x1=2018.6, y0=hi.1, y1=hi.1, col="red")
segments(x0=2018.6,x1=2019.4, y0=lo.2, y1=lo.2, col="red")
segments(x0=2018.6,x1=2019.6, y0=hi.2, y1=hi.2, col="red")


#Future Forecast
x_vars_f = df[,c(4,5,6,7,9,18:22)]; #x_vars_f = x_vars_f[,-5]
pred_f <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:4,5)]))

#Future Forecasts of the 5 Scenarios
pred_f1 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:4,6)]))
pred_f2 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:4,7)]))
pred_f3 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:4,8)]))
pred_f4 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:4,9)]))
pred_f5 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[c(4:31),c(1:4,10)]))

#Figure 3-4 : Plots of the Future Forecasts of the 5 Scenarios
plot(c(2000:2019), y_var, col='red', xlim=c(2000,2050), ylim=c(0,max(pred_f5)), 
     xlab = "", ylab = "VMT");
#lines(c(2000:2019), predall,col='green');
#lines(c(2000:2019),y_var, col='blue');
lines(c(2023:2050),pred_f1, col='red');
lines(c(2023:2050),pred_f2, col='orange');
lines(c(2023:2050),pred_f3, col='yellow');
lines(c(2023:2050),pred_f4, col='cyan');
lines(c(2023:2050),pred_f5, col='green');
legend("bottomright", 
       legend = c("Scenario A", "Scenario B", "Scenario C", "Scenairo D", "Scenario E"), 
       pch = 15,
       col = c('green', 'cyan', 'yellow', 'orange', 'red'))

##### 2) GHG ###############################################

#clean space
remove(list=ls())

# Loading the library
library(glmnet); library(dplyr)

#set working directory
setwd("C:/Users/zapate/Documents/task6_6_2/FHWA-Mobility-Trends/Year1/Policy Analysis")

#load the data
dp<-read.csv("Data 20230509 - DataP.csv", header=TRUE)
df<-read.csv("Data 20230509 - DataF.csv", header=TRUE)
summary(dp)
summary(df)

# Variables: GHG and X's
y_var = dp[,4];

### a. With EVs - EV Station Locations ####
x_vars = dp[,-c(1:5,6,13,14,15)]; 

lambda_seq=10^seq(5,-10,by=-.1)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_vars), 18)
train = 1:18
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(as.matrix(x_vars[train,]), y_var[train], alpha = 1, family="gaussian", lambda = lambda_seq, 
                       nfolds = 3)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, family="gaussian", lambda = best_lam)

#Table 3-8 coefficents
coef(lasso_best)

pred <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars[x_test,]))
predall <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars))

#Test Predictions
final <- cbind(y_var[x_test], pred)
head(final)

#R-Square Computations
actual <- y_var
preds <- predall
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq


#Out of sample testing
d = y_test - pred
mse.out = mean((d)^2)
mae.out = mean(abs(d))
rmse.out = sqrt(mse.out)

cat(" MAE:", mae.out, "\n", "MSE:", mse.out, "\n", 
    "RMSE:", rmse.out, "\n")

#in sample testing
d = y_var[train] - predall[1:18]
mse.in = mean((d)^2)
mae.in = mean(abs(d))
rmse.in = sqrt(mse.in)

cat(" MAE:", mae.in, "\n", "MSE:", mse.in, "\n", 
    "RMSE:", rmse.in, "\n")

#prediction interval unscaled
X.X<-t(as.matrix(cbind(rep(1,18), x_vars[train,]))) %*% as.matrix(cbind(rep(1,18), x_vars[train,]))
x0<-cbind(1, as.matrix(x_vars[19,]))
x1<-cbind(1, as.matrix(x_vars[20,]))

#Plot the best model in scaled form (not in report)
#prediction interval calculation
lo.1<-pred[1,1]-2.16*sqrt(mse.in*(1+as.matrix(x0) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x0))))
lo.2<-pred[2,1]-2.16*sqrt(mse.in*(1+as.matrix(x1) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x1))))
hi.1<-pred[1,1]+2.16*sqrt(mse.in*(1+as.matrix(x0) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x0))))
hi.2<-pred[2,1]+2.16*sqrt(mse.in*(1+as.matrix(x1) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x1))))
lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)

#prediction interval plot
plot(dp$Year[1:18],dp$TransportGHG[1:18], xlim=c(2000,2020), ylim=c(1747.5,2106.217), xlab="Year", ylab="VMT")
lines(dp$Year[1:18],predall[1:18],  xlim=c(2000,2020), ylim=c(1747.5,2106.217), col="blue")
lines(dp$Year[19:20], dp$TransportGHG[19:20], xlim=c(2000,2020),ylim=c(1747.5,2106.217), type="p", pch=16)
lines(dp$Year[19:20], pred, xlim=c(2000,2020), ylim=c(1747.5,2106.217), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=lo.1, y1=hi.1, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=lo.2, y1=hi.2, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=lo.1, y1=lo.1, col="red")
segments(x0=2017.6,x1=2018.6, y0=hi.1, y1=hi.1, col="red")
segments(x0=2018.6,x1=2019.4, y0=lo.2, y1=lo.2, col="red")
segments(x0=2018.6,x1=2019.6, y0=hi.2, y1=hi.2, col="red")

#Future Forecast
x_vars_f = df[,c(4:8,9,18:22)]; #x_vars_f = x_vars_f[,-5]
pred_f <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[,c(1:5,6)]))

#Future Forecasts of the 5 Scenarios
pred_f1 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[,c(1:5,7)]))
pred_f2 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[,c(1:5,8)]))
pred_f3 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[,c(1:5,9)]))
pred_f4 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[,c(1:5,10)]))
pred_f5 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[,c(1:5,11)]))

#Figure 3-5 Plots of the Future Forecasts of the 5 Scenarios
plot(c(2000:2019), y_var, col='red', xlim=c(2000,2050), ylim=c(0,max(pred_f5)), 
     xlab ="", ylab = "Transport GHG");
#lines(c(2000:2019), predall,col='green');
#lines(c(2000:2019),y_var, col='blue');
lines(c(2023:2050),pred_f1[4:31], col='red');
lines(c(2023:2050),pred_f2[4:31], col='orange');
lines(c(2023:2050),pred_f3[4:31], col='yellow');
lines(c(2023:2050),pred_f4[4:31], col='cyan');
lines(c(2023:2050),pred_f5[4:31], col='green');
legend("bottomleft", 
       legend = c("Scenario A", "Scenario B", "Scenario C", "Scenairo D", "Scenario E"), 
       pch = 15,
       col = c('green', 'cyan', 'yellow', 'orange', 'red'))

#==================================================================#

### b. With EVs - EV Sales ####
x_vars = dp[,-c(1:5,6,11,14,15)];

lambda_seq=10^seq(5,-10,by=-.1)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_vars), 18)
train = 1:18
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(as.matrix(x_vars[train,]), y_var[train], alpha = 1, family="gaussian", lambda = lambda_seq, 
                       nfolds = 3)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, family="gaussian", lambda = best_lam)
coef(lasso_best)

pred <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars[x_test,]))
predall <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars))

#Test Predictions
final <- cbind(y_var[x_test], pred)
head(final)

#R-Square Computations
actual <- y_var
preds <- predall
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq


#Out of sample testing
d = y_test - pred
mse.out = mean((d)^2)
mae.out = mean(abs(d))
rmse.out = sqrt(mse.out)

cat(" MAE:", mae.out, "\n", "MSE:", mse.out, "\n", 
    "RMSE:", rmse.out, "\n")

#in sample testing
d = y_var[train] - predall[1:18]
mse.in = mean((d)^2)
mae.in = mean(abs(d))
rmse.in = sqrt(mse.in)

cat(" MAE:", mae.in, "\n", "MSE:", mse.in, "\n", 
    "RMSE:", rmse.in, "\n")

#prediction interval unscaled
X.X<-t(as.matrix(cbind(rep(1,18), x_vars[train,]))) %*% as.matrix(cbind(rep(1,18), x_vars[train,]))
x0<-cbind(1, as.matrix(x_vars[19,]))
x1<-cbind(1, as.matrix(x_vars[20,]))

#Plot the best model in scaled form (not in report)
#prediction interval calculation
lo.1<-pred[1,1]-2.16*sqrt(mse.in*(1+as.matrix(x0) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x0))))
lo.2<-pred[2,1]-2.16*sqrt(mse.in*(1+as.matrix(x1) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x1))))
hi.1<-pred[1,1]+2.16*sqrt(mse.in*(1+as.matrix(x0) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x0))))
hi.2<-pred[2,1]+2.16*sqrt(mse.in*(1+as.matrix(x1) %*% solve(X.X, tol=7.92698e-29) %*% t(as.matrix(x1))))
lo<-c(lo.1,lo.2); hi<-c(hi.1,hi.2)

#prediction interval plot
plot(dp$Year[1:18],dp$TransportGHG[1:18], xlim=c(2000,2020), ylim=c(1500.5,2106.217), xlab="Year", ylab="VMT")
lines(dp$Year[1:18],predall[1:18],  xlim=c(2000,2020), ylim=c(1500.5,2106.217), col="blue")
lines(dp$Year[19:20], dp$TransportGHG[19:20], xlim=c(2000,2020),ylim=c(1500.5,2106.217), type="p", pch=16)
lines(dp$Year[19:20], pred, xlim=c(2000,2020), ylim=c(1500.5,2106.217), type="p", pch=16, col="blue")
segments(x0=2018,x1=2018, y0=lo.1, y1=hi.1, lty = "dotted", col="red")
segments(x0=2019,x1=2019, y0=lo.2, y1=hi.2, lty = "dotted", col="red")
segments(x0=2017.6,x1=2018.4, y0=lo.1, y1=lo.1, col="red")
segments(x0=2017.6,x1=2018.6, y0=hi.1, y1=hi.1, col="red")
segments(x0=2018.6,x1=2019.4, y0=lo.2, y1=lo.2, col="red")
segments(x0=2018.6,x1=2019.6, y0=hi.2, y1=hi.2, col="red")



#==================================================================#

### c. W/O EVs ####
x_vars = dp[,-c(1:5,6,11,13,14,15)]; 

lambda_seq=10^seq(5,-10,by=-.1)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_vars), 18)
train = 1:18
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(as.matrix(x_vars[train,]), y_var[train], alpha = 1, family="gaussian", lambda = lambda_seq, 
                       nfolds = 3)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, family="gaussian", lambda = best_lam)
coef(lasso_best)


##### ELECTRIC VEHICLES #####

##### 1) VMT ###### 

#these results are not included in the report

#clean space
remove(list=ls())

# Loading the library
library(glmnet); library(dplyr)

#set working directory
setwd("C:/Users/zapate/Documents/task 6b year 1")

#load the data
dp<-read.csv("Data 20230509 - DataP.csv", header=TRUE)
df<-read.csv("Data 20230509 - DataF.csv", header=TRUE)
summary(dp)
summary(df)

# Variables: VMT and X's
y_var = dp[,2]; 

### a. With EVs - EV Station Locations ####
x_vars = dp[,-c(1:5,6,13,14,15)]; 

lambda_seq=10^seq(20,5,by=-.1)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_vars), 18)
train = 1:18
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(as.matrix(x_vars[train,]), y_var[train], alpha = 1, family="gaussian", lambda = lambda_seq, 
                       nfolds = 3)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, family="gaussian", lambda = best_lam)
coef(lasso_best)

pred <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars[x_test,]))
predall <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars))

#Test Predictions
final <- cbind(y_var[x_test], pred)
head(final)

#R-Square Computations
actual <- y_var
preds <- predall
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq

##### 2) GHG ######

#clean space
remove(list=ls())

# Loading the library
library(glmnet); library(dplyr)

#set working directory
setwd("C:/Users/zapate/Documents/task6_6_2/FHWA-Mobility-Trends/Year1/Policy Analysis")

#load the data
dp<-read.csv("Data 20230509 - DataP.csv", header=TRUE)
df<-read.csv("Data 20230509 - DataF.csv", header=TRUE)
summary(dp)
summary(df)

# Variables: GHG and X's
y_var = dp[,4]; 

### a. With EVs - EV Station Locations ####
x_vars = dp[,-c(1:5,6,13,14,15)]; 

lambda_seq=10^seq(5,-10,by=-.1)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_vars), 18)
train = 1:18
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(as.matrix(x_vars[train,]), y_var[train], alpha = 1, family="gaussian", lambda = lambda_seq, 
                       nfolds = 3)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, family="gaussian", lambda = best_lam)

# Table 4-4 coefficents
coef(lasso_best)

pred <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars[x_test,]))
predall <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars))

#Test Predictions
final <- cbind(y_var[x_test], pred)
head(final)

#R-Square Computations
actual <- y_var
preds <- predall
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq

#Future Forecast
x_vars_f = df[,c(4:8,9,13:17)]; #x_vars_f = x_vars_f[,-5]
pred_f <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[,c(1:5,6)]))

#Plots of the Future Forecasts of the 5 Scenarios
plot(c(2000:2019), y_var, col='red', xlim=c(2000,2050), ylim=c(0,max(pred_f)));
lines(c(2023:2050),pred_f[4:31], col='red');


### b. With EVs - EV Sales ####
x_vars = dp[,-c(1:5,6,11,14,15)]; 

lambda_seq=10^seq(5,-10,by=-.1)

# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(x_vars), 18)
train = 1:18
x_test = (-train)
y_test = y_var[x_test]

cv_output <- cv.glmnet(as.matrix(x_vars[train,]), y_var[train], alpha = 1, family="gaussian", lambda = lambda_seq, 
                       nfolds = 3)

# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam

# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, family="gaussian", lambda = best_lam)

# table 4-5 coefficents
coef(lasso_best)

pred <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars[x_test,]))
predall <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars))

#Test Predictions
final <- cbind(y_var[x_test], pred)
head(final)

#R-Square Computations
actual <- y_var
preds <- predall
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
rsq

#Future Forecast
x_vars_f = df[,c(4:7,9:10,13:17)]; #x_vars_f = x_vars_f[,-5]
pred_f <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[,c(1:5,6)]))

#Future Forecasts of the 5 Scenarios
pred_f1 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[,c(1:5,7)]))
pred_f2 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[,c(1:5,8)]))
pred_f3 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[,c(1:5,9)]))
pred_f4 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[,c(1:5,10)]))
pred_f5 <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f[,c(1:5,11)]))

#Plots of the Future Forecasts of the 5 Scenarios
plot(c(2000:2019), y_var, col='red', xlim=c(2000,2050), ylim=c(0,max(pred_f5)), 
     xlab = "", ylab = "GHG");
#lines(c(2000:2019), predall,col='green');
#lines(c(2000:2019),y_var, col='blue');
lines(c(2023:2050),pred_f1[4:31], col='red');
lines(c(2023:2050),pred_f2[4:31], col='orange');
lines(c(2023:2050),pred_f3[4:31], col='yellow');
lines(c(2023:2050),pred_f4[4:31], col='cyan');
lines(c(2023:2050),pred_f5[4:31], col='green')
legend("bottomleft", 
       legend = c("Scenario A", "Scenario B", "Scenario C", "Scenairo D", "Scenario E"), 
       pch = 15,
       col = c('green', 'cyan', 'yellow', 'orange', 'red'))
