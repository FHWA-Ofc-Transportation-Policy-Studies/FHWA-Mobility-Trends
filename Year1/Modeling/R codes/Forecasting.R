# Loading the library
library(glmnet)

#set working directory
setwd("C:/Users/tawfik/Desktop/fhwa/Task 4")

#clean space
remove(list=ls())

#load the data
dp<-read.csv("Data 20230307 - DataP.csv", header=TRUE)
df<-read.csv("Data 20230307 - DataF.csv", header=TRUE)
summary(dp)
summary(df)
y_var = dp[,2]
x_vars = dp[,-c(1:5)]; #x_vars=x_vars[,-5]

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

#Future Forecast
x_vars_f = df[,c(3,4,5:7,8,9)]; #x_vars_f = x_vars_f[,-5]
pred_f <- predict(lasso_best, s = best_lam, newx = as.matrix(x_vars_f))


#GLM
 myglm = glm(y_var[train] ~ GDP + Unemployment.Rate.1 + TransitPMT + 
         EVPrivateandPublicChargingStationLocations + 
         X.populationTelework, family = gaussian, data=x_vars[train,]); summary(myglm)

# myglm = glm(y_var[train] ~ Population + GDP + Unemployment.Rate.1 + TotLaneMiles + TransitPMT + 
#         EVPrivateandPublicChargingStationLocations + 
#         X.populationTelework, family = gaussian, data=x_vars[train,])
# summary(myglm)

#LM
mylm = lm(y_var[train] ~ GDP + Unemployment.Rate.1 + TransitPMT + 
            EVPrivateandPublicChargingStationLocations + 
            X.populationTelework, family = gaussian, data=x_vars[train,])

summary(mylm)

mylm = lm(y_var[train] ~ GDP + Unemployment.Rate.1 + TransitPMT + 
              EVPrivateandPublicChargingStationLocations + 
            X.populationTelework, family = gaussian, data=x_vars[train,])

summary(mylm)
