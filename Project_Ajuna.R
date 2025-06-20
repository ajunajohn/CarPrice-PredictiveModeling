install.packages("corrplot")
install.packages("car")
install.packages("zoo")
library(car)
library(corrplot)
library(readr)
library(onewaytests)  
library(lmtest)
library(MASS)
library(leaps)
library(caret)
library(tidyverse)

#Load Data
CarPriceData<-read.csv("D:/Ajuna/OneDrive - purdue.edu/01. Spring 2024/STAT 51200/ProjectGroup/Project_Ajuna/CarPrice.csv", header = TRUE, sep=",")
#Initial Regression model
price.Fullmodel1 =lm(price~ carlength+carwidth+carheight
                    +wheelbase,data = CarPriceData)
summary(price.Fullmodel1)
anova(price.Fullmodel1)

price.Reduced = lm(price~ 1, data = CarPriceData)
anova(price.Reduced)
#check for significance of variables by comparing it to full model
anova(price.Reduced,price.Fullmodel1 )

## SLR Car Price against X
#In this section, each explanatory variable is tested for correlation against 
#the response variable (price). Further an SLR model is fit to test linear 
#impact of each independent variable. Variable carheight you would expect to be
#significant seems to be not.

#price vs carlength
cor(CarPriceData$price, CarPriceData$carlength, method = "pearson")
price.mod =lm(price~carlength, data = CarPriceData)
summary(price.mod)

#price vs carheight
cor(CarPriceData$price, CarPriceData$carheight, method = "pearson")
price.mod =lm(price~carheight, data = CarPriceData)
summary(price.mod)

#price vs carwidth
cor(CarPriceData$price, CarPriceData$carwidth, method = "pearson")
price.mod =lm(price~carwidth, data = CarPriceData)
summary(price.mod)

#price vs wheelbase 
cor(CarPriceData$price, CarPriceData$wheelbase,method = "pearson")
price.mod =lm(price~wheelbase, data = CarPriceData)
summary(price.mod)


# # # MLR Diagnostics # # #

# # # Influentiual Pts and Outliers # # #

#check for outliers on Y
rStudentVals <- abs(rstudent(price.Fullmodel1))
#residuals <- rstandard(price.Fullmodel1)
alpha <- 0.05
# Calculate Bonferroni critical value
Bonferroni_critical_value <- qt(1 - alpha / (2 * 205), df = 205 - 1 - 5)
Bonferroni_critical_value
# Identify outliers based on Bonferroni critical value
outliers <- abs(rStudentVals) > Bonferroni_critical_value
# Print identified outliers
print(rStudentVals[outliers])


#check for outliers on X
residuals=lm.influence(price.Fullmodel1)$hat
threshold=2*(5/205)
threshold
outlier=abs(residuals)>threshold
print(residuals[outlier])


#DFFITS
thresh1<-2*sqrt(5/205)
thresh1
influentialPoints=dffits(price.Fullmodel1)>thresh1
print(dffits(price.Fullmodel1)[influentialPoints])

#CooksD
cooksVals <- cooks.distance(price.Fullmodel1)
max(cooksVals)
# compute the critical F values to compare against cooksD 
thresh1=qf(.2,5,200)
thresh2=qf(.5,5,200)
thresh2
influentialPoints=abs(cooksVals)>thresh1
print(cooksVals[influentialPoints])

#DFBETAS
thresh3<-2/sqrt(205)
thresh3
influentialPoints<-dfbetas(price.Fullmodel1) >= thresh3
print(dfbetas(price.Fullmodel1)[influentialPoints])
dfbetasPlots(price.Fullmodel1)
plot(price.Fullmodel1, pch=18, col="green", which = c(4))

#Diagnostic with the influence plot
influencePlot(lm(price ~ carlength + carwidth + carheight + wheelbase, data = CarPriceData))
threshold <- qt(1 - 0.05 / (2*n), n - 1 - p)
print(threshold)
df <- data.frame(
  StudRes = c(1.7497113, 4.5285282, 5.2463282, 0.4445345),
  Hat = c(0.08531066, 0.03595258, 0.03595258, 0.15829465),
  CookD = c(0.056524784, 0.139365824, 0.181254333, 0.007462648)
)
studres_outliers <- which(df$StudRes > threshold)
hbar <- (2 * p) / n
hat_outliers <- which(df$Hat > hbar)
print(studres_outliers)
print(hat_outliers)
D <- qf(1 - 0.05, p, n-p)
cook_influential <- which(df$CookD > D)
print(cook_influential)


#Residuals against fitted values:
res<- residuals(price.Fullmodel1)
plot(fitted(price.Fullmodel1), res)
abline(0,0)
residual_sd <- sd(resid(price.Fullmodel1))
upper_bound <- 2 * residual_sd
lower_bound <- -2 * residual_sd
abline(h = upper_bound, col="red", linetype = "dashed")
abline(h = lower_bound, col="red", linetype = "dashed")


#Linear trend
#scatter plot
plot(CarPriceData[c(10,11,12,13,26)])
#added variable plots to check the effect of each variable
avPlots(price.Fullmodel1)
#Carwidth shows a positive linear trend. wheelbase doesn't show added-on effect.
#All the 4 variables seem to have non-constant variance as points are not scattered 
#evenly across 0, instead are concentrated in certain areas.There is increased 
#correlation, particularly between wheelbase, carlength, and carwidth. 
#This means that all the Xs could benefit from transforming X.

#Multicollinearity - VIF
corMatrix=cor(CarPriceData[c(10,11,12,13,26)])
corMatrix
corrplot(corMatrix)
#Given the correlations observed, there is some evidence of multicollinearity, 
#particularly between wheelbase, carlength, and carwidth.
vifFM <- vif(price.Fullmodel1)
vifFM
#As per the result from vif method,multicollinearity is not severe enough to 
#substantially affect the interpretation or stability of the regression coefficients.

#Constant variance

#Constant variance - Breusch Pagan Test (BP test)
bptest(price.Fullmodel1)


#Normality test
shapiro.test(residuals(price.Fullmodel1))
qqnorm(residuals(price.Fullmodel1))
qqline(residuals(price.Fullmodel1))
#not normally distributed as per Shapiro test


# # # Remedial Measure - Box Cox # # #

#Box-cox to correct non-constant variance / normality issue

bcmle <- boxcox(price.Fullmodel1, lambda = seq(-3,3, by=0.1))
lambda <- bcmle$x[which.max(bcmle$y)]
lambda
CarPriceData$price_transformed <- (CarPriceData$price^lambda - 1) / lambda
proj_model_bc = lm(formula = price_transformed ~ carlength+carwidth+carheight+wheelbase, data=CarPriceData)
summary(proj_model_bc)
anova(proj_model_bc)
#transformation Constant variance - Breusch Pagan Test (BP test)
bptest(proj_model_bc)

#transformation Normality test
shapiro.test(residuals(proj_model_bc))
qqnorm(residuals(proj_model_bc))
qqline(residuals(proj_model_bc))

# # # Influentiual Pts and Outliers after transformation # # #

#check for outliers on Y
rStudentVals <- abs(rstudent(proj_model_bc))
#residuals <- rstandard(price.Fullmodel1)
alpha <- 0.05
# Calculate Bonferroni critical value
Bonferroni_critical_value <- qt(1 - alpha / (2 * 205), df = 205 - 1 - 5)
Bonferroni_critical_value
# Identify outliers based on Bonferroni critical value
outliers <- abs(rStudentVals) > Bonferroni_critical_value
# Print identified outliers
print(rStudentVals[outliers])


#check for outliers on X
residuals=lm.influence(proj_model_bc)$hat
threshold=2*(5/205)
threshold
outlier=abs(residuals)>threshold
print(residuals[outlier])


#DFFITS
thresh1<-2*sqrt(5/205)
thresh1
influentialPoints=dffits(proj_model_bc)>thresh1
print(dffits(proj_model_bc)[influentialPoints])

#CooksD
cooksVals <- cooks.distance(proj_model_bc)
max(cooksVals)
# compute the critical F values to compare against cooksD 
thresh1=qf(.2,5,200)
thresh2=qf(.5,5,200)
thresh2
influentialPoints=abs(cooksVals)>thresh1
print(cooksVals[influentialPoints])

#DFBETAS
thresh3<-2/sqrt(205)
thresh3
influentialPoints<-dfbetas(proj_model_bc) >= thresh3
print(dfbetas(proj_model_bc)[influentialPoints])
dfbetasPlots(proj_model_bc)
plot(proj_model_bc, pch=18, col="green", which = c(4))

#Diagnostic with the influence plot
influencePlot(lm(price_transformed ~ carlength + carwidth + carheight + wheelbase, data = CarPriceData))
threshold <- qt(1 - 0.05 / (2*n), n - 1 - p)
print(threshold)
df <- data.frame(
  StudRes = c(1.7497113, 4.5285282, 5.2463282, 0.4445345),
  Hat = c(0.08531066, 0.03595258, 0.03595258, 0.15829465),
  CookD = c(0.056524784, 0.139365824, 0.181254333, 0.007462648)
)
studres_outliers <- which(df$StudRes > threshold)
hbar <- (2 * p) / n
hat_outliers <- which(df$Hat > hbar)
print(studres_outliers)
print(hat_outliers)
D <- qf(1 - 0.05, p, n-p)
cook_influential <- which(df$CookD > D)
print(cook_influential)

vif(proj_model_bc)
# # # Alternate Regressions # # #
#Robust Regression

Price.rob = rlm(price_transformed ~ carlength+carwidth+carheight+wheelbase, data=CarPriceData, psi = psi.bisquare)
summary(Price.rob)
anova(Price.rob)

#transformation Normality test
shapiro.test(residuals(Price.rob))
qqnorm(residuals(Price.rob))
qqline(residuals(Price.rob))

#transformation Constant variance - Breusch Pagan Test (BP test)
bptest(Price.rob)

#Residuals against fitted values:
res_bc_rob<- resid(Price.rob)
plot(fitted(Price.rob), res_bc_rob)
abline(0,0)
residual_sd <- sd(resid(Price.rob))
upper_bound <- 2 * residual_sd
lower_bound <- -2 * residual_sd
abline(h = upper_bound, col="red", linetype = "dashed")
abline(h = lower_bound, col="red", linetype = "dashed")

#transformation added variable plots to check the effect of each variable
avPlots(Price.rob)
predictor_variables <- CarPriceData[c("carlength", "carwidth", "carheight", "wheelbase")]
correlation_matrix <- cor(predictor_variables)
print(correlation_matrix)
vif <- vif(lm(price_transformed ~ carlength  + carwidth + carheight + wheelbase, data = CarPriceData))
vif

#Bootstrap on Robust
library(boot)
boot.robustCoef <- function(data, indices, maxit=100) {
  data <- data[indices,]
  data.mod<-lm(price_transformed ~ carlength + carwidth + carheight+wheelbase, data=data, psi = psi.bisquare)
  return(coef(data.mod))
}

# Bootstrap the data
transformedModel.boot <- boot(data = CarPriceData, statistic = boot.robustCoef, R = 100, maxit = 100)
transformedModel.boot

# 95% confidence intervals
boot.ci(transformedModel.boot, type="perc", index=1)
boot.ci(transformedModel.boot, type="perc", index=2)
boot.ci(transformedModel.boot, type="perc", index=3)
boot.ci(transformedModel.boot, type="perc", index=4)
boot.ci(transformedModel.boot, type="perc", index=5)

c((1/1.543)^(1/lambda), (1/1.551)^(1/lambda))
c((1/0.0001)^(1/lambda), (1/0.0001)^(1/lambda))
c((1/0.0002)^(1/lambda), (1/0.0003)^(1/lambda))
-1*c((1/0.0001)^(1/lambda), (1/0.0000)^(1/lambda))
-1*c((1/0.0001)^(1/lambda), (1/0.0000)^(1/lambda))


#Checked for marginal impacts and effects did stepwise regression model 
#to check which variable is recommended to be retained in the model
#Model Selection Algorithm "Best Subset" with non normal data 
library(ALSM)
colnames(CarPriceData)
X <- c(11, 12, 13, 10)
PredCols <- CarPriceData[, X]
bs <- BestSub(PredCols, CarPriceData$price_transformed, num=1)
bs
step(proj_model_bc, method="both", trace=1)
proj <- CarPriceData

#step(price.Fullmodel1, method="both", trace=1)
plotmodel.s(CarPriceData[, X],CarPriceData$price_transformed)

#K-fold cross validation
#p=4
set.seed(123)
train.control<-trainControl(method="cv", number=10)
step.model <- train(price_transformed ~ carlength  + carwidth + wheelbase,
                    data=CarPriceData,method="leapBackward",tuneGrid=
                    data.frame(nvmax=5),trControl=train.control)
step.model$results
#p=3
set.seed(123)
train.control<-trainControl(method="cv", number=10)
step.model <- train(price_transformed ~ carlength  + carwidth ,
                    data=CarPriceData,method="leapBackward",tuneGrid=
                      data.frame(nvmax=5),trControl=train.control)
step.model$results
#p=5
set.seed(123)
train.control<-trainControl(method="cv", number=10)
step.model <- train(price_transformed ~ carlength  + carwidth + carheight+wheelbase,
                    data=CarPriceData,method="leapBackward",tuneGrid=
                      data.frame(nvmax=5),trControl=train.control)
step.model$results


# Anova Bootstrap 
anova_test <- boot(data = transformedModel.boot$t, statistic = function(data, indices) {
  model_reduced<- lm(price_transformed ~ 1, data = CarPriceData[indices, ])
model_full <- lm(price_transformed ~ carlength + carwidth + carheight+ wheelbase, data = CarPriceData[indices, ])
  return(anova(model_reduced, model_full)$"Pr(>F)"[2])
}, R = 100)
print(anova_test)
#Bootstrapped GLT for individual p-values
glt <- function(full_model, reduced_model) {
  glt_result <- anova(full_model, reduced_model)
  glt_result_p_value <- glt_result$"Pr(>F)"[2]
  return(glt_result_p_value)
}
full_model <- lm(price_transformed ~ carlength + carwidth + carheight + wheelbase, data = CarPriceData)
reduced_models <- lapply(1:4, function(i) {
  formula <- as.formula(paste("price_transformed ~", paste(c("carlength", "carwidth", "carheight", "wheelbase")[-i], collapse = "+")))
  lm(formula, data = CarPriceData)
})
glt_result_p_values <- sapply(reduced_models, function(reduced_model) glt(full_model, reduced_model))
print(glt_result_p_values)


