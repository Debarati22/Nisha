rm(list=ls())
set.seed(seed=123456789)

#Importing Data 
library(readxl)
SeoulBikeData <- read_excel("C:/Users/DEBARATI/Downloads/SeoulBikeData.xlsx")
View(SeoulBikeData)
df=data.frame(SeoulBikeData)

#Exploratory Data Analysis
library(dplyr)
head(df)
glimpse(df)
length(df$Hour)
dim(df) #8760 observations and 14 variables
str(df)# 10 variables are numeric, 3 variables are categorical and 1 is date type.  
sum(duplicated(df))


#omitting the non-numeric variables 
data=df[-c(1,12:14)]
col=colnames(data)
attach(data)
summary(data)
head(data)

#defining predictors and response. Rented Bike Count is response variable and Hour, Temperature, Humidity, Wind speed, Visibility, Dew.point.temperature, solar radiation, rainfall, snowfall are predtictors
y=data$Rented.Bike.Count
x=data[,2:10]

#scatterplot of response vs each predictors
par(mfrow=c(4,3))
for(i in 1:9)
{
  plot(data[,(i+1)],y,xlab=col[i+1],ylab=col[1],col="black")
}

#scatterplot matrix between each variable
pairs(data[,1:10])

#checking for correlation between the variables
cormatrix=cor(data)
library(corrplot)
corrplot(cormatrix,method='number')#very high correlation between dew point temperature and temperature. 

#boxplot 
par(mfrow=c(4,3))
for(i in 1:10)
{
  boxplot(data[i],ylab=col[i]) 
}
#huge number of outliers are present in the response variable,Wind speed, Solar radiation, Rainfall, Snowfall

#histogram
par(mfrow=c(4,3))
for(i in 1:10)
{
  hist(data[,i],freq=FALSE,xlab=col[i])
  dx=density(data[,i])
  lines(dx,lwd=2,col="blue")
}
#none of the variable seem to have normal distribution 

#check for whether there is any missing value
library(Matrix)
x=is.na(data)
colSums(x) #no missing value

#fitting a linear regression without any scaling and manipulation 
fit=lm(Rented.Bike.Count~.,data=data)
summary(fit)#adjusted r2=0.47


#split data into training and testing set
library(caret)

set.seed(seed=1234)

#use 80% datast for training and 20% data set for testing
train.sample=createDataPartition(data$Rented.Bike.Count,p=0.8,list=FALSE)
train_data=data[train.sample,]
test_data=data[-train.sample,]

#scaling the train data
n1=length(train_data$Rented.Bike.Count)
data1_1=data.frame(sqrt(1/(n1-1))*scale(train_data,center=TRUE,scale=TRUE))
dim(data1_1)#7009 observations and 10 variables
y_train=data1_1$Rented.Bike.Count
x_train=data1_1[,-c(1)]

#scaling the test data
n2=length(test_data$Rented.Bike.Count)
data1_2=data.frame(sqrt(1/(n2-1))*scale(test_data,center=TRUE,scale=TRUE))
dim(data1_2)#1751 observations 
y_test=data1_2$Rented.Bike.Count
x_test=data1_2[,-c(1)]


#fitting regression to scaled train data
fit1_1=lm(y_train~.-1,data=x_train)   
summary(fit1_1) #adjusted r2=0.4718

#model performances on the basis  of test data
library(Metrics)
y_hat=predict(fit1_1,data1_2)
rmse(y_test,y_hat) #RMSE=0.01749787
length(y_hat)


library("olsrr")



#to check multicollinearity
library(car)
library(Matrix)
X=as.matrix(data1_1[,2:10])#matrix of explanatory variables
det(t(X)%*%X) #determinant of X'X = 0.00202548
vif(fit1_1) #VIF of Dew point temperature= 115.181883, VIF of Temperature=86.538007, VIF of humidity=20.382120, which indicate presence of multicollinearity


########checking for outlier,leverage and influence points######
library("olsrr")
ols_plot_cooksd_chart(fit1_1)
ols_plot_resid_stand(fit1_1)
ols_plot_resid_stud(fit1_1)

lev1=ols_plot_resid_lev(fit1_1)

#outliers
which(lev1$data$color=="outlier")
length(which(lev1$data$color=="outlier")) #number of outliers 383 

#leverage points
which(lev1$data$color=="leverage")
length(which(lev1$data$color=="leverage")) #number of leverage 275

#outlier and leverage
length(which(lev1$data$color=="outlier & leverage")) #number of obs which are both outlier and leverage 9

#influence points 
#plotting cook's distance
lev2=ols_plot_cooksd_bar(fit1_1)
length(which(lev2$data$color=="outlier")) #number of outliers 257

#plotting DFBETA and DFFITS staistics
lev3=ols_plot_dfbetas(fit1_1)
lev4=ols_plot_dffits(fit1_1)
length(which(lev4$data$color=="outlier")) # number of outliers 257

##############################################
#variable reduction 
#stepwise regression

#Forward,Backwrd,step_wise regression based on AIC
f1=ols_step_forward_aic(fit1_1,details=TRUE) #forward regression
pred1=predict(f1$model,data1_2)
rmse(y_test,pred1)#rmse=0.01750209


f2=ols_step_backward_aic(fit1_1,details=TRUE)#backward regression 
pred2=predict(f2$model,data1_2)
rmse(data1_2$Rented.Bike.Count,pred2)#rmse=0.01749787


f3=ols_step_both_aic(fit1_1,details=TRUE)#bothside regression
pred3=predict(f3$model,data1_2)
rmse(data1_2$Rented.Bike.Count,pred3)

#AIC is lowest when all the predictors are used

par(mfrow=c(2,2))
plot(f1)
plot(f2)
plot(f3)


library("leaps")
model=regsubsets(Rented.Bike.Count~.,data=data1_1,nvmax=10)
summary(model)#to check which variable to be selected in the model
k=ols_step_best_subset(fit1_1)
plot(k)


#dropping Wind speed and Dew temperature and fitting model to rest data 

data2_1=data.frame(data1_1[,-c(5,7)])#train data
head(data2_1)

data2_2=data.frame(data1_2[,-c(5,7)])#test data
head(data2_2)


fit2_1=lm(Rented.Bike.Count~.,data=data2_1)
summary(fit2_1)#adj r2=0.4674

#model performances
y_hat=predict(fit2_1,data2_2)
rmse(data2_2$Rented.Bike.Count,y_hat) #rmse=0.01750209

X1=as.matrix(data2_1[,2:7])#matrix of explanatory variables
det(t(X1)%*%X1) #determenent of X'X = 0.3243043

vif(fit2_1)#all vif <5 , hence multicollinearity is removed. 
corrplot(cor(data2_1),method='number')

Hence the variables in the final data Rented Bike Count, Hour, Temperature, Humidity, Visibility, Solar Radiation, Rainfall, Snowfall..cm.

#check for assumptions for the latest fitted regression

#check for normality

#diagramatically
library(olsrr)
plot(fit2_1)
ols_plot_resid_qq(fit2_1)#normality assumption is violated

#normality test
#shapiro-wilk test cannot be performed since number of observations is greater than 5000
library(nortest)
ks.test(resid(fit2_1),"pnorm")  #p value <0.05, hence residuals are not normally distributed
lillie.test(resid(fit2_1)) #p-value is less than 0.05. hence normality assumption is violated

#to check for homoscedasticity
plot(fit2_1,1,lwd=3)
library(skedastic)
glejser(mainlm=fit1_1,sigma="main",statonly=FALSE) #since p-value <0.05, heteroscedasticity is present

#to check for autocorrelation (durbin-watson test is conducted)
acf(fit2_1$residuals,type="correlation")
durbinWatsonTest(fit2_1) #D=0.7273455 < 2  hence autocorrelation is present and residuals follow AR(1) process.


#checking for outlier,leverage and influence points
ols_plot_resid_stud(fit2_1)
ols_plot_resid_stand(fit2_1)

lev5=ols_plot_resid_lev(fit2_1)
length(which(lev5$data$color=="outlier")) #number of outliers 386

#leverage points
which(lev5$data$color=="leverage")
length(which(lev5$data$color=="leverage")) #number of leverage 213

#outlier and leverage
length(which(lev5$data$color=="outlier & leverage")) #number of obs which are both outlier and leverage 4

#influence points 
#plotting cook's distance
lev6=ols_plot_cooksd_bar(fit2_1)
length(which(lev6$data$color=="outlier")) #number of outliers 267

#plotting DFBETA and DFFITS staistics
lev7=ols_plot_dfbetas(fit2_1)


#hich(lev5$data$color=="outlier")

lev8=ols_plot_dffits(fit2_1)
length(which(lev8$data$color=="outlier")) # number of outliers 267


################# Ridge Regression ######################################

#since the data has multicollinearity, we can use ridge regression
#Ridge regression analysis

#load packages
library("data.table")
library("dplyr")
library("glmnet")
library("ggplot2")
library("caret")

y1=data1_1$Rented.Bike.Count
x1=data.matrix(data1_1[,c(2:9)])


y2=data1_2$Rented.Bike.Count
x2=data.matrix(data1_2[,c(2:9)])


#fit ridge regression model
lambda=10^seq(2,-2,by=-.1)
model=glmnet(x1,y1,alpha=0,standardize = FALSE,standardize.response = FALSE,lambda=lambda)
model
summary(model)

#perform k-fold cross-validation to find 
cv_model=cv.glmnet(x1,y1,alpha=0,standardize=FALSE,standardize.response=FALSE,lambda=lambda)
cv_model

#find optimal lambda value that minimizes test MSE
best_lambda=cv_model$lambda.min

#plot of test MSE by lambda value
plot(cv_model)

#fit the best model
coef(best_model)

#produce ridge trace plot
plot(model,xvar="lambda")

#computing RMSE
best_model=glmnet(x1,y1,alpha=0,lambda=0.01)
summary(best_model) 
y_hat=predict(best_model,s=best_lambda,newx=x2)
rmse(y2,y_hat) #rmse=0.01865118


################### Principal Component regression #############################

#principal component regression

#load packages
library("pls")


#fit PCR model
pc_model=pcr(y1~x1,scale=FALSE,validation="CV")
summary(pc_model) 

#visualize cross-validation plot
validationplot(pc_model)
validationplot(pc_model,val.type="R2")

#use model to make predictions on a test set
pc_model1=pcr(y2~x2,data=data1_2,scale=FALSE,validation="CV")
summary(pc_model1)

#computing RMSE
y_hat1=predict(pc_model1,data=data1_2$Rented.Bike.Count)
rmse(data1_2$Rented.Bike.Count,y_hat1) #rmse= 0.01897104


###########################################################



plot(data[,2],y_log)
abline(lm(log(data$Rented.Bike.Count)~data[,2],data=data),col="red")



##remove heteroscedasticity

#we can use weighted least square regression instead of ordinary least square regression

wt=1/lm(abs(fit2_1$residuals)~fit2_1$fitted.values)$fitted.values^2 
wls_model1=lm(Rented.Bike.Count~.,data=data2_1,weights=wt)
summary(wls_model1) #adjusted r_square= 0.3576< than that of OLS method. hence wls does not give better model


#model performance
y_hat=predict(wls_model1,data2_2[,-c(1)]) 
rmse(data2_2$Rented.Bike.Count,y_hat) #rmse=0.01800191

#robust regression since huge number of outliers,influential points are present and for normality
library(MASS)
m=rlm(y1~.,data=data1_1)
summary(m) #rse is smaller than that of OLS method
y_hat=predict(m,data1_2)
rmse(y2,y_hat)# 0.6299> that of ols

#removing autocorrelation 
library(orcutt)
auto=lm(Rented.Bike.Count~.,data=data2_1)
coch=cochrane.orcutt(auto)
summary(coch)
x_red_test=data.frame(data2_2[,-c(1)])
pred=predict(coch,x_red_test)
rmse(data2_2$Rented.Bike.Count,pred)#0.0248
