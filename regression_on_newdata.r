library(psych)
library(corrplot)
library(ggplot2)
Data=read.csv("C:/Users/user/Downloads/asfactor.csv",stringsAsFactors = TRUE)

##Visualisation
data_visual=data[,c(-1,-2,-5,-6,-7)]
cor=round(cor(data_visual),2)

corrplot(cor,method="number")
###cor(Data$Price,Data$Seats,method = "pearson")
colnames(Data)
attach(Data)
qplot(data=Data,Price,Engine,col=Fuel_Type)
qplot(data=Data,Price,Engine,col=Power)
qplot(data=Data,Price,Engine,col=Transmission)
qplot(data=Data,Price,Name,col=Year)
table(Data$Name)

pairs.panels(data_visual)

d=as.numeric(data$Owner_Type)
train.size=0.75
train.data=Data[1:3845,]
valid.data=Data[3846:nrow(Data),]
colnames(Data)
fit=lm(Price~Engine*Power*Fuel_Type*Name*Year+Location+Transmission,data=train.data)
summary(fit)
fit2=lm(Price~Engine*Power*Fuel_Type*Transmission+Name*Year+Location,data=train.data)
summary(fit2)
fit3=lm(Price~Engine*Power*Fuel_Type*Transmission+Name*Year*Location,data=train.data)
summary(fit3)

#plot(fit)###subset(valid.data,select = c(Year,Kilometers_Driven,Fuel_Type,Transmission,Power))
train.data$pred_Price=predict(fit,newdata =train.data)
valid.data$pred_Price=predict(fit,newdata =valid.data)
summary(fit)
#View(train.data$pred_Price)
train.cor=round(cor(train.data$pred_Price,train.data$Price),2)
train.RMSE=round(sqrt(mean((train.data$Price-train.data$pred_Price)^2)))
train.MAE=round(mean(abs(train.data$Price-train.data$pred_Price)))
c(train.cor^2,train.RMSE,train.MAE)

valid.cor=round(cor(valid.data$pred_Price,valid.data$Price),2)
valid.RMSE=round(sqrt(mean((valid.data$Price-valid.data$pred_Price)^2)))
valid.MAE=round(mean(abs(valid.data$Price-valid.data$pred_Price)))
c(valid.cor^2,valid.RMSE,valid.MAE)

