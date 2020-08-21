library(stringr)
library(dummies)
library(openxlsx)
data=read.xlsx("C:/Users/user/Downloads/UsedCarsData.xlsx")

data$Name=word(data$Name,sep =" ")
data$Power=as.numeric(gsub(" bhp","",data$Power))
data$Mileage=as.numeric(gsub(" kmpl","",data$Mileage))
data$Engine=as.numeric(gsub(" CC","",data$Engine))

table(is.na(data$Mileage))
table(is.na(data$Engine))
table(is.na(data$Power))
table(is.na(data$Seats))
table(is.na(data$New_Price))


data=data[-which(is.na(data$Mileage)),]
data=data[-which(is.na(data$Engine)),]
data=data[-which(is.na(data$Power)),]
data=data[-which(is.na(data$Seats)),]
data=data[,-12]
table(is.na(data))


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
data_out=data
data_out$Year=remove_outliers(data_out$Year)
boxplot(data_out$Year)

data_out=data_out[complete.cases(data_out$Year),]
#KM Driven

data_out$Kilometers_Driven=remove_outliers(data_out$Kilometers_Driven)
boxplot(data_out$Kilometers_Driven)
data_out=data_out[complete.cases(data_out$Kilometers_Driven),]

#Mileage
data_out$Mileage=remove_outliers(data_out$Mileage)
data_out=data_out[complete.cases(data_out$Mileage),]
boxplot(data_out$Mileage)

#Engine
data_out$Engine=remove_outliers(data_out$Engine)
data_out=data_out[complete.cases(data_out$Engine),]
boxplot(data_out$Engine)

#power
data_out$Power=remove_outliers(data_out$Power)
data_out=data_out[complete.cases(data_out$Power),]
boxplot(data_out$Power)

data=data_out

data$Name=as.factor(data$Name)
data$Location=as.factor((data$Location))
data$Fuel_Type=as.factor((data$Fuel_Type))
data$Transmission=as.factor((data$Transmission))

nrow(data)
table(data$Name)
################
for(i in 1:nrow(data))
{
  if(isTRUE(data$Name[i]=="Datsun"|| data$Name[i]=="Ambassador" || data$Name[i]=="Bentley"|| data$Name[i]=="Lamborghini" || data$Name[i]=="Porsche" || data$Name[i]=="Fiat" || data$Name[i]=="Force" || data$Name[i]=="Isuzu" ||data$Name[i]=="ISUZU" || data$Name[i]=="Volvo"  || data$Name[i]=="Jeep" || data$Name[i]=="Mitsubishi"))
  {
    data=data[-i,]
  }
  
}
for(i in 1:nrow(data))
{
  data$Engine[i]=(data$Engine[i]-min(data$Engine))/(max(data$Engine)-min(data$Engine))
  data$Power[i]=(data$Power[i]-min(data$Power))/(max(data$Power)-min(data$Power))
  data$Price[i]=(data$Price[i]-min(data$Price))/(max(data$Price)-min(data$Price))
  
}
table(data1$Name)

plot(data$Year,data$Price)
plot(data$Kilometers_Driven,data$Price)
plot(data$Owner_Type,data$Price)
plot(data$Mileage,data$Price)
plot(data$Engine,data$Price)
plot(data$Power,data$Price)
write.csv(data,file = "C:/Users/user/Downloads/asfactor.csv",row.names = F)
