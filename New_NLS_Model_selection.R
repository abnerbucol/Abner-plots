#NEW model selection
#NLS
#ALL
library(car)
library(Hmisc)
getwd()
setwd("C:/Users/admin/Documents/R4/NEW/RECALC_BFUA")
df <- read.csv("bleekeri_BFUA_MR.csv")
df <- read.csv("bleekeri_BFUA_Fished.csv")
str(df)
x<-c(df$DOP)
y<-c(df$reserve)

y<-c(df$fished)
#plot data
plot(y~x, pch=19, xlim=c(0,12), axes=TRUE, ylim=c(0,1000), xlab=" ", ylab= " ")
plot(y~x, pch=21, xlim=c(0,12), axes=TRUE, ylim=c(0,1000), xlab=" ", ylab= " ")
#Exponential model
mod1 <- nls(y ~ exp(a + b * x), data = df, start = list(a = 0, b = 1))
summary(mod1)
AIC(mod1)
#R-squared (goodness of fit)
(RSS.e <- sum(residuals(mod1)^2))
(TSS <- sum((y - mean(y))^2))
1 - (RSS.e/TSS)

#LINEAR:
lm.model<-lm(y~x, df)
summary(lm.model)
AIC(lm.model)
library(car)


#logistic model parameters
coef(lm(logit(y/100)~x, data=df))
#insert initial model parameters (phi2=intercept; phi3=x coef)
mod1<- nls(y ~ phi1/(1+exp(-(phi2+phi3*x))),start=list(phi1=100,phi2=-5.0412347     ,phi3=0.5730846 ), data=df, trace=TRUE)
summary(mod1)
AIC(mod1)

x<-c(data1$DOP)
y<-c(data1$reserve)
y<-c(data1$fished)
#bfua
plot(y~x, pch=19, xlim=c(0,12), axes=TRUE, ylim=c(0,400), xlab=" ", ylab= " ")
plot(y~x, pch=21, xlim=c(0,12), axes=TRUE, ylim=c(0,400), xlab=" ", ylab= " ")
#plot data
plot(y~x, pch=19, xlim=c(0,12), axes=TRUE, ylim=c(0,8), xlab=" ", ylab= " ")
plot(y~x, pch=21, xlim=c(0,12), axes=TRUE, ylim=c(0,8), xlab=" ", ylab= " ")

#Exponential model
mod1 <- nls(y ~ exp(a + b * x), data = data1, start = list(a = 0, b = 0.2))
summary(mod1)
AIC(mod1)
x<-seq(0,11)
mod2 <-(predict(mod1, list(x=x)))
lines(mod2~x, lwd=2, lty=1, col="black")
lines(mod2~x, lwd=2, lty=2, col="black")
#SE_MR
par(new=T)
plot(data1$DOP, data1$reserve, xlim=c(0,12), axes=TRUE, ylim=c(0,400), type="n", xlab="", ylab="")
with (
  data = data1
  , expr = errbar(data1$DOP,data1$reserve, data1$reserve+se, data1$reserve-se, add=T, pch=19, cap=.01)
)

par(new=T)
#SE_Fished
plot(data1$DOP, data1$fished, xlim=c(0,12), axes=TRUE, ylim=c(0,400), type="n", xlab="", ylab="")
with (
  data = data1
  , expr = errbar(data1$DOP,data1$fished, data1$fished+se, data1$fished-se, add=T, pch=21, cap=.01)
)

#R-squared (goodness of fit)
(RSS.e <- sum(residuals(mod1)^2))
(TSS <- sum((y - mean(y))^2))
1 - (RSS.e/TSS)
#AIC = 241.2257#
#R2 = 0.9896685#

#LINEAR:
lm.model<-lm(y~x, data1)
summary(lm.model)
AIC(lm.model)
x<-seq(1,5)
lm.model2 <-(predict(lm.model, list(x=x)))
lines(lm.model2~x, lty=1, lwd=2, col="black")
lines(lm.model2~x, lty=2, lwd=2, col="black")

238.1519 #AIC
(RSS <- sum(residuals(lm.model)^2))
(TSS <- sum((y - mean(y))^2))
1 - (RSS/TSS)
a = -462.8754
b = 3.6046


#Logistic model:
data1<-read.csv(file.choose(), header=TRUE)
str(data1)
x<-c(data1$DOP)
y<-c(data1$MR)
plot(y~x, pch=19, axes=TRUE, col="black", xlab="DOP (years)", ylab="total biomass (kg/500m2)")

#logistic model parameters
coef(lm(logit(y/1.2)~x, data=data1))
#insert initial model parameters (phi2=intercept; phi3=x coef)
mod1<- nls(y ~ phi1/(1+exp(-(phi2+phi3*x))),start=list(phi1=1.2,phi2=-1.153347,phi3=0.119055), data=data1, trace=TRUE)
summary(mod1)
AIC(mod1)

x<-seq(0,20)
mod2 <-(predict(mod1, list(x=x)))
lines(mod2~x, lwd=3, col="red")
text(8, 80, expression(y== 100/1+exp(-(17.4962 + 2.0349*x))))#just copy parameters

par(new=T)
#Lm50_male
17.4962/2.0349
#LM_female
abline(v=116.3501, lwd=2, lty=2, col="red")


#Lm50_male
5.52592/0.05041
abline(v=, lwd=2, lty=2, col="blue")
abline(h=50, lty=2, col="black")
