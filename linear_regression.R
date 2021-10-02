getwd()
setwd("C:/Users/admin/Documents/R4/contaminants")
df <- read.csv("phthalates.csv")
str(df)
x<-c(df$density)
y<-c(df$total)

y<-c(df$female)
y<-c(df$imm)
#plot data
plot(y~x, pch=19, xlim=c(0,10), axes=TRUE, ylim=c(0,1), xlab=" ", ylab= " ")
#linear regression
lm.model<-lm(y~x, df)
summary(lm.model)
x<-seq(0,10)
mod2 <-(predict(lm.model, list(x=x)))
lines(mod2~x, lwd=2, lty=1, col="black")
