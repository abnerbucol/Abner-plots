getwd()
setwd("C:/Users/admin/Documents/RFishBase/NEW/density_sex")
data<-read.csv("bleekeri_density_MR.csv")
str(data)

summary(data1)
#one group
data <- ddply(data, c("category"), summarise, 
              n = length(density),
              mean = mean(density),
              sd = sd(density),
              se = sd / sqrt(n))
data
