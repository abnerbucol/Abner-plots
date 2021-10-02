data1<-read.csv(file.choose(), header=TRUE)
str(data1)
library(ggplot2)
ggplot(data1, aes(x=size, fill=medium)) +
  geom_histogram()
