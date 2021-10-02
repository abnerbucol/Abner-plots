#ggplot_graphs_SD (also stacked)
getwd()
setwd("C:/Users/admin/Documents/R4/contaminants/Analysis")
df<-read.csv("pesticides.csv")
str(df)
library(ggplot2)
library(tidyverse)
head(df)
summary(df)
#selecting columns
df_4<- select(df, 2:10)
head(df_4)
#filtering rows
df_site<- filter(df, site=="Bais")
head(df_site)
dim(df_site)
#gathering columns to make table
df_long<- gather(df, type, concentration, 2:10)
head(df_long)
#grouping&sumarizing data
#group by
df_sumzd<- group_by(df_long, site)
head(df_sumzd)
#summarize
#df_sumzd<-group_by(df_long, site, type) %>% summarise(max=max(concentration), sd=sd(concentration))
df_sumzd<-group_by(df_long, site, type) %>% summarise(max=max(concentration))
head(df_sumzd)
view(df_sumzd)
#copy_summarized_data
x <- readClipboard()
write.table(df_sumzd, "clipboard", sep="\t", row.names=FALSE)
#write.table(x, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)

#plot basic plots (with identity)
p<-ggplot(df_sumzd, aes(x=site, y=max))+
  geom_bar(stat="identity")
p

#fill color  
p<-ggplot(df_sumzd, aes(x=site, y=max))+
  geom_bar(stat="identity", fill="red")
p

#fill color, mapping to variable
p<-ggplot(df_sumzd, aes(x=site, y=max, fill=type))+
  geom_bar(stat="identity")
p

#fill color, stacked barplot
p<-ggplot(df_sumzd, aes(x=site, y=max, fill=type))+
  geom_bar(stat="identity")
p

library("ggsci")
#fill color, stacked barplot
p<-ggplot(df_sumzd, aes(x=site, y=max, fill=type))+
  geom_bar(stat="identity")+
  coord_flip()+
  theme_bw()+
  scale_fill_npg()
  #scale_fill_viridis_d()
p + theme(
  legend.position = "right",
  legend.direction= "vertical",
  legend.key.size = unit(0.9, "lines"),
  legend.title = element_text(color = "blue", size = 9),
  legend.text = element_text(color = "black", size = 9)
)  
########################################################################




#with error bars
p<-ggplot(df, aes(x=site, y=mean, fill=mes_type, label=mean))+
  geom_bar(stat="identity", position="dodge")+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.25, 
                size=1, position=position_dodge(0.9), alpha=0.3)+
  theme_bw()
p

