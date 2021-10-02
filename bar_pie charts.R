# Pie Chart with Percentages
slices <- c(33, 9, 7, 2, 3, 3, 7) 
slices <- c(51.56, 14.06, 10.94, 3.13, 4.69, 4.69, 10.94) 
lbls <- c("Pandanus leaf axil/surface", "palm leaves", "tree fern", "Musa leaf axil/surface", "tree leaves","tree buttress with fern", "shrubs")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main=" ")

# bar chart
dat <- read.table(text = "EVA	GPPS	HDPE	LDPE	PET	Polyamide	Polysmide	PP	PVC
Ayungon	0	0	0	0	0	0	1	0	0
Bais 	0	0	0	1	0	0	1	0	0
Dumaguete	1	1	7	3	4	1	1	1	0
Manjuyod	2	0	2	0	0	0	2	44	1"
                  , header = TRUE)
barplot(as.matrix(dat), xlab="Type", ylab="count", col=terrain.colors(3))

library(reshape2)
dat$row <- seq_len(nrow(dat))
dat2 <- melt(dat, id.vars = "row")
library(ggplot2)
ggplot(dat2, aes(x=variable, y=value, fill=row)) + 
  geom_bar(stat="identity") +
  xlab("transect") +
  ylab("percent cover") +
  guides(fill=FALSE) +
  theme_bw()

#########################barchart using easyGgplot2#########################
getwd()
install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")
library(easyGgplot2)
df <- read.csv("seagrass.csv")
head(df)
ggplot2.barplot(data=df, xName="transect", yName='seagrass', 
backgroundColor="pink",
mainTitle="(a) seagrass cover",
xtitle="Transect", ytitle="Percentage",
mainTitleFont=c(14,"bold.italic", "blue"),
xtitleFont=c(14,"bold", "#993333"),
ytitleFont=c(14,"bold", "#993333"), 
ylim=c(0,100))

#############################################################################
df <- read.csv("job.csv")
head(df)
ggplot2.barplot(data=df, xName="Occupation", yName='Percentage', 
                backgroundColor="lightblue",
mainTitle="(c) Occupation",
xtitle="Occupation", ytitle="Percentage",
mainTitleFont=c(14,"bold.italic", "red"),
xtitleFont=c(14,"bold", "#993333"),
ytitleFont=c(14,"bold", "#993333"), 
ylim=c(0,100))
##########################
df <- read.csv("age.csv")
head(df)
ggplot2.barplot(data=df, xName="age", yName='number.of.respondents', 
                backgroundColor="lightblue",
                mainTitle="(a) Age profile of respondents",
                xtitle="Age (years)", ytitle="no. respondents",
                mainTitleFont=c(14,"bold.italic", "red"),
                xtitleFont=c(14,"bold", "#993333"),
                ytitleFont=c(14,"bold", "#993333"), 
                ylim=c(0,8))
############resources####
#pie chart
# 3D Exploded Pie Chart
library(plotrix)
slices <- c(64.7, 55.9, 50, 26.5, 11.8, 5.9) 
lbls <- c("coral reef (64.7%)", "mangrove (55.9%)", "seagrass (50%)", "wood (26.5%)", "fish (11.8%)", "algal bed (5.9%)")
pie3D(slices,labels=lbls,explode=0.1,
      main="identified resources")
###############threats###
df <- read.csv("threat.csv")
head(df)
ggplot2.barplot(data=df, xName="threat", yName='percentage', 
                backgroundColor="white",
                mainTitle="Threats to natural resources",
                xtitle="Threats", ytitle="percentage",
                mainTitleFont=c(14,"bold.italic", "red"),
                xtitleFont=c(14,"bold", "#993333"),
                ytitleFont=c(14,"bold", "#993333"), 
                ylim=c(0,100))
#########mineral###########
df <- read.csv("mineral.csv")
head(df)
ggplot2.barplot(data=df, xName="mineral", yName='percentage', 
                backgroundColor="light green",
                mainTitle="mineral resources",
                xtitle="Mineral", ytitle="percentage",
                mainTitleFont=c(14,"bold.italic", "blue"),
                xtitleFont=c(14,"bold", "#993333"),
                ytitleFont=c(14,"bold", "#993333"), 
                ylim=c(0,50))
#pie chart
# 3D Exploded Pie Chart
library(plotrix)
slices <- c(23, 12.3, 6.05, 3.49, 3.49, 3.26, 2.56, 2.33, 2.09, 1.86) 
lbls <- c("ground w/ leaf litter (23%)", "bare boulder (12.33%)", "fern (6.03%)", "boulder w/ fern (3.49%)", "rock w/ moss (3.49%)", "ground w/ leaf litter,fern,moss (3.26%)", "tree buttress (2.56%)", "log (2.3%)", "tree buttress w/ leaf litter (2.09%)", "rock w/ leaf litter (1.86%)")
pie3D(slices,labels=lbls,explode=0.2,
      main="Platymantis dorsalis: microhabitat usage")

slices <- c(57.8, 8.9, 8.9, 4.4, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2, 2.2) 
lbls <- c("GLL (57.8%)", "F (8.9%)", "GB (8.9%)", "GLR (4.4%)", "AL (2.2%)", "BL (2.2%)","BLL_f (2.2%)", "GF (2.2%)", "L (2.2%)", "LF (2.2%)", "LLL (2.2%)", "RLL (2.2%)", "TB (2.2%)")
pie3D(slices,labels=lbls,explode=0.2,
      main="Platymantis corrugatus: microhabitat usage")

slices <- c(55.8, 14, 10.5, 8.1, 7, 3.5, 1.2) 
lbls <- c("Pandanus (55.8%)", "palm (14%)", "tree fern (10.5%)", "shrub leaves (8.1%)", "Musa leaf/axil (7 %)", "tree leaves (3.5%)","tree buttress w/ferns,litter (1.2%)")
pie3D(slices,labels=lbls,explode=0.2,
      main="Platymantis hazelae: microhabitat usage")

citation()
