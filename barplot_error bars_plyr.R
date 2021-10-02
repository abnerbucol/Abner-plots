#NEW_PLYR_barplot_error bars NEW
#Use Iris data available in R
#Use ddply to summarize data - the summary will be used to create bar chart
getwd()
#setwd("C:/Users/admin/Documents/")
setwd("C:/Users/admin/Documents/Mantehage")
library(plyr)
library(lattice)
df<-read.csv("fecundity_year.csv")
str(df)
data <- ddply(df, c("year"), summarise, 
              n = length(Fec),
              mean = mean(Fec),
              sd = sd(Fec),
              se = sd / sqrt(n))
data
view(data)
#optional_copy_summarised_data
x <- readClipboard()
write.table(data, "clipboard", sep="\t", row.names=FALSE)
write.table(x, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)

#add error bar dimensions to "data" dataframe
# ulim = upper limit
# llim = lower limit
data$ulim <- data$mean + data$se
data$llim <- data$mean - data$se
data

#plot the barchart
barchart(data$mean ~ data$site, ylim=c(0,10),
         xlab = list(label = "site", fontsize = 10),
         ylab = list(label = "% cover", fontsize = 10),
         scales = list(alternating = FALSE, tck = c(1,0), cex=0.8),
         panel = function(x, y, ..., subscripts) 
         {panel.barchart(x, y, subscripts = subscripts, ...)
           ll = data$llim[subscripts]
           ul = data$ulim[subscripts]
           #vertical error bars
           panel.segments(as.numeric(x), ll,  
                          as.numeric(x), ul,
                          col = 'black', lwd = 1)
           #lower horizontal cap
           panel.segments(as.numeric(x) - 0.1, ll,  
                          as.numeric(x) + 0.1, ll,
                          col = 'black', lwd = 1)
           #upper horizontal cap
           panel.segments(as.numeric(x) - 0.1, ul,  
                          as.numeric(x) + 0.1, ul,
                          col = 'black', lwd = 1)
         })

#sample only_added random assignment in treatment
density$no <- 1:51
density$trt <- ifelse(density$no %% 2 == 0, "A", "B")
category <-(data2$category)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Barchart with grouped bars (DENSITY)
getwd()
setwd("C:/Users/Owner/Documents/RFishBase/NEW/Apo")
density<-read.csv("Apo_density_MR.csv")

density<-read.csv("Apo_density_F.csv")

library(plyr)
str(benthic)
data2 <- ddply(benthic, c("PS", "DOP"), summarise, 
               n = length(DS),
               mean = mean(DS),
               sd = sd(DS),
               se = sd / sqrt(n))

data2$ulim <- data2$mean + data2$se
data2$llim <- data2$mean - data2$se

data2$err <- ifelse(data2$DOP == "DOP", -0.5, 0.5)
data2

#PLOT
barchart(data2$mean ~ data2$DOP, 
         groups = data2$PS, #Be sure to add the groups argument to specify how to group the bars
         ylim = c(0, 100),
         auto.key = list(space = "top", rectangles = TRUE, points = FALSE), #add legend
         xlab = list(label = "years", fontsize = 12),
         ylab = list(label = "%", fontsize = 12),
         scales = list(alternating = TRUE, tck = c(1,0), cex=0.8),
         panel = function(x, y, ..., subscripts) 
         {panel.barchart(x, y, subscripts = subscripts, ...)
           ll = data2$llim[subscripts]
           ul = data2$ulim[subscripts]
           #vertical error bars
           panel.segments(as.numeric(x) + data2$err[subscripts], ll, #added data2$err[subscripts]
                          as.numeric(x) + data2$err[subscripts], ul, #to specify offset of error
                          col = 'black', lwd = 1)                    #bars
           #lower horizontal cap
           panel.segments(as.numeric(x) + data2$err[subscripts] - 0.1, ll, #same as above 
                          as.numeric(x) + data2$err[subscripts] + 0.1, ll,
                          col = 'black', lwd = 1)
           #upper horizontal cap
           panel.segments(as.numeric(x) + data2$err[subscripts] - 0.1, ul, #same as above 
                          as.numeric(x) + data2$err[subscripts] + 0.1, ul,
                          col = 'black', lwd = 1)
         })


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Barchart with grouped bars (BIOMASS)
getwd()
setwd("C:/Users/Owner/Documents/RFishBase/NEW/Apo")
biomass<-read.csv("Apo_biomass_MR.csv")

biomass<-read.csv("Apo_biomass_F.csv")
library(plyr)
str(biomass)
data2 <- ddply(biomass, c("category", "years"), summarise, 
               n = length(biomass),
               mean = mean(biomass),
               sd = sd(biomass),
               se = sd / sqrt(n))

data2$ulim <- data2$mean + data2$se
data2$llim <- data2$mean - data2$se

data2$err <- ifelse(data2$category == "category", -0.0175, 0.0175)
data2

#PLOT
library(lattice)
barchart(data2$mean ~ data2$years, 
         groups = data2$category, #Be sure to add the groups argument to specify how to group the bars
         ylim = c(0, 1),
         auto.key = list(space = "top", rectangles = TRUE, points = FALSE), #add legend
         xlab = list(label = "years", fontsize = 12),
         ylab = list(label = "biomass", fontsize = 12),
         scales = list(alternating = TRUE, tck = c(1,0), cex=0.8),
         panel = function(x, y, ..., subscripts) 
         {panel.barchart(x, y, subscripts = subscripts, ...)
           ll = data2$llim[subscripts]
           ul = data2$ulim[subscripts]
           #vertical error bars
           panel.segments(as.numeric(x) + data2$err[subscripts], ll, #added data2$err[subscripts]
                          as.numeric(x) + data2$err[subscripts], ul, #to specify offset of error
                          col = 'black', lwd = 1)                    #bars
           #lower horizontal cap
           panel.segments(as.numeric(x) + data2$err[subscripts] - 0.1, ll, #same as above 
                          as.numeric(x) + data2$err[subscripts] + 0.1, ll,
                          col = 'black', lwd = 1)
           #upper horizontal cap
           panel.segments(as.numeric(x) + data2$err[subscripts] - 0.1, ul, #same as above 
                          as.numeric(x) + data2$err[subscripts] + 0.1, ul,
                          col = 'black', lwd = 1)
         })

