#barplot with dplyr
library(ggplot2)
getwd()
setwd("C:/Users/Owner/Documents/RFishBase/barchart")
data1<-read.csv("bleekeri_density.csv")
str(data1)
library(dplyr)
#try only
data <- data1 %>% # the names of the new data frame and the data frame to be summarised
  group_by(years) %>%   # the grouping variable
  summarise(mean_PL = mean(density),  # calculates the mean of each group
            sd_PL = sd(density), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(density)/sqrt(n())) # calculates the standard error of each group

plot <- ggplot(data, aes(years, mean_PL)) +
  geom_col() +  
  geom_errorbar(aes(ymin = mean_PL - sd_PL, ymax = mean_PL + sd_PL), width=0.4) #Standard error

plot + labs(y=" ", x = " ") + theme_classic()

#target
data <- data1 %>% # the names of the new data frame and the data frame to be summarised
  group_by(site) %>%   # the grouping variable
  summarise(mean_PL = mean(target),  # calculates the mean of each group
            sd_PL = sd(target), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(target)/sqrt(n())) # calculates the standard error of each group
plot <- ggplot(data, aes(years, mean_PL)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = mean_PL - sd_PL, ymax = mean_PL + sd_PL), width=0.2) #Standard deviation

plot + labs(y=" ", x = "years") + theme_classic()

#herbivore
data <- data1 %>% # the names of the new data frame and the data frame to be summarised
  group_by(site) %>%   # the grouping variable
  summarise(mean_PL = mean(herbivore),  # calculates the mean of each group
            sd_PL = sd(herbivore), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(herbivore)/sqrt(n())) # calculates the standard error of each group
plot <- ggplot(data, aes(site, mean_PL)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = mean_PL - sd_PL, ymax = mean_PL + sd_PL), width=0.2) #Standard deviation

plot + labs(y=" ", x = "Site") + theme_classic()

#planktivore
data <- data1 %>% # the names of the new data frame and the data frame to be summarised
  group_by(site) %>%   # the grouping variable
  summarise(mean_PL = mean(planktivore),  # calculates the mean of each group
            sd_PL = sd(planktivore), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(planktivore)/sqrt(n())) # calculates the standard error of each group
plot <- ggplot(data, aes(site, mean_PL)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = mean_PL - sd_PL, ymax = mean_PL + sd_PL), width=0.2) #Standard deviation

plot + labs(y=" ", x = "Site") + theme_classic()

#predator
data <- data1 %>% # the names of the new data frame and the data frame to be summarised
  group_by(site) %>%   # the grouping variable
  summarise(mean_PL = mean(predator),  # calculates the mean of each group
            sd_PL = sd(predator), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(predator)/sqrt(n())) # calculates the standard error of each group
plot <- ggplot(data, aes(site, mean_PL)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = mean_PL - sd_PL, ymax = mean_PL + sd_PL), width=0.2) #Standard deviation

plot + labs(y=" ", x = "Site") + theme_classic()



#SE
#Buntod biomass plot
library(ggplot2)
biomass<-read.csv(file.choose(), header=TRUE)
str(data1)
library(dplyr)
biomass_summary <- biomass %>% # the names of the new data frame and the data frame to be summarised
  group_by(site) %>%   # the grouping variable
  summarise(mean_PL = mean(total),  # calculates the mean of each group
            sd_PL = sd(total), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(total)/sqrt(n())) # calculates the standard error of each group
plot <- ggplot(biomass_summary, aes(site, mean_PL)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = mean_PL - SE_PL, ymax = mean_PL + SE_PL), width=0.2)

plot + labs(y="total biomass (kg/1000m2) ? s.e.", x = "Site") + theme_classic()

#Buntod density plot
library(ggplot2)
density<-read.csv(file.choose(), header=TRUE)
str(density)
library(dplyr)
density_summary <- density %>% # the names of the new data frame and the data frame to be summarised
  group_by(site) %>%   # the grouping variable
  summarise(mean_PL = mean(predator),  # calculates the mean of each group
            sd_PL = sd(predator), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(predator)/sqrt(n())) # calculates the standard error of each group
plot <- ggplot(density_summary, aes(site, mean_PL)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = mean_PL - SE_PL, ymax = mean_PL + SE_PL), width=0.2)

plot + labs(y="predator density (ind./1000m2) ? s.e.", x = "Site") + theme_classic()



#other parameters with S.E.
seagrass_summary <- seagrass %>% # the names of the new data frame and the data frame to be summarised
  group_by(species) %>%   # the grouping variable
  summarise(mean_PL = mean(leaf.width),  # calculates the mean of each group
            sd_PL = sd(leaf.width), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(leaf.width)/sqrt(n())) # calculates the standard error of each group
seagrassPlot <- ggplot(seagrass_summary, aes(species, mean_PL)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = mean_PL - SE_PL, ymax = mean_PL + SE_PL), width=0.2)

seagrassPlot + labs(y="leaf.width (cm) ? s.e.", x = "Species") + theme_classic()

#GSI with S.E.
gsi<-read.csv(file.choose(), header=TRUE)
str(gsi)
library(ggplot2)
library(dplyr)
gsi_summary <- gsi %>% # the names of the new data frame and the data frame to be summarised
  group_by(month) %>%   # the grouping variable
  summarise(mean_PL = mean(GSI),  # calculates the mean of each group
            sd_PL = sd(GSI), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(GSI)/sqrt(n())) # calculates the standard error of each group
gsiPlot <- ggplot(gsi_summary, aes(month, mean_PL)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = mean_PL - SE_PL, ymax = mean_PL + SE_PL), col="blue", width=0.2)

gsiPlot + labs(y="GSI (%) ? s.e.", x = "Month") + theme_classic()






#simple barchart
data <- c(3,
          5,
          7,
          9,
          10,
          4,
          8,
          7,
          9,
          11
)
barplot(data)
#with slash
data1<-read.csv(file.choose(), header=TRUE)
str(data1)
age<- c(5,10,20,10,10,20,30,20,30,40,5,5,40)
table(age)
barplot(table(age),
        main="Age Count of 10 Students",
        xlab="Age",
        ylab="Count",
        border="red",
        col="blue",
        density=5
)

XT = xtabs(EPT~code, data=data1)
XT
barplot(XT,
        beside = TRUE,
        legend = FALSE,
        ylim   = c(0, 6),    # adjust to remove legend overlap
        xlab   = "Station",
        ylab   = "EPT Index")
