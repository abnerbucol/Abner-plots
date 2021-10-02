data1<-read.csv(file.choose(), header=TRUE)
str(data1)
#simple
counts <- table(data1$count)
barplot(counts, main=" ",
        xlab=" ")
x<-c(data1$location)
y<-c(data1$count)
barplot(y, col=as.numeric(x), 
        xlab="site", ylab=" ")
library(plotrix)
#set gap limit
from <-30
to <-35
gap.barplot(data1$count, gap=c(from,to), col=as.factor(x), 
            xlab=" ", ylab=" ")
axis.break(2, from, breakcol="grey", style="gap")
axis.break(2, from*(1+0.02), breakcol="black", style="slash")
axis.break(4, from*(1+0.02), breakcol="black", style="slash")
axis(3, at=from)

abline(v=13.5, lwd=0.5, lty=2, col="grey")
abline(v=26.5, lwd=0.5, lty=2, col="grey")
abline(v=36.5, lwd=0.5, lty=2, col="grey")

legend("topright", inset=.04, title=" ",
       c("Hipposideros obscurus", "Miniopterus australis", "Rhinolophus arcuatus", "Hipposideros diadema", "Megaderma spasma"), fill=as.numeric(x), horiz=FALSE, cex=0.6)

#fish
from <-20
to <-35
gap.barplot(data1$count, gap=c(from,to), col=as.numeric(x), 
            xlab="type", ylab="microplastic particles")
axis.break(2, from, breakcol="grey", style="gap")
axis.break(2, from*(1+0.02), breakcol="black", style="slash")
axis.break(4, from*(1+0.02), breakcol="black", style="slash")
axis(2, at=from)
#legend
legend("topleft", inset=.04, title="Type",
       c("GPPS","PE", "PET", "PA", "PP"), fill=as.numeric(x), horiz=FALSE, cex=0.6)
abline(v=5.5, lwd=0.5, lty=2, col="grey")
abline(v=10.5, lwd=0.5, lty=2, col="grey")
abline(v=15.5, lwd=0.5, lty=2, col="grey")

#simple
barplot(data1$count, col=as.numeric(x), 
            xlab="type", ylab="microplastic particles")
#legend
legend("topright", inset=.03, title="Type",
       c("HDPE","LDPE", "Polyamide", "TPU"), fill=as.numeric(x), horiz=FALSE, cex=0.8)


#boxplot
boxplot(data1$size~data1$medium, 
        xlab="Medium", ylab="Size (microns)", 
        col=topo.colors(3))

legend("topleft", inset=.04, title="Type",
       c("Clam","Fish","Sand"), fill=topo.colors(8), horiz=FALSE, cex=0.8)

abline(h=1415.778, lwd=0.5, lty=2, col="blue")
abline(h=1804.294, lwd=0.5, lty=2, col="green")
abline(h=1367.693, lwd=0.5, lty=2, col="yellow")
