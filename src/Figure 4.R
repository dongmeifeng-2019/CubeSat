library(tidyverse)

###############change working directory here##############
setwd('D:/CubeSat')

width_mean = read.table('data/Widths/width_mean.txt',header=T)
width_count = read.table('data/Widths/width_count.txt',header=T)

windows(12,9)
par(mfrow=c(2,1))
par(mar=c(3.6,3.6,1.1,1.1),mgp=c(2.2,0.8,0),family = "serif")
color.names = c("red",'blue','black')

barplot(t(log10(width_mean)),beside=T,ylim=c(0,3),xlab="",ylab="Width (m)", col=color.names,axis.lty="solid",
        yaxt='n')
abline(v=17,lty=2)
abline(v=29,lty=2)
legend('topright', legend=c('Landsat','Sentinel-2','Planet'),col=color.names,fill=color.names,  cex=0.8,horiz=T,bty='n')
axis(2, at=c(0,1,2,3),labels=c('1','10','100','1000'), col.axis="black", las=2)

barplot(t(width_count),beside=T,ylim=c(0,70),xlab="Reach",ylab="Number of Images", col=color.names,axis.lty="solid")
abline(v=17,lty=2)
abline(v=29,lty=2)
text(x=6,y=65,c('Large rivers'))
text(x=6,y=60,c('(width > 100 m)'))
text(x=23,y=65,c('Medium rivers'))
text(x=23,y=60,c('(100 m >= width > 40 m)'))

text(x=36,y=65,c('Small rivers'))
text(x=36,y=60,c('(40 m >= width)'))


