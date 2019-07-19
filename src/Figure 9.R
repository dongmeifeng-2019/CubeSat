library(tidyverse)

##################change working directory here###############
setwd('D:/CubeSat')

nrmse_mr = read.table('cache/discharge/06/nRMSE.txt',header=T)
nrmse_rs = read.table('cache/discharge/07/nRMSE.txt',header=T)

windows(12,9)
par(mfrow=c(2,1))
par(mar=c(3.6,3.6,1.1,1.1),mgp=c(2.2,0.8,0),family = "serif")
color.names = c("red",'skyblue','black','darkseagreen','goldenrod1','chartreuse4')

barplot(t(nrmse_mr),beside=T,ylim=c(0,100),xlab="",ylab="nRMSE (%)", col=color.names,axis.lty="solid")
abline(v=29,lty=2)
abline(v=50,lty=2)
text(x=14,y=95,c('Large rivers'))
text(x=14,y=90,c('(width > 100 m)'))
text(x=38,y=95,c('Medium rivers'))
text(x=38,y=90,c('(100 m >= width > 40 m)'))

text(x=60,y=95,c('Small rivers'))
text(x=60,y=90,c('(40 m >= width)'))

barplot(t(nrmse_rs),beside=T,ylim=c(0,100),xlab="Reach",ylab="nRMSE (%)", col=color.names,axis.lty="solid")
abline(v=29,lty=2)
abline(v=50,lty=2)
legend('topleft', legend=c('Landsat','Sentinel-2','Planet','Landsat+Sentinel-2',
                           'Sentinel-2+Planet','Landsat+Sentinel-2+Planet'),
       col=color.names,fill=color.names,  cex=0.8,horiz=F,bty='n')



