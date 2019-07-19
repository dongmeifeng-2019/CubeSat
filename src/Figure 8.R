library(tidyverse)

##################change working directory here###############
setwd('D:/CubeSat')

nrmse_ncep = read.table('cache/discharge/03/nRMSE.txt',header=T)
nrmse_merra = read.table('cache/discharge/04/nRMSE.txt',header=T)
nrmse_era = read.table('cache/discharge/05/nRMSE.txt',header=T)

windows(12,12)
par(mfrow=c(3,1))
par(mar=c(3.6,3.6,1.1,1.1),mgp=c(2.2,0.8,0),family = "serif")
color.names = c("red",'skyblue','black','darkseagreen','goldenrod1','chartreuse4')

barplot(t(nrmse_ncep),beside=T,ylim=c(0,100),xlab="",ylab="nRMSE (%)", col=color.names,axis.lty="solid")
legend('topleft', legend=c('Landsat','Sentinel-2','Planet','Landsat+Sentinel-2',
                           'Sentinel-2+Planet','Landsat+Sentinel-2+Planet'),
       col=color.names,fill=color.names,  cex=1.2,horiz=F,bty='n')
abline(v=29,lty=2)
abline(v=50,lty=2)
text(x=14,y=95,c('Large rivers'),cex=1.2)
text(x=14,y=88,c('(width > 100 m)'),cex=1.2)
text(x=33.5,y=95,c('Medium rivers'),cex=1.2)
text(x=33.5,y=88,c('(40 ~ 100 m)'),cex=1.2)

text(x=55,y=95,c('Small rivers'),cex=1.2)
text(x=55,y=88,c('(40 m >= width)'),cex=1.2)

barplot(t(nrmse_merra),beside=T,ylim=c(0,100),xlab="",ylab="nRMSE (%)", col=color.names,axis.lty="solid")
abline(v=29,lty=2)
abline(v=50,lty=2)

barplot(t(nrmse_era),beside=T,ylim=c(0,100),xlab="Reach",ylab="nRMSE (%)", col=color.names,axis.lty="solid")
abline(v=29,lty=2)
abline(v=50,lty=2)
