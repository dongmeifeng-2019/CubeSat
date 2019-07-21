library(tidyverse)

##################change working directory here###############
setwd('D:/CubeSat')

nrmse_rc = read.table('cache/discharge/01/nRMSE.txt',header=T)
nrmse_bam = read.table('cache/discharge/02/nRMSE.txt',header=T)

windows(12,9)
par(mfrow=c(3,2))
par(oma = c(0, 0, 2, 0))
par(mar=c(3.6,3.6,1.1,1.1),mgp=c(2.2,0.8,0),family = "serif")
color.names = c("red",'skyblue','black','darkseagreen','goldenrod1','chartreuse4')
barplot(t(nrmse_bam[1:4,]),beside=T,ylim=c(0,110),xlab="",ylab="nRMSE (%)", col=color.names,axis.lty="solid")
legend('topleft', legend=c('Landsat','Sentinel-2','Planet','Landsat+Sentinel-2',
                           'Sentinel-2+Planet','Landsat+Sentinel-2+Planet'),
       col=color.names,fill=color.names,  cex=1.2,horiz=F,bty='n')
mtext('BAM',side=3,cex=2.0)
barplot(t(nrmse_rc[1:4,]),beside=T,ylim=c(0,110),xlab="",ylab="nRMSE (%)", col=color.names,axis.lty="solid")
mtext('Rating Curve',side=3,cex=2.0)
barplot(t(nrmse_bam[5:7,]),beside=T,ylim=c(0,110),xlab="",ylab="nRMSE (%)", col=color.names,axis.lty="solid")
barplot(t(nrmse_rc[5:7,]),beside=T,ylim=c(0,110),xlab="",ylab="nRMSE (%)", col=color.names,axis.lty="solid")
barplot(t(nrmse_bam[8:11,]),beside=T,ylim=c(0,110),xlab="",ylab="nRMSE (%)", col=color.names,axis.lty="solid")
barplot(t(nrmse_rc[8:11,]),beside=T,ylim=c(0,110),xlab="",ylab="nRMSE (%)", col=color.names,axis.lty="solid")



# 
# 
# 
# 
# windows(12,9)
# par(mfrow=c(2,1))
# par(mar=c(3.6,3.6,1.1,1.1),mgp=c(2.2,0.8,0),family = "serif")
# color.names = c("red",'skyblue','black','darkseagreen','goldenrod1','chartreuse4')
# 
# barplot(t(nrmse_bam),beside=T,ylim=c(0,110),xlab="",ylab="nRMSE (%)", col=color.names,axis.lty="solid")
# abline(v=29,lty=2)
# abline(v=50,lty=2)
# text(x=14,y=65,c('Large rivers'))
# text(x=14,y=60,c('(width > 100 m)'))
# text(x=38,y=108,c('Medium rivers'))
# text(x=38,y=103,c('(100 m >= width > 40 m)'))
# 
# text(x=63,y=75,c('Small rivers'))
# text(x=63,y=70,c('(40 m >= width)'))
# 
# barplot(t(nrmse_rc),beside=T,ylim=c(0,100),xlab="Reach",ylab="nRMSE (%)", col=color.names,axis.lty="solid")
# abline(v=29,lty=2)
# abline(v=50,lty=2)
# legend('topleft', legend=c('Landsat','Sentinel-2','Planet','Landsat+Sentinel-2',
#                            'Sentinel-2+Planet','Landsat+Sentinel-2+Planet'),
#        col=color.names,fill=color.names,  cex=0.8,horiz=F,bty='n')
# 


