library(lubridate)
setwd('D:/RiverWidth/Arcrwl/Planet')
discharge_folder='cache/bam_discharge/01/'

idlist=c('15453500','15356000','15485500','15515500','15484000','15493400','15514000','15477740',
         '15493700','15511000','15519100')

width_list = c('LS','ST','PL','LS-ST', 'ST-PL','comb')  # LS: Landsat, ST: Sentinel-2, PL: Planet, comb: 3 datasets combined
width_input = width_list[3]
reachID = idlist[7]

t = c(seq(as.Date("2016.05.14","%Y.%m.%d" ), as.Date("2016.09.15","%Y.%m.%d" ),'days'),
      seq(as.Date("2017.05.14","%Y.%m.%d" ), as.Date("2017.09.15","%Y.%m.%d" ),'days'),
      seq(as.Date("2018.05.14","%Y.%m.%d" ), as.Date("2018.09.15","%Y.%m.%d" ),'days'))


for(reachID in idlist[9]){
  gauge_folder='data/Priors/USGS/'
  gauge_in=read_delim(paste(gauge_folder,reachID,'.txt',sep=""),delim=" ") %>%
    transmute(gauge_Q= as.integer(q_cfs)*(.3048^3), Date= time  )
  gauge_in = data.frame(Date=t)%>%
    left_join(gauge_in,by='Date')
  output_all = gauge_in[gauge_in$Date%in%t,]
  for(width_input in width_list){
    if((reachID%in%idlist[1:4]&width_input%in%width_list[c(1,4,6)])|
       (reachID%in%idlist[1:9]&width_input%in%width_list[c(2,5)])|
       (reachID%in%idlist&width_input%in%width_list[3])
    ){
      
      file_q=paste(discharge_folder,reachID,'_', width_input,'.txt',sep="")
      Q_in=read_delim(file_q,delim=" ") %>%
        transmute(gauge_Q= as.integer(gauge_Q), rc_Q= as.integer(rc_Q),Date= as.Date(Date)  )
      #read.table(file_q,header = T)
      
      output = data.frame(Q_in)%>%
        select(-gauge_Q)
      
      output_all = output_all%>%
        left_join(output,by='Date')
    }
  }
}

colnames(output_all)=c('Date','Gauge_Q',width_list[c(2,3,5)])
output_all = output_all[order(output_all$Date),]
tiff(paste0("cache/bam_discharge/01/",reachID,".tiff"), 
     height = 10.16, width = 17.78, units = 'cm', res = 1200)
ymin=min(output_all[,2],na.rm=T)
ymax=max(output_all[,2],na.rm=T)*0.7

#windows(7,4)
par(mfrow=c(4,2))
par(mar=c(1,3.0,1.1,0),mgp=c(1.5,0.5,0),family = "serif")
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
ts1 = output_all[complete.cases(output_all[,3]),1:3]
ts1=unique(ts1)
par(fig=c(0,4,5.2,10)/10,new=TRUE)
plot(c(1:nrow(ts1)),ts1$Gauge_Q,font=2,type='l',col='black',tck=-0.03,lwd=2,
     ylab=expression(paste('Discharge (m'^3,'/s)',sep='')),
     ylim=c(ymin,ymax))
lines(c(1:nrow(ts1)),ts1$ST,col='red')
legend('topright',c('Gauge','Simulation'),col=c('black','red'),lty=1,bty='n',lwd=c(2,1))
text(x=1,ymax,labels='a',font=2,cex=1.5)
ts2 = output_all[rowSums(is.na(output_all[,c(1,2,4)]) )<1,c(1,2,4)]
ts2=unique(ts2)
par(mar=c(1,3.0,1.1,1.1),mgp=c(1.5,0.5,0),family = "serif")
par(fig=c(4,10,5.2,10)/10,new=TRUE)
plot(c(1:nrow(ts2)),ts2$Gauge_Q,font=2,type='l',col='black',tck=-0.03,lwd=2,
     ylab=expression(paste('Discharge (m'^3,'/s)',sep='')),
     ylim=c(ymin,ymax))
lines(c(1:nrow(ts2)),ts2$PL,col='red')

text(x=1,ymax,labels='b',font=2,cex=1.5)
ts3 = output_all[rowSums(is.na(output_all[,c(1,2,5)]) )<1,c(1,2,5)]
ts3=unique(ts3)
par(fig=c(0,10,0,5.2)/10,new=TRUE)  #x1,x2,y1,y2
par(mar=c(2.5,3.0,0.7,1.1),mgp=c(1.5,0.5,0),family = "serif")
plot(c(1:nrow(ts3)),ts3$Gauge_Q,font=2,type='l',col='black',tck=-0.03,lwd=2,xlab='Time Step',
     ylab=expression(paste('Discharge (m'^3,'/s)',sep='')),
     ylim=c(ymin,ymax))
lines(c(1:nrow(ts3)),ts3$'ST-PL',col='red')
text(x=0.5,ymax,labels='c',font=2,cex=1.5)
dev.off()
