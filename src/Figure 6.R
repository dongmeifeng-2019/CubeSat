library(lubridate)

##################change working directory here###############
setwd('D:/CubeSat')


discharge_folder='cache/discharge/01/'

idlist=c('15453500','15356000','15485500','15515500','15484000','15493400','15514000','15477740',
         '15493700','15511000','15519100')

width_list = c('LS','ST','PL','LS-ST', 'ST-PL','comb')  # LS: Landsat, ST: Sentinel-2, PL: Planet, comb: 3 datasets combined
width_input = width_list[1]
reachID = idlist[7]

t = c(seq(as.Date("2016.05.14","%Y.%m.%d" ), as.Date("2016.09.15","%Y.%m.%d" ),'days'),
      seq(as.Date("2017.05.14","%Y.%m.%d" ), as.Date("2017.09.15","%Y.%m.%d" ),'days'),
      seq(as.Date("2018.05.14","%Y.%m.%d" ), as.Date("2018.09.15","%Y.%m.%d" ),'days'))

for(reachID in idlist[3]){
  gauge_folder='data/Priors/USGS/'
  gauge_in=read_delim(paste(gauge_folder,reachID,'.txt',sep=""),delim=" ") %>%
    transmute(gauge_Q= as.integer(q_cfs)*(.3048^3), Date= time  )
  gauge_in = data.frame(Date=t)%>%
    left_join(gauge_in,by='Date')
  output_all1 = gauge_in[gauge_in$Date%in%t,]
  output_all = gauge_in[gauge_in$Date%in%t,]
  for(width_input in width_list){
    if((reachID%in%idlist[1:4]&width_input%in%width_list[c(1,4,6)])|
       (reachID%in%idlist[1:9]&width_input%in%width_list[c(2,5)])|
       (reachID%in%idlist&width_input%in%width_list[3])
    ){
      ################------------rating curve---------------########
      discharge_folder1='cache/discharge/01/'
      file_q=paste(discharge_folder1,reachID,'_', width_input,'.txt',sep="")
      Q_in=read_delim(file_q,delim=" ") %>%
        transmute(gauge_Q= as.integer(gauge_Q), rc_Q= as.integer(rc_Q),Date= as.Date(Date)  )
      #read.table(file_q,header = T)
      
      output = data.frame(Q_in)%>%
        select(-gauge_Q)
      
      output_all1 = output_all1%>%
        left_join(output,by='Date')
      
      ####--------------BAM-----------------------###
      discharge_folder2='cache/discharge/02/'
      file_date=paste(discharge_folder2,reachID,'_', width_input,'_','dates.rds',sep="")
      dates_in=readRDS(file_date)
      date_df=data.frame(Date=dates_in,time=seq(1:length(dates_in)))
      
      for(k in 20:20){
        file_q=paste(discharge_folder2,reachID,'_', width_input,'_',k,'_BAM_b.rds',sep="")
        Q_in=readRDS(file_q)
        bam_out_df=(Q_in$data) %>%
          left_join(date_df,by="time") 
        
        bam_mean=filter(bam_out_df, stat == "mean") %>%
          select(-Date) %>%
          spread(series,flow) %>%
          transmute(time,bam_Q=rowMeans(.[,3:5]))
        
        output = date_df%>%
          left_join(bam_mean,by='time')%>%
          select(-time)
      }
      output_all = output_all%>%
        left_join(output,by='Date')
    }
  }
}

colnames(output_all)=c('Date','Gauge_Q',width_list)
output_all = output_all[order(output_all$Date),]
colnames(output_all1)=c('Date','Gauge_Q',width_list)
output_all1 = output_all1[order(output_all1$Date),]
#tiff(paste0("cache/discharge/01/",reachID,"_combined.tiff"), 
    # height = 22.86, width = 17.78, units = 'cm', res = 1200)
size=1.8
windows(7,9)
par(mfrow=c(4,2))
par(oma = c(0, 2, 0, 0)) 
par(mar=c(1,4,1.1,0),mgp=c(2,0.5,0),family = "serif")
layout(matrix(c(1,2,3,3,4,5,6,6), 4, 2, byrow = TRUE))
ts1 = output_all[complete.cases(output_all[,3]),1:3]
ts1=unique(ts1)
ts1_1 = output_all1[complete.cases(output_all1[,3]),1:3]
ts1_1=unique(ts1_1)
par(fig=c(0,4.5,7.56,9.99)/10,new=TRUE)
plot(c(1:nrow(ts1)),ts1$Gauge_Q,font=1,type='l',col='black',xlim=c(1,10),tck=0.02,lwd=2,cex.lab=size,cex.axis=size,
     ylab='',
     ylim=c(min(output_all$Gauge_Q,na.rm=T),max(output_all$Gauge_Q,na.rm=T)*1.1))
lines(c(1:nrow(ts1)),ts1$LS,col='red')
lines(c(1:nrow(ts1_1)),ts1_1$LS,col='blue')
text(x=1.2,y=max(output_all$Gauge_Q,na.rm=T)*1.07,labels='a',font=2,cex=1.5*size)

ts2 = output_all[rowSums(is.na(output_all[,c(1,2,4)]) )<1,c(1,2,4)]
ts2=unique(ts2)
ts2_1 = output_all1[rowSums(is.na(output_all1[,c(1,2,4)]) )<1,c(1,2,4)]
ts2_1=unique(ts2_1)
par(mar=c(1,1.0,1.1,1.1),mgp=c(2,0.5,0),family = "serif")
par(fig=c(4.5,10,7.56,9.99)/10,new=TRUE)
plot(c(1:nrow(ts2)),ts2$Gauge_Q,font=1,type='l',col='black',tck=0.02,lwd=2,cex.lab=size,cex.axis=size,yaxt="n",
     ylab='',
     ylim=c(min(output_all$Gauge_Q,na.rm=T),max(output_all$Gauge_Q,na.rm=T)*1.1))
legend('topright',c('Gauge','BAM','Rating Curve'),col=c('black','red','blue'),lty=1,bty='n',lwd=c(2,1,1),cex=size)
lines(c(1:nrow(ts2)),ts2$ST,col='red')
lines(c(1:nrow(ts2_1)),ts2_1$ST,col='blue')


text(x=1,y=max(output_all$Gauge_Q,na.rm=T)*1.07,labels='b',font=2,cex=1.5*size)
ts3 = output_all[rowSums(is.na(output_all[,c(1,2,5)]) )<1,c(1,2,5)]
ts3=unique(ts3)
ts3_1 = output_all1[rowSums(is.na(output_all1[,c(1,2,5)]) )<1,c(1,2,5)]
ts3_1=unique(ts3_1)
par(fig=c(0,10,5.13,7.56)/10,new=TRUE)  #x1,x2,y1,y2
par(mar=c(1,4,1.1,1.1),mgp=c(2,0.5,0),family = "serif")
plot(c(1:nrow(ts3)),ts3$Gauge_Q,font=1,type='l',col='black',tck=0.02,lwd=2,cex.lab=size,cex.axis=size,
     ylab='',
     ylim=c(min(output_all$Gauge_Q,na.rm=T),max(output_all$Gauge_Q,na.rm=T)*1.1))
lines(c(1:nrow(ts3)),ts3$PL,col='red')
lines(c(1:nrow(ts3_1)),ts3_1$PL,col='blue')
text(x=0.5,y=max(output_all$Gauge_Q,na.rm=T)*1.07,labels='c',font=2,cex=1.5*size)

ts4 = output_all[rowSums(is.na(output_all[,c(1,2,6)]) )<1,c(1,2,6)]
ts4 = ts4[!duplicated(ts4[,1]),]
ts4_1 = output_all1[rowSums(is.na(output_all1[,c(1,2,6)]) )<1,c(1,2,6)]
ts4_1 = ts4_1[!duplicated(ts4_1[,1]),]
par(fig=c(0,4.5,2.7,5.13)/10,new=TRUE)  #x1,x2,y1,y2
par(mar=c(1,4,1.1,0),mgp=c(2,0.5,0),family = "serif")
plot(c(1:nrow(ts4)),ts4$Gauge_Q,font=1,type='l',col='black',tck=0.02,lwd=2,cex.lab=size,cex.axis=size,
     ylab='',
     ylim=c(min(output_all$Gauge_Q,na.rm=T),max(output_all$Gauge_Q,na.rm=T)*1.1))
lines(c(1:nrow(ts4)),ts4$'LS-ST',col='red')
lines(c(1:nrow(ts4_1)),ts4_1$'LS-ST',col='blue')
text(x=1.5,y=max(output_all$Gauge_Q,na.rm=T)*1.07,labels='d',font=2,cex=1.5*size)

ts5 = output_all[rowSums(is.na(output_all[,c(1,2,7)]) )<1,c(1,2,7)]
ts5 = ts5[!duplicated(ts5[,1]),]
ts5_1 = output_all1[rowSums(is.na(output_all1[,c(1,2,7)]) )<1,c(1,2,7)]
ts5_1 = ts5_1[!duplicated(ts5_1[,1]),]
par(fig=c(4.5,10,2.7,5.13)/10,new=TRUE)  #x1,x2,y1,y2
par(mar=c(1,1.0,1.1,1.1),mgp=c(2,0.5,0),family = "serif")
plot(c(1:nrow(ts5)),ts5$Gauge_Q,font=1,type='l',col='black',tck=0.02,lwd=2,cex.lab=size,cex.axis=size,
     ylab='',yaxt="n",
     ylim=c(min(output_all$Gauge_Q,na.rm=T),max(output_all$Gauge_Q,na.rm=T)*1.1))
lines(c(1:nrow(ts5)),ts5$'ST-PL',col='red')
lines(c(1:nrow(ts5_1)),ts5_1$'ST-PL',col='blue')
text(x=0.5,y=max(output_all$Gauge_Q,na.rm=T)*1.07,labels='e',font=2,cex=1.5*size)

ts6 = output_all[rowSums(is.na(output_all[,c(1,2,8)]) )<1,c(1,2,8)]
ts6 = ts6[!duplicated(ts6[,1]),]
ts6_1 = output_all1[rowSums(is.na(output_all1[,c(1,2,8)]) )<1,c(1,2,8)]
ts6_1 = ts6_1[!duplicated(ts6_1[,1]),]
par(fig=c(0,10,0,2.7)/10,new=TRUE)  #x1,x2,y1,y2
par(mar=c(3,4,1.1,1.1),mgp=c(2,0.5,0),family = "serif")
plot(c(1:nrow(ts6)),ts6$Gauge_Q,font=1,type='l',col='black',tck=0.02,xlab='Time Step',lwd=2,cex.lab=size,cex.axis=size,
     ylab='',font.lab=1,
     ylim=c(min(output_all$Gauge_Q,na.rm=T),max(output_all$Gauge_Q,na.rm=T)*1.1))
lines(c(1:nrow(ts6)),ts6$comb,col='red')
lines(c(1:nrow(ts6_1)),ts6_1$comb,col='blue')
text(x=0.2,y=max(output_all$Gauge_Q,na.rm=T)*1.07,labels='f',font=2,cex=1.5*size)

mtext(expression(paste('Discharge (m'^3,'/s)',sep='')), side = 2, outer = TRUE, line = -0.5,cex=0.8*size)
#mtext('text is here', side=1, line=3.5, at=9)
#dev.off()