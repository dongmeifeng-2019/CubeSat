library(lubridate)
setwd('D:/RiverWidth/Arcrwl/Planet')


idlist=c('15453500','15356000','15485500','15515500','15484000','15493400','15514000','15477740',
         '15493700','15511000','15519100')

width_list = c('LS','ST','PL','LS-ST', 'ST-PL','comb')  # LS: Landsat, ST: Sentinel-2, PL: Planet, comb: 3 datasets combined
width_input = width_list[3]
reachID = idlist[7]

t = c(seq(as.Date("2016.05.14","%Y.%m.%d" ), as.Date("2016.09.15","%Y.%m.%d" ),'days'),
      seq(as.Date("2017.05.14","%Y.%m.%d" ), as.Date("2017.09.15","%Y.%m.%d" ),'days'),
      seq(as.Date("2018.05.14","%Y.%m.%d" ), as.Date("2018.09.15","%Y.%m.%d" ),'days'))


for(reachID in idlist[5]){
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
      discharge_folder1='cache/bam_discharge/01/'
      file_q=paste(discharge_folder1,reachID,'_', width_input,'.txt',sep="")
      Q_in=read_delim(file_q,delim=" ") %>%
        transmute(gauge_Q= as.integer(gauge_Q), rc_Q= as.integer(rc_Q),Date= as.Date(Date)  )
      #read.table(file_q,header = T)
      
      output = data.frame(Q_in)%>%
        select(-gauge_Q)
      
      output_all1 = output_all1%>%
        left_join(output,by='Date')
      
      ####--------------BAM-----------------------###
      discharge_folder2='cache/bam_discharge/02/'
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


colnames(output_all1)=c('Date','Gauge_Q',width_list[c(2,3,5)])
output_all1 = output_all1[order(output_all1$Date),]
colnames(output_all)=c('Date','Gauge_Q',width_list[c(2,3,5)])
output_all = output_all[order(output_all$Date),]
tiff(paste0("cache/bam_discharge/01/",reachID,"_combined.tiff"), 
     height = 10.16, width = 17.78, units = 'cm', res = 1200)
ymin=min(output_all[,2],na.rm=T)
ymax=max(output_all[,2],na.rm=T)*0.7
size = 1.5

#windows(7,4)
par(mfrow=c(4,2))
par(mar=c(1,3.5,1.1,0),mgp=c(1.6,0.5,0),family = "serif")
layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
ts1 = output_all[complete.cases(output_all[,3]),1:3]
ts1=unique(ts1)
ts1_1 = output_all1[complete.cases(output_all1[,3]),1:3]
ts1_1=unique(ts1_1)
par(fig=c(0,4,5,10)/10,new=TRUE)
plot(c(1:nrow(ts1)),ts1$Gauge_Q,font=1,type='l',col='black',tck=0.02,lwd=2,cex.lab=size,cex.axis=size,
     ylab='',
     ylim=c(ymin,ymax))
lines(c(1:nrow(ts1)),ts1$ST,col='red')
lines(c(1:nrow(ts1_1)),ts1_1$ST,col='blue')
legend(x=2,y=ymax*1.1,c('Gauge','BAM','Rating Curve'),col=c('black','red','blue'),lty=1,bty='n',lwd=c(2,1,1),cex=size)
text(x=1,ymax*0.98,labels='a',font=2,cex=1.5*size)
ts2 = output_all[rowSums(is.na(output_all[,c(1,2,4)]) )<1,c(1,2,4)]
ts2=unique(ts2)
ts2_1 = output_all1[rowSums(is.na(output_all1[,c(1,2,4)]) )<1,c(1,2,4)]
ts2_1=unique(ts2_1)
par(mar=c(1,1.0,1.1,1.1),mgp=c(1.6,0.5,0),family = "serif")
par(fig=c(4,10,5,10)/10,new=TRUE)
plot(c(1:nrow(ts2)),ts2$Gauge_Q,font=1,type='l',col='black',tck=0.02,lwd=2,cex.lab=size,cex.axis=size,
     ylab='',yaxt="n",
     ylim=c(ymin,ymax))
lines(c(1:nrow(ts2)),ts2$PL,col='red')
lines(c(1:nrow(ts2_1)),ts2_1$PL,col='blue')

text(x=1,ymax*0.98,labels='b',font=2,cex=1.5*size)
ts3 = output_all[rowSums(is.na(output_all[,c(1,2,5)]) )<1,c(1,2,5)]
ts3=unique(ts3)
ts3_1 = output_all1[rowSums(is.na(output_all1[,c(1,2,5)]) )<1,c(1,2,5)]
ts3_1=unique(ts3_1)
par(fig=c(0,10,0,5)/10,new=TRUE)  #x1,x2,y1,y2
par(mar=c(1.4,3.5,0.7,1.1),mgp=c(1.6,0.5,0),family = "serif")
plot(c(1:nrow(ts3)),ts3$Gauge_Q,font=1,type='l',col='black',tck=0.02,lwd=2,cex.lab=size,cex.axis=size,
     ylab='',
     ylim=c(ymin,ymax))
lines(c(1:nrow(ts3)),ts3$'ST-PL',col='red')
lines(c(1:nrow(ts3_1)),ts3_1$'ST-PL',col='blue')
text(x=0.1,ymax*0.98,labels='c',font=2,cex=1.5*size)
dev.off()
