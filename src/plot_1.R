rm(list=ls())
library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(gtable)
library(grid)
library("cowplot")

discharge_folder='cache/BAM_Discharge/'
gauge_folder='data/Priors/USGS/'
source("munge/metrics.R")
idlist=c('15493400','15511000','15493700','15477740','15356000','15514000','15453500',
         '15485500','15515500','15484000','15519100')
width_input = c('LS','ST','PL','Comb') 
myplots <- list()  # new empty list
widthID = width_input[4]
#idlist=c('15493400')
i = 1
k=2
error_metrics = data.frame()
for(reachID in idlist){
  gauge_in=read_delim(paste(gauge_folder,reachID,'.txt',sep=""),delim=" ") %>%
    transmute(gauge_Q= as.integer(q_cfs)*(.3048^3), Date= time  ) 
  file_date=paste(discharge_folder,reachID,'_', widthID,'_','bamdates.rdata',sep="")
  dates_in=load(file_date)
  date_df=data.frame(Date=bamdates,time=seq(1:length(bamdates)))
  output = date_df%>%
    left_join(gauge_in,by="Date")
  
  for(k in 2:20){
    file_q=paste(discharge_folder,reachID,'_', widthID,'_',k,'.rds',sep="")
    Q_in=bam_hydrograph(readRDS(file_q))
    
    bam_out_df=(Q_in$data) %>%
      left_join(date_df,by="time") 
    
    bam_mean=filter(bam_out_df, stat == "mean") %>%
      select(-Date) %>%
      spread(series,flow) %>%
      transmute(time,bam_Q=rowMeans(.[,3:5]))
    
    output = cbind(output,bam_mean$bam_Q)
  }
  colnames(output)=c('Date','time', 'Gauge_Q',paste0('Bam_Q_',c(2:20)))
  
  local({
    j <- i
    mydata = output
    
    nm = reachID
    ymin = min(mydata[,3:22],na.rm=T)
    ymax = max(mydata[,3:22],na.rm=T)
    pl <- ggplot() + 
      geom_point(aes(mydata$time,y = mydata$Gauge_Q, colour = 'Gauge' )) + 
      
      geom_line(aes(mydata$time,y = mydata$Bam_Q_2, colour = '2')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_3, colour = '3')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_4, colour = '4')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_5, colour = '5')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_6, colour = '6')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_7, colour = '7')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_8, colour = '8')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_9, colour = '9')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_10, colour = '10')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_11, colour = '11')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_12, colour = '12')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_13, colour = '13')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_14, colour = '14')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_15, colour = '15')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_16, colour = '16')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_17, colour = '17')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_18, colour = '18')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_19, colour = '19')) +
      geom_line(aes(mydata$time,y = mydata$Bam_Q_20, colour = '20')) +
      ylim(ymin,ymax) +
      xlab('Time')+ylab("Discharge (cms)")
    if(j==11){
      pl = pl  + theme_bw()+ labs(subtitle=paste(nm,widthID))+
        guides(col=guide_legend(nrow=5))+theme(legend.position=c(0.7,0.7))+
        theme(legend.title = element_blank())
    }else{
      pl = pl  + theme_bw()+labs(subtitle=nm)+theme(legend.position='none')
    }
    
    myplots[[j]] <<- pl  # add each plot into plot list
  })
  #output=na.omit(output)
  #ts = as.data.frame(cbind(output$Gauge_Q,output$Bam_Q_LS))
  getnrmse = function(n){
    a = metrics(output$Gauge_Q,output[,n+2])
    return(c(n,a[3]))
  }
  metrics_out = data.frame(lapply(c(2:20),getnrmse))
  #print(metrics_out)
  out = metrics_out[,which.min(metrics_out[2,])]
  error_metrics = rbind(error_metrics,out)
  i = i + 1
}

cnm = c('best n','n_rmse (%)')
e_tb = data.frame()
e_tb[1:11,1] = format(round(error_metrics[,1],1),nsmall=0)
e_tb[1:11,2] = format(round(error_metrics[,2],1),nsmall=1)
colnames(e_tb) = cnm
rownames(e_tb) = idlist
#dev.off()
myt <- ttheme_default(
  # Use hjust and x to left justify the text
  # Alternate the row fill colours
  core = list(fg_params=list(hjust = 1, x=0.75),
              bg_params=list(fill=c("grey95", "grey100"), col="black")),
  
  # Change column header to white text and red background
  #colhead = list(fg_params=list(col="white"),
  # bg_params=list(fill="red")),
  colhead = list(fg_params=list(fontface="bold",col='black'),
                 bg_params=list(fill=rep("grey80",3), col="black")),
  # Change row header to white text and red background
  rowhead = list(fg_params=list(fontface="plain"),
                 bg_params=list(fill=rep(NA,9)))
)
windows(9,6)

grid.arrange(myplots[[7]],myplots[[5]],myplots[[9]],myplots[[8]],myplots[[10]],myplots[[1]],
             myplots[[6]],myplots[[3]],myplots[[4]],myplots[[2]],myplots[[11]],
             tableGrob(e_tb,theme=myt),
             ncol = 4,
             widths = c(1.5, 1.5,1.5,1.5),
             clip = FALSE
)


