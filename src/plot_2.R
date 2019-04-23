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
width_input = c('LS','ST','PL') 


error_metrics = data.frame()
for(reachID in idlist){
  gauge_in=read_delim(paste(gauge_folder,reachID,'.txt',sep=""),delim=" ") %>%
    transmute(gauge_Q= as.integer(q_cfs)*(.3048^3), Date= time  ) 
  
  for(widthID in width_input){
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
    
    getnrmse = function(n){
      a = metrics(output$Gauge_Q,output[,n+2])
      return(a[3])
    }
    metrics_out = data.frame(lapply(c(2:20),getnrmse))%>%
      unname()%>%
      as.matrix()
    
    error_metrics = rbind(error_metrics,metrics_out[1,])
  }
  
}

myplots <- list()  # new empty list
i = 1
for(reachID in idlist){
  local({
    j <- i
    mydata = data.frame(t(error_metrics[(3*(j-1)+1):(3*j),]))
    colnames(mydata) = c('Landsat','Sentinel','Planet')
    mydata$nxs = c(2:20)
    nm = reachID
    ymin = min(mydata[,1:3],na.rm=T)
    ymax = max(mydata[,1:3],na.rm=T)
    pl <- ggplot() +
      geom_line(aes(x=mydata$nxs,y = mydata$Landsat, colour = 'Landsat')) +
      geom_line(aes(x=mydata$nxs,y = mydata$Sentinel, colour = 'Sentinel-2')) +
      geom_line(aes(x=mydata$nxs,y = mydata$Planet, colour = 'Planet')) +
      ylim(ymin,ymax) +
      xlab('number of xs')+ylab("nRMSE (%)")
    if(j==11){
      pl = pl  + theme_bw()+ labs(subtitle=paste(nm))+
        guides(col=guide_legend(nrow=1))+theme(legend.position=c(0.5,0.5))+
        theme(legend.title = element_blank())
    }else{
      pl = pl  + theme_bw()+labs(subtitle=nm)+theme(legend.position='none')
    }
    
    myplots[[j]] <<- pl  # add each plot into plot list
  })
  i = i + 1
}

windows(9,6)
  
grid.arrange(myplots[[7]],myplots[[5]],myplots[[9]],myplots[[8]],myplots[[10]],myplots[[1]],
             myplots[[6]],myplots[[3]],myplots[[4]],myplots[[2]],myplots[[11]],
             #tableGrob(e_tb,theme=myt),
             ncol = 4,
             #widths = c(1.5, 1.5,1.5,1.5),
             clip = FALSE
)


