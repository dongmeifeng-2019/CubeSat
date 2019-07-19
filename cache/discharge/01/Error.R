rm(list=ls())
library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(gtable)
library(grid)
library("cowplot")

discharge_folder='D:/RiverWidth/Arcrwl/Planet/cache/discharge/01/'  #8 for USGS prior and 9 for NCEP prior
gauge_folder='D:/RiverWidth/Arcrwl/Planet/data/Priors/USGS/'
source('D:/RiverWidth/Arcrwl/Planet/munge/metrics.r')

idlist=c('15453500','15356000','15485500','15515500','15484000','15493400','15514000','15477740',
         '15493700','15511000','15519100')

t = c(seq(as.Date("2016.05.14","%Y.%m.%d" ), as.Date("2016.09.15","%Y.%m.%d" ),'days'),
      seq(as.Date("2017.05.14","%Y.%m.%d" ), as.Date("2017.09.15","%Y.%m.%d" ),'days'),
      seq(as.Date("2018.05.14","%Y.%m.%d" ), as.Date("2018.09.15","%Y.%m.%d" ),'days'))
width_list = c('LS','ST','PL','LS-ST', 'ST-PL','comb')
myplots <- list()  # new empty list
width_input = width_list[3]
reachID=idlist[10]

i = 1
error_metrics = data.frame()
for(reachID in idlist){
  
  output_all = data.frame()
  gauge_in=read_delim(paste(gauge_folder,reachID,'.txt',sep=""),delim=" ") %>%
    transmute(gauge_Q= as.integer(q_cfs)*(.3048^3), Date= time  )
  
  j=1
  
  for(width_input in width_list){
    if((reachID%in%idlist[1:4]&width_input%in%width_list[c(1,4,6)])|
       (reachID%in%idlist[1:9]&width_input%in%width_list[2])|
       (reachID%in%idlist&width_input%in%width_list[3])|
       (reachID%in%idlist[1:9]&width_input%in%width_list[5])
    ){
      output_all = gauge_in[gauge_in$Date%in%t,]
      file_q=paste(discharge_folder,reachID,'_', width_input,'.txt',sep="")
      Q_in=read_delim(file_q,delim=" ") %>%
        transmute(gauge_Q= as.integer(gauge_Q), rc_Q= as.integer(rc_Q),Date= as.Date(Date)  )
      #read.table(file_q,header = T)
      
      output = data.frame(Q_in)%>%
        select(-gauge_Q)
      
      output_all = output_all%>%
        left_join(output,by='Date')
      
      colnames(output_all)=c('gauge_Q','Date','BAM_Q')
      
      out=metrics(output_all$gauge_Q,output_all$BAM_Q)[5]
      
    }else{
      out=NA
    }
    error_metrics[i,j] = out
    j= j + 1
  }
  i = i + 1
}
colnames(error_metrics)=c('Landsat','Sentinel-2','Planet','LS-ST', 'ST-PL','LS-ST-PL')
rownames(error_metrics)=idlist
write.table(error_metrics,'D:/RiverWidth/Arcrwl/Planet/cache/discharge/01/NSE.txt',
            col.names = T,row.names = T)


