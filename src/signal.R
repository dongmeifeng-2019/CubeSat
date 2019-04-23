library('ProjectTemplate')
setwd('D:/RiverWidth/Arcrwl/Planet')
load.project()
source('munge/ThresholdingAlgo.R')
idlist=c('15493400','15511000','15493700','15477740','15356000','15514000','15453500',
         '15485500','15515500','15484000','15519100')

priorID = 2   #1: USGS, 2:NECP, 3:PR

width_list = c('LS','ST','PL')  # LS: Landsat, ST: Sentinel-2, PL: Planet
width_input = width_list[3]
reachID = idlist[1]
xs_samp=c(2:20)

if(width_input == 'PL'){
  width_folder='data/Widths/Planet/'
  width_in=readRDS(paste(width_folder,'width_',reachID,'.rds',sep=""))  #for planet
  width_DF2 = width_in
}else{
  if(width_input == 'ST'){
    width_folder='data/Widths/Sentinel-2/'
  }
  if(width_input == 'LS'){
    width_folder='data/Widths/Landsat/'
  }
  width_in=read.table(paste(width_folder,'BAM_width_',reachID,'.txt',sep=""),header=T) #for landsat and sentinel-2
  width_DF2 = width_in[, colSums(width_in != 0) > 0]
}

width_DF2=width_DF2 %>%
  filter_all(any_vars(. !=0)) 

width_DF2= width_DF2[, colSums(is.na(width_DF2) ) < nrow(width_DF2)]
width_DF2[width_DF2==0]=NA

bamdates= as.Date(substr(colnames(width_DF2),2,nchar(colnames(width_DF2))  ) ,"%Y.%m.%d"  )
gauge_folder='data/Priors/USGS/'
gauge_in=read_delim(paste(gauge_folder,reachID,'.txt',sep=""),delim=" ") %>%
  transmute(gauge_Q= as.integer(q_cfs)*(.3048^3), Date= time  )

gauge_q = data.frame(Date=bamdates)%>%
  left_join(gauge_in,by='Date')
# Run algo with lag = 30, threshold = 5, influence = 0

result = lapply(c(1:nrow(width_DF2)),function(x){ThresholdingAlgo(y=width_DF2[x,],lag=5,threshold = 3,influence=0.1)})
sig = data.frame()
for(i in 1:nrow(width_DF2)){
  sig=rbind(sig,result[[i]]$signals)
}
sig=unname(sig)
sig_q = ThresholdingAlgo(gauge_q$gauge_Q,lag=5,threshold = 3,influence=0.1)
# Plot result
par(mfrow = c(2,1),oma = c(2,2,0,0) + 0.1,mar = c(0,0,2,1) + 0.2)
plot(1:length(gauge_q$gauge_Q),gauge_q$gauge_Q,type="l",ylab="",xlab="") 

plot(t(sig[5,]),type="S",col="red",ylab="",xlab="",ylim=c(-1.5,1.5),lwd=2)
lines(sig_q$signals,type="S",col="black")

