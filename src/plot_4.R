idlist=c('15493400','15511000','15493700','15477740','15356000','15514000','15453500',
         '15485500','15515500','15484000','15519100')
width_list = c('LS','ST','PL') 

most_var_xs=function(n){
  width_DF3 = transform(width_DF2, SD=apply(width_DF2,1, sd, na.rm = TRUE))
  width_DF4=width_DF3[order(-width_DF3$SD),]%>%
    select(-SD)
  return(mean(apply(width_DF4[1:n,],2,sd),na.rm=T))
  
}
xs_samp = c(2:20)
xs_sd = data.frame()
for(reachID in idlist){
  for(width_input in width_list){
    
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
    #generate bam formatted data and priors
    width_samp=xs_samp%>%map(most_var_xs)%>%
      unname()%>%
      as.matrix()
    xs_sd = rbind(xs_sd,t(width_samp))
    
  }
}
windows()
par(mfrow=c(3,4))
par(mar=c(3.4,3.6,1.1,1.1),mgp=c(2,0.8,0))
for(j in 1:11){
  mydata1 = data.frame(t(error_metrics[(3*(j-1)+1):(3*j),]))
  mydata2 = data.frame(t(xs_sd[(3*(j-1)+1):(3*j),]))
  
  colnames(mydata1) = c('Landsat_er','Sentinel_er','Planet_er')
  colnames(mydata2) = c('Landsat_sd','Sentinel_sd','Planet_sd')
  #mydata$nxs = c(2:20)
  mydata = data.frame(mydata2,mydata1)
  
  ymin = min(mydata1[,1:3],na.rm=T)
  ymax = max(mydata1[,1:3],na.rm=T)
  plot(mydata$Landsat_sd,mydata$Landsat_er,ylim=c(ymin,ymax),col='red',main=idlist[j],
       xlab='sd of width',ylab='nRMSE (%)')
  points(mydata$Sentinel_sd,mydata$Sentinel_er,col='green')
  points(mydata$Planet_sd,mydata$Planet_er,col='blue')
  #text(x=1,y=ymin,labels =idlist[j])
  if(j==1){
    legend('topleft',c('Landsat','Sentinel','Planet'),col=c('red','green','blue'),pch=c(1,1,1),bty='n')
  }
  
}
   