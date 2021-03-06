library(lubridate)
setwd('D:/RiverWidth/Arcrwl/Planet')
discharge_folder='cache/discharge/'

idlist=c('15453500','15356000','15485500','15515500','15484000','15493400','15514000','15477740',
         '15493700','15511000','15519100')

width_list = c('LS','ST','PL','LS-ST', 'ST-PL','comb')  # LS: Landsat, ST: Sentinel-2, PL: Planet, comb: 3 datasets combined
width_input = width_list[3]
reachID = idlist[7]

t = c(seq(as.Date("2016.05.14","%Y.%m.%d" ), as.Date("2016.09.15","%Y.%m.%d" ),'days'),
      seq(as.Date("2017.05.14","%Y.%m.%d" ), as.Date("2017.09.15","%Y.%m.%d" ),'days'),
      seq(as.Date("2018.05.14","%Y.%m.%d" ), as.Date("2018.09.15","%Y.%m.%d" ),'days'))

for(reachID in idlist[2]){
  gauge_folder='data/Priors/USGS/'
  gauge_in=read_delim(paste(gauge_folder,reachID,'.txt',sep=""),delim=" ") %>%
    transmute(gauge_Q= as.integer(q_cfs)*(.3048^3), Date= time  )
  gauge_in = data.frame(Date=t)%>%
    left_join(gauge_in,by='Date')
  for(width_input in width_list){
    if((reachID%in%idlist[1:4]&width_input%in%width_list[c(1,4,6)])|
       (reachID%in%idlist[1:9]&width_input%in%width_list[c(2,5)])|
       (reachID%in%idlist&width_input%in%width_list[3])
    ){
      if(width_input == 'PL'){
        width_folder='data/Widths/Planet/'
        width_in=readRDS(paste(width_folder,'width_',reachID,'.rds',sep=""))  #for planet
        #width_in = width_in[, colSums(width_in != 0) > 0]
        width_in[width_in==0]=NA
        width_DF2= width_in[, colSums(is.na(width_in) ) < nrow(width_in)]
        
      }
      if(width_input == 'ST'){
        if(reachID%in%idlist[1:9]){
          width_folder='data/Widths/Sentinel-2/'
          width_in=read.table(paste(width_folder,'BAM_width_',reachID,'.txt',sep=""),header=T) #for landsat and sentinel-2
          width_in = width_in[, colSums(width_in != 0,na.rm=T) > 0]
          width_in[width_in==0]=NA
          width_DF2= width_in[, colSums(is.na(width_in) ) < nrow(width_in)]
        }
        
      }
      if(width_input == 'LS'){
        if(reachID%in%idlist[1:4]){
          width_folder='data/Widths/Landsat_SR_Zou/'
          width_in=read.table(paste(width_folder,'BAM_width_',reachID,'.txt',sep=""),header=T) #for landsat and sentinel-2
          width_in = width_in[, colSums(width_in != 0,na.rm=T) > 0]
          width_in[width_in==0]=NA
          width_DF2= width_in[, colSums(is.na(width_in) ) < nrow(width_in)]
        }
        
      }
      
      if(width_input == 'LS-ST'){
        if(reachID%in%idlist[1:4]){
          width_folder='data/Widths/Sentinel-2/'
          width_in=read.table(paste(width_folder,'BAM_width_',reachID,'.txt',sep=""),header=T) #for landsat and sentinel-2
          colnames(width_in) = paste0(colnames(width_in),'ST')
          width_in = width_in[, colSums(width_in != 0,na.rm=T) > 0]
          width_in[width_in==0]=NA
          width_DF2_st= width_in[, colSums(is.na(width_in) ) < nrow(width_in)]
          
          width_folder='data/Widths/Landsat_SR_Zou/'
          
          width_in=read.table(paste(width_folder,'BAM_width_',reachID,'.txt',sep=""),header=T) #for landsat and sentinel-2
          colnames(width_in) = paste0(colnames(width_in),'LS')
          width_in = width_in[, colSums(width_in != 0,na.rm=T) > 0]
          width_in[width_in==0]=NA
          width_DF2_ls= width_in[, colSums(is.na(width_in) ) < nrow(width_in)]
          
          comb=merge(width_DF2_ls,width_DF2_st,by=0,all=TRUE)
          rownames(comb)=comb$Row.names
          width_DF2 = comb%>%
            select(-Row.names)
        }
        
      }
      
      if(width_input == 'ST-PL'){
        links = read.table(paste('data/Widths/',reachID,'_links.txt',sep=''),header=T)
        
        width_folder='data/Widths/Planet/'
        width_in=readRDS(paste(width_folder,'width_',reachID,'.rds',sep=""))  #for planet
        colnames(width_in) = paste0(colnames(width_in),'PL')
        width_in[width_in==0]=NA
        width_DF2_pl= width_in[, colSums(is.na(width_in) ) < nrow(width_in)]
        rownames(width_DF2_pl)=links$ID
        #width_DF2_pl = width_DF2_pl[order(rownames(width_DF2_pl)),]
        
        width_folder='data/Widths/Sentinel-2/'
        width_in=read.table(paste(width_folder,'BAM_width_',reachID,'.txt',sep=""),header=T) #for landsat and sentinel-2
        colnames(width_in) = paste0(colnames(width_in),'ST')
        width_in = width_in[, colSums(width_in != 0,na.rm=T) > 0]
        width_in[width_in==0]=NA
        width_DF2_st= width_in[, colSums(is.na(width_in) ) < nrow(width_in)]
        
        comb=merge(width_DF2_st,width_DF2_pl,by=0)
        
        rownames(comb)=comb$Row.names
        comb = comb%>%
          select(-Row.names)
        
        width_DF2 = comb
        
      }
      
      if(width_input == 'comb'){
        if(reachID%in%idlist[1:4]){
          links = read.table(paste('data/Widths/',reachID,'_links.txt',sep=''),header=T)
          
          width_folder='data/Widths/Planet/'
          width_in=readRDS(paste(width_folder,'width_',reachID,'.rds',sep=""))  #for planet
          colnames(width_in) = paste0(colnames(width_in),'PL')
          width_in[width_in==0]=NA
          width_DF2_pl= width_in[, colSums(is.na(width_in) ) < nrow(width_in)]
          rownames(width_DF2_pl)=links$ID
          #width_DF2_pl = width_DF2_pl[order(rownames(width_DF2_pl)),]
          
          width_folder='data/Widths/Sentinel-2/'
          width_in=read.table(paste(width_folder,'BAM_width_',reachID,'.txt',sep=""),header=T) #for landsat and sentinel-2
          colnames(width_in) = paste0(colnames(width_in),'ST')
          width_in = width_in[, colSums(width_in != 0,na.rm=T) > 0]
          width_in[width_in==0]=NA
          width_DF2_st= width_in[, colSums(is.na(width_in) ) < nrow(width_in)]
          
          width_folder='data/Widths/Landsat_SR_Zou/'
          
          width_in=read.table(paste(width_folder,'BAM_width_',reachID,'.txt',sep=""),header=T) #for landsat and sentinel-2
          colnames(width_in) = paste0(colnames(width_in),'LS')
          width_in = width_in[, colSums(width_in != 0,na.rm=T) > 0]
          width_in[width_in==0]=NA
          width_DF2_ls= width_in[, colSums(is.na(width_in) ) < nrow(width_in)]
          
          
          comb=merge(width_DF2_ls,width_DF2_pl,by=0)
          comb=merge(width_DF2_ls,width_DF2_st,by=0,all=TRUE)
          rownames(comb)=comb$Row.names
          comb = comb%>%
            select(-Row.names)
          
          width_DF2 = merge(comb,width_DF2_pl,by=0)
          width_DF2=width_DF2[order(as.numeric(width_DF2$Row.names)),]%>%
            select(-Row.names)
        }
      }
      
      width_samp=width_DF2
      width_samp = width_samp[rowSums(is.na(width_samp)) <ncol(width_samp),]
      width_samp= width_samp[, colSums(is.na(width_samp) ) < nrow(width_samp)]
      bamdates= as.Date(substr(colnames(width_samp),2,nchar(colnames(width_samp))  ) ,"%Y.%m.%d"  )
      
      width_samp=width_samp%>%
        unname()%>%
        as.matrix()
      
      width_date=data.frame(Date=bamdates)%>%
        mutate(flag=1)
      
      Q=width_date%>%
        left_join(gauge_in,by="Date") %>%
        filter(flag==1) %>%
        select(-flag)
      
      logq_matrix= log(matrix(t(Q$gauge_Q),nrow=nrow(width_samp),ncol=nrow(Q),byrow=TRUE))
      
      logq_matrix[logq_matrix==-Inf]=NA
      
      logwidth_DF=as.matrix(log(width_samp) )
      
      logwidth_DF[logwidth_DF==-Inf] =NA
      
      #logwidth_DF=logwidth_DF[2:301,]
      
      if(all(is.na(Q$gauge_Q))){
        AHG = matrix(NA,nrow=nrow(logwidth_DF),ncol=2)
        pval = array(NA,dim=c(1,nrow(logwidth_DF)))
      }else{
        AHG=t(apply(logwidth_DF, 1, function(x) lm(x ~ (as.matrix(logq_matrix[1,])), na.action = na.exclude)$coefficients  ))
        #AHG[AHG[,2]<0,2]=0.1
        #AHG[AHG[,2]>1,2]=0.999
        pval=t(apply(logwidth_DF, 1, function(x) 
          summary(lm(x ~ (as.matrix(logq_matrix[1,])), na.action = na.exclude))$coefficients[,4][2]))
      }
      
      
      Q_inv=exp(apply(logwidth_DF, 2, function (x) (x-AHG[,1]) /AHG[,2]  ))
      q_new=Q_inv[order(pval),]
      
      #plot(apply(q_new[1:20,],2, function(x) median(x,na.rm=TRUE)))
      #plot(median(Q_inv[1:20,],na.rm=TRUE))
      #lines(exp(logq_matrix[1,]), lwd=3)
      out = data.frame(Date=bamdates,rc_Q=apply(q_new[1:20,],2, function(x) median(x,na.rm=TRUE)),
                       gauge_Q=exp(logq_matrix[1,]))
      out = out[order(out$Date),]
      #write.table(out,paste(discharge_folder,reachID,'_',width_input,'.txt',sep=''),
      #            col.names = T,row.names = F)
      plot(out$Date, out$rc_Q)
      lines(out$Date, out$gauge_Q)
      width_new=width_samp[order(pval),]
      colnames(width_new)=as.character(bamdates)
      out_width = width_new[1:20,]
      #write.table(out_width,paste(discharge_folder,'width_',reachID,'_',width_input,'.txt',sep=''),
        #          col.names = T,row.names = F)
      
      bhat = AHG[order(pval),][1:20,2]
      #write.table(bhat,paste(discharge_folder,'bhat_',reachID,'_',width_input,'.txt',sep=''),
         #         col.names = F,row.names = F)
      
      out_pval=pval[order(pval)][1:20]
      #write.table(t(AHG),'D:/RiverWidth/paper1/figure8.txt',col.names = F,row.names = F,append = T)
      #write.table(out_pval,paste(discharge_folder,'pval_',reachID,'_',width_input,'.txt',sep=''),
                 # col.names = F,row.names = F)
      #residuals=apply(logwidth_DF, 1, function(x) lm(x ~ (as.matrix(logq_matrix[1,])), na.action = na.exclude)$residuals)
      # mean_resid=lapply(residuals, function(x) mean(exp(x)))
    }
  }
  
}
    
    