
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
#width_DF2 = width_DF2[,order(names(width_DF2))]

#bamdates= as.Date(substr(colnames(width_DF2),2,nchar(colnames(width_DF2))  ) ,"%Y.%m.%d"  )
#generate bam formatted data and priors
most_var_xs=function(n){
  width_DF3 = transform(width_DF2, SD=apply(width_DF2,1, sd, na.rm = TRUE))
  width_DF4=width_DF3[order(-width_DF3$SD),]%>%
    select(-SD)
  return(width_DF4[1:n,])

}
#read in the width with best AHG regressions
# width_samp=read.table(paste('cache/bam_discharge/13/','width_',reachID,'_',width_input,'.txt',sep=''),header=T)
# width_samp= width_samp[, colSums(is.na(width_samp) ) < nrow(width_samp)]
# bamdates= as.Date(substr(colnames(width_samp),2,nchar(colnames(width_samp))  ) ,"%Y.%m.%d"  )

#select the width at 20 most variable xs
#width_samp=most_var_xs(xs_samp)

###------------use the best randomly selected 20 xs
pos = read.table(paste('cache/bam_discharge/18/','best_batch.txt',sep=''),header=F)
file_date=paste('cache/bam_discharge/18/',reachID,'_', widthID,'_','bamdates.rdata',sep="")
dates_in=load(file_date)
bamdates = bamdates[[pos[kk,ii]]]
file_data=paste('cache/bam_discharge/18/',reachID,'_', widthID,'_','bamdatas.rdata',sep="")
width_in=load(file_data)
width_samp=bamdatas[[pos[kk,ii]]]$Wobs
###------------use the best randomly selected 20 xs

width_samp=width_samp%>%
  unname()%>%
  as.matrix()

width_samp[width_samp==0]=NA

# width_samp=most_var_xs(xs_samp)%>%
#   unname()%>%
#   as.matrix()
# width_samp=width_DF2%>%
#   unname() %>% 
#   as.matrix()

bamdatas = bam_data(w = width_samp, Qhat = exp(logQc_hat), max_xs = nrow(width_samp))

makeBamPriors = function(x,y){
  planet_priors=bam_priors(x,variant="amhg",  lowerbound_b=0.4, upperbound_b=1)
  planet_priors$logWc_hat= log(mean(as.matrix(y),na.rm=TRUE)) 
  planet_priors$logQc_sd=logQc_sd
  planet_priors$logQc_hat = logQc_hat
  planet_priors$lowerbound_logQc =lowerbound_logQc
  planet_priors$upperbound_logQc = upperbound_logQc
  planet_priors$lowerbound_logQ = lowerbound_logQ
  planet_priors$upperbound_logQ = upperbound_logQ
  planet_priors$Werr_sd= werr_sd
  planet_priors$b_sd = b_sd
  planet_priors$b_hat[is.na(planet_priors$b_hat)]=mean(planet_priors$b_hat,na.rm=T)  # remove NA from priors$b_hat
  planet_priors$logQ_sd= rep(logQc_sd,ncol(y))
  planet_priors$logA0_hat[is.na(planet_priors$logA0_hat)]=mean(planet_priors$logA0_hat,na.rm=T) # remove NA from priors$logA0_hat
  return(planet_priors)
}

bampriors = makeBamPriors(bamdatas,width_samp)

# if(rcID==1){
#    source('src/rating_curve2.R')
#    b = AHG[,2]
#    if(all(is.na(b))){  #keep old setting if rating curve does not work
#      bampriors$b_sd = b_sd
#      bampriors$b_hat[is.na(bampriors$b_hat)]=mean(bampriors$b_hat,na.rm=T)  # remove NA from priors$b_hat
#    }else{
#      bampriors$b_hat=b
#      if(is.na(sd(b))){
#        bampriors$b_sd = b_sd
#      }else if(sd(b)==0){
#        bampriors$b_sd = b_sd
#      }else{
#        bampriors$b_sd = sd(b,na.rm=T)
#      }
#    }
# }

if(rcID==1){
  b=read.table(paste('cache/bam_discharge/13/','bhat_',reachID,'_',width_input,'.txt',sep=''),header=F)
  if(all(is.na(b))){  #keep old setting if rating curve does not work
    #next()
  }else{
    b[b>1]=0.95
    b[b<0]=0.10
    b[is.na(b)]=mean(b[,1],na.rm=T)
    # print(paste('bhat_',reachID,'_',width_input,'.txt',sep=''))
    # print(t(b))
    bampriors$b_hat=b[,1]
    if(sapply(b, sd,na.rm=T)==0){
      bampriors$b_sd = b_sd
    }else{
      bampriors$b_sd = sapply(b, sd,na.rm=T)
    }
    
  }
}

#write.table(t(bampriors$b_hat),'cache/b.txt',append = T,col.names = F, row.names = F)

#cache("bamdatas")
#cache("bampriors")
#cache("bamdates")
#

