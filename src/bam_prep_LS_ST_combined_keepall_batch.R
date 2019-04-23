most_var_xs=function(n){
  width_DF3 = transform(width_DF2, SD=apply(width_DF2,1, sd, na.rm = TRUE))
  width_DF4=width_DF3[order(-width_DF3$SD),]%>%
    select(-SD)
  return(width_DF4[1:n,])
}


width_folder='data/Widths/Sentinel-2/'
width_in=read.table(paste(width_folder,'BAM_width_',reachID,'.txt',sep=""),header=T) #for landsat and sentinel-2
colnames(width_in) = paste0(colnames(width_in),'ST')
width_in = width_in[, colSums(width_in != 0) > 0]
width_in[width_in==0]=NA
width_DF2_st= width_in[, colSums(is.na(width_in) ) < nrow(width_in)]

width_folder='data/Widths/Landsat/'

width_in=read.table(paste(width_folder,'BAM_width_',reachID,'.txt',sep=""),header=T) #for landsat and sentinel-2
colnames(width_in) = paste0(colnames(width_in),'LS')
width_in = width_in[, colSums(width_in != 0) > 0]
width_in[width_in==0]=NA
width_DF2_ls= width_in[, colSums(is.na(width_in) ) < nrow(width_in)]

comb=merge(width_DF2_ls,width_DF2_st,by=0,all=TRUE)
rownames(comb)=comb$Row.names
width_DF2 = comb%>%
  select(-Row.names)

xsIDs = rownames(width_DF2)
#width_DF5=read.table(paste('cache/bam_discharge/13/','width_',reachID,'_',width_input,'.txt',sep=''),header=T)
#width_DF5=most_var_xs(xs_samp)
width_DF2= width_DF2[, colSums(is.na(width_DF2) ) < nrow(width_DF2)]
bamdates= as.Date(substr(colnames(width_DF2),2,nchar(colnames(width_DF2))  ) ,"%Y.%m.%d"  )

##--------------boostrap start-------------##
getdates = function(df){
  return(as.Date(substr(colnames(df),2,nchar(colnames(df))  ) ,"%Y.%m.%d"  ))
}

myxs = replicate(500, width_DF2 %>% sample_n(20), simplify=F)
myxs_1=pmap(list(myxs),function(x){x[, colSums(is.na(x) ) < nrow(x)]})
bamdates = myxs_1%>%
  map(getdates)
width_samp=myxs_1%>%
  map(unname) %>% 
  map(as.matrix) 

##-----------boostrap end-----------------##

bamdatas = pmap(list(w = width_samp, Qhat = exp(logQc_hat)), 
                bam_data, max_xs = 40L)

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
  planet_priors$logQ_sd= rep(logQc_sd,ncol(y))
  planet_priors$b_hat[is.na(planet_priors$b_hat)]=mean(planet_priors$b_hat,na.rm=T)  # remove NA from priors$b_hat
  planet_priors$logA0_hat[is.na(planet_priors$logA0_hat)]=mean(planet_priors$logA0_hat,na.rm=T) # remove NA from priors$logA0_hat
  return(planet_priors)
}

bampriors = map2(bamdatas,width_samp,makeBamPriors)

# if(rcID==1){
#   source('src/rating_curve2.R')
#   b = AHG[,2]
#   if(all(is.na(b))){  #keep old setting if rating curve does not work
#     bampriors$b_sd = b_sd
#     bampriors$b_hat[is.na(bampriors$b_hat)]=mean(bampriors$b_hat,na.rm=T)  # remove NA from priors$b_hat
#   }else{
#     bampriors$b_hat=b
#     if(is.na(sd(b))){
#       bampriors$b_sd = b_sd
#     }else if(sd(b)==0){
#       bampriors$b_sd = b_sd
#     }else{
#       bampriors$b_sd = sd(b,na.rm=T)
#     }
#   }
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

#bamdates = data.frame(Date=bamdates,flag=substr(colnames(width_DF5),12,nchar(colnames(width_DF5))  ))

# cache("bamdatas")
# cache("bampriors")
# cache("bamdates")
cache("xsIDs")
