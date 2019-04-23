
if(width_input == 'PL'){
  links = read.table(paste('data/Widths/',reachID,'_links.txt',sep=''),header=T)
  width_folder='data/Widths/Planet/'
  width_in=readRDS(paste(width_folder,'width_',reachID,'.rds',sep=""))  #for planet
  width_DF2 = width_in
  rownames(width_DF2)=links$ID
  xsIDs = rownames(width_DF2)
  
}else{
  if(width_input == 'ST'){
    width_folder='data/Widths/Sentinel-2/'
  }
  if(width_input == 'LS'){
    width_folder='data/Widths/Landsat/'
  }
  width_in=read.table(paste(width_folder,'BAM_width_',reachID,'.txt',sep=""),header=T) #for landsat and sentinel-2
  width_DF2 = width_in[, colSums(width_in != 0) > 0]
  xsIDs = rownames(width_DF2[rowSums(width_DF2==0)<ncol(width_DF2),])
}

width_DF2=width_DF2 %>%
  filter_all(any_vars(. !=0)) 

width_DF2= width_DF2[, colSums(is.na(width_DF2) ) < nrow(width_DF2)]
width_DF2[width_DF2==0]=NA

bamdates= as.Date(substr(colnames(width_DF2),2,nchar(colnames(width_DF2))  ) ,"%Y.%m.%d"  )
#generate bam formatted data and priors
most_var_xs=function(n){
  width_DF3 = transform(width_DF2, SD=apply(width_DF2,1, sd, na.rm = TRUE))
  width_DF4=width_DF3[order(-width_DF3$SD),]%>%
    select(-SD)
  return(width_DF4[1:n,])
  
}
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

# width_samp=xs_samp%>%map(most_var_xs)%>% 
#   map(unname) %>% 
#   map(as.matrix)%>%
#   map(unname)%>%
#   map(as.matrix)

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

# cache("bamdatas")
# cache("bampriors")
# cache("bamdates")
cache("xsIDs")

