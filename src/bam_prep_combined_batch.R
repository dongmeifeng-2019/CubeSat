links = read.table(paste('data/Widths/',reachID,'_links.txt',sep=''),header=T)

width_folder='data/Widths/Planet/'
width_in=readRDS(paste(width_folder,'width_',reachID,'.rds',sep=""))  #for planet
width_DF2_pl = width_in
rownames(width_DF2_pl)=links$ID
#width_DF2_pl = width_DF2_pl[order(rownames(width_DF2_pl)),]

width_folder='data/Widths/Sentinel-2/'
width_in=read.table(paste(width_folder,'BAM_width_',reachID,'.txt',sep=""),header=T) #for landsat and sentinel-2
width_DF2_st = width_in[, colSums(width_in != 0) > 0]

width_folder='data/Widths/Landsat/'

width_in=read.table(paste(width_folder,'BAM_width_',reachID,'.txt',sep=""),header=T) #for landsat and sentinel-2
width_DF2_ls = width_in[, colSums(width_in != 0) > 0]


comb=merge(width_DF2_ls,width_DF2_pl,by=0)
rownames(comb)=comb$Row.names
comb = comb%>%
  select(-Row.names)

width_DF2 = merge(comb,width_DF2_st,by=0,all=TRUE)
width_DF2=width_DF2[order(as.numeric(width_DF2$Row.names)),]%>%
  select(-Row.names)
width_DF2[width_DF2==0]=NA
width_DF2= width_DF2[, colSums(is.na(width_DF2) ) < nrow(width_DF2)]

rs <- colSums(is.na(width_DF2))
width_DF2 = width_DF2[,order(rs)]
bamdates= as.Date(substr(colnames(width_DF2),2,nchar(colnames(width_DF2))  ) ,"%Y.%m.%d"  )
width_DF2 <- width_DF2[, !duplicated(bamdates)]  #remove duplicated columns, keep first ones
bamdates <- bamdates[!duplicated(bamdates)]
#generate bam formatted data and priors
most_var_xs=function(n){
  width_DF3 = transform(width_DF2, SD=apply(width_DF2,1, sd, na.rm = TRUE))
  width_DF4=width_DF3[order(-width_DF3$SD),]%>%
    select(-SD)
  return(width_DF4[1:n,])
  
}

width_samp=xs_samp%>%map(most_var_xs)%>% 
  map(unname) %>% 
  map(as.matrix)%>%
  map(unname)%>%
  map(as.matrix)

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

cache("bamdatas")
cache("bampriors")
cache("bamdates")