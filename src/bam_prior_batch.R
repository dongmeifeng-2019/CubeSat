Qc_bound_list=c(0.03,0.1,0.5,1.0)  #  0.10, 0.50 1.00, original:0.03
werr_sd_list = c(30,60, 100,200)
b_sd_list = c(0.1,0.5,1.0)
werr_sd = 30    # meter, 30, 100, 200  original 40
b_sd = 0.3      # channel morphology, 0.1, 0.5, 1.0   original 0.3
Qc_bound = 0.1  #0.03 
t = c(seq(as.Date("2016.05.14","%Y.%m.%d" ), as.Date("2016.09.15","%Y.%m.%d" ),'days'),
      seq(as.Date("2017.05.14","%Y.%m.%d" ), as.Date("2017.09.15","%Y.%m.%d" ),'days'),
      seq(as.Date("2018.05.14","%Y.%m.%d" ), as.Date("2018.09.15","%Y.%m.%d" ),'days'))
if(priorID==1){
  gauge_folder='data/Priors/USGS/'
  gauge_in=read_delim(paste(gauge_folder,reachID,'.txt',sep=""),delim=" ") %>%
    transmute(gauge_Q= as.integer(q_cfs)*(.3048^3), Date= time  )
  gauge_in = data.frame(Date=t)%>%
    left_join(gauge_in,by='Date')
  logQc_hat = log(mean(gauge_in$gauge_Q,na.rm=TRUE)/(sqrt(1+ var(gauge_in$gauge_Q,na.rm=TRUE)/(mean(gauge_in$gauge_Q,na.rm=TRUE)^2))))
  logQc_sd = log(1  + var(gauge_in$gauge_Q,na.rm=TRUE)/ (mean(gauge_in$gauge_Q,na.rm=TRUE))^2  )
  lowerbound_logQ =  log(min(gauge_in$gauge_Q,na.rm=TRUE))  #logQc_hat-0.1 #
  upperbound_logQ = log(max(gauge_in$gauge_Q,na.rm=TRUE))  #logQc_hat #
}
if(priorID==2){
  ncep_folder='data/Priors/NCEP/'
  ncep_in=read_delim(paste(ncep_folder,reachID,'.txt',sep=""),delim=" ") %>%
    transmute(ncep_Q= as.integer(q_cfs)*(.3048^3), Date= time  ) 
  ncep_in[ncep_in==0]=NA
  ncep_in = data.frame(Date=t)%>%
    left_join(ncep_in,by='Date')
  logQc_hat = log(mean(ncep_in$ncep_Q,na.rm=TRUE)/(sqrt(1+ var(ncep_in$ncep_Q,na.rm=TRUE)/(mean(ncep_in$ncep_Q,na.rm=TRUE)^2))))
  logQc_sd = log(1  + var(ncep_in$ncep_Q,na.rm=TRUE)/ (mean(ncep_in$ncep_Q,na.rm=TRUE))^2  )
  lowerbound_logQ =  log(min(ncep_in$ncep_Q,na.rm=TRUE))  #logQc_hat-0.1 #
  upperbound_logQ = log(max(ncep_in$ncep_Q,na.rm=TRUE))  #logQc_hat #
}
if(priorID==3){
  # need update here
  ncep_in=read_delim(paste(ncep_folder,reachID,'.txt',sep=""),delim=" ") %>%
    transmute(ncep_Q= as.integer(q_cfs)*(.3048^3), Date= time  ) 
  
  logQc_hat = log(mean(ncep_in$ncep_Q,na.rm=TRUE)/(sqrt(1+ var(ncep_in$ncep_Q,na.rm=TRUE)/(mean(ncep_in$ncep_Q,na.rm=TRUE)^2))))
  logQc_sd = log(1  + var(ncep_in$ncep_Q,na.rm=TRUE)/ (mean(ncep_in$ncep_Q,na.rm=TRUE))^2  )
  
}


lowerbound_logQc = logQc_hat - Qc_bound*logQc_hat   #log(min(gauge_in$gauge_Q,na.rm=TRUE))   
upperbound_logQc= logQc_hat + Qc_bound*logQc_hat    #log(max(gauge_in$gauge_Q,na.rm=TRUE))    
