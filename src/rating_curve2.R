#bamdates= as.Date(substr(colnames(width_DF2),2,nchar(colnames(width_DF2))  ) ,"%Y.%m.%d"  )

#load('data/Widths/Planet/width_15356000.Rdata')

library(lubridate)
width_DF2 = width_samp
width_date=data.frame(Date=bamdates)%>%
  mutate(flag=1)

Q=width_date%>%
  left_join(gauge_in,by="Date") %>%
  filter(flag==1) %>%
  select(-flag)

logq_matrix= log(matrix(t(Q$gauge_Q),nrow=nrow(width_DF2),ncol=nrow(Q),byrow=TRUE))

logq_matrix[logq_matrix==-Inf]=NA

logwidth_DF=as.matrix(log(width_DF2) )

logwidth_DF[logwidth_DF==-Inf] =NA

#logwidth_DF=logwidth_DF[2:301,]

if(all(is.na(Q$gauge_Q))){
  AHG = matrix(NA,nrow=nrow(width_DF2),ncol=2)
}else{
  AHG=t(apply(logwidth_DF, 1, function(x) lm(x ~ (as.matrix(logq_matrix[1,])), na.action = na.exclude)$coefficients  ))
  
  AHG[AHG[,2]<0,2]=0.1
  AHG[AHG[,2]>1,2]=0.999
  AHG[is.na(AHG[,2]),2]=mean(AHG[,2],na.rm=T)
}


# Q_inv=exp(apply(logwidth_DF, 2, function (x) (x-AHG[,1]) /AHG[,2]  ))
# 
# plot(apply(Q_inv,2, function(x) median(x,na.rm=TRUE)),ylim=c(0,10000))
# 
# lines(exp(logq_matrix[1,]), lwd=3)
# 
# residuals=apply(logwidth_DF, 1, function(x) lm(x ~ (as.matrix(logq_matrix[1,])), na.action = na.exclude)$residuals)
# 
# mean_resid=lapply(residuals, function(x) mean(exp(x)))
