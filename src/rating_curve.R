
#bamdates= as.Date(substr(colnames(width_DF2),2,nchar(colnames(width_DF2))  ) ,"%Y.%m.%d"  )

gauge_q = data.frame(Date=bamdates)%>%
  left_join(gauge_in,by='Date')

get_rc = function(x,y){
  
  a=lm(log(x)~log(y))
  return(c(unname(a$coefficients[2]),unname(summary(a)$coefficients[,4][2])))
}
b=array()
p_val=array()
for(i in 1:nrow(width_samp)){
  x = width_samp[i,]
  if(all(is.na(gauge_q$gauge_Q))){
    b[i]=NA
    p_val[i]=NA
    next
  }
  c=get_rc(x,gauge_q$gauge_Q)
  if(is.na(c[1])){
    
  }else{
    if(c[1]>1){
      c[1]=0.9
    }
    if(c[1]<0){
      c[1]=0.1
    }
  }
  
  b[i] = c[1]
  p_val[i]=c[2]
}
b[is.na(b)]=mean(b,na.rm=T)

