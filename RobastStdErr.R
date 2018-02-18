RobastStdErr <-
function(fit_model,respons_y,variables_vec,dat,alpha){
  ver_bhat<-VCOV_betahat(fit_model,dat,variables_vec)
  vars<- as.vector(diag(ver_bhat))
  Robust_StdErr<-sqrt(vars) #
  
  n=length(dat[,1])
  k=length(variables_vec)
  bhat<-Beta_Hat(fit_model,respons_y,variables_vec,dat)
  t_stat<-bhat/Robust_StdErr #
  p_value<-c(0,times=length(t_stat[,1])) #
  CI_UB<-c(0,times=length(t_stat[,1]))
  CI_LB<-c(0,times=length(t_stat[,1]))
  
  for(i in 1:length(t_stat[,1])){
    p_value[i]=pt(t_stat[i],df=n-k-1,lower.tail = FALSE)
    CI_UB[i]<-bhat[i]+qt(alpha/2,df=n-k-1,lower.tail = FALSE)*Robust_StdErr[i]
    CI_LB[i]<-bhat[i]-qt(alpha/2,df=n-k-1,lower.tail = FALSE)*Robust_StdErr[i]
  }
  
  s<-as.matrix(cbind(Robust_StdErr,t_stat,p_value,CI_LB,CI_UB))
  colnames(s)[2]<-c("t_stat")
  return(s)
}
