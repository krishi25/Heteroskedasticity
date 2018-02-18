WaldStats <-
function(fit_model,dat,variables_vec,response_y,R){
  
  V<-VCOV_betahat(fit_model,dat,variables_vec)
  bhat<-Beta_Hat(fit_model,response_y,variables_vec,dat)
  Wald<-t(R%*%bhat)%*%solve(R%*%V%*%t(R))%*%(R%*%bhat)/nrow(R)
  p_value<-pf(Wald, df1=nrow(R), df2=length(dat[,1])-length(variables_vec)-1,lower.tail =FALSE)
  if(p_value<2.2e-16){
    p_value<-c("<2.2e-16")
  }
  s<-cbind(Wald,p_value)
  colnames(s)=c("wald","p-value")
  rownames(s)=c(" ")
  return(s)
}
