VCOV_betahat <-
function(fit_model,dat,variables_vec){
  n=length(dat[,1])
  k=length(variables_vec)
  res<-residuals(fit_model)
  res1<-as.vector(res^2)
  c1<-rep(1,times=length(dat[,1]))
  variable<-dat[,variables_vec]
  X<-as.matrix(cbind(c1,variable))
  mtres<-matrix(0,ncol=length(dat[,1]),nrow=length(dat[,1]))
  for(i in 1:length(dat[,1])){
    mtres[i,i]<-res1[i]
  }
  
  vcov_bhat<-(n/(n-k-1))*solve(t(X)%*%X)%*%t(X)%*%mtres%*%X%*%solve(t(X)%*%X)
  return(vcov_bhat)
}
