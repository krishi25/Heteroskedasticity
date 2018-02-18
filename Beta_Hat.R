Beta_Hat <-
function(fit_model,respons_y,variables_vec,dat){
  r<-residuals(fit_model)    
  s<-median(abs(r-median(r)))/.6745
  rstar<-(r/s)
  n=length(dat[,1])
  c1<-rep(1,times=length(dat[,1]))
  X=as.matrix(cbind(c1,dat[,variables_vec]))
  psiBS<-rstar*(1-(rstar/4.685)^2)^2
  w<-psiBS/rstar
  diagmat<-matrix(0,ncol=n,nrow=n)
  for(i in 1:n){
    diagmat[i,i]<-w[i]
  }
  w1<-diagmat
  y<-dat[,respons_y]
  bhat<-solve(t(X)%*%w1%*%X)%*%t(X)%*%(w1)%*%as.matrix(y)
  yhat<-X%*%bhat
  par(mfrow=c(1,length(variables_vec)))
  for(j in 1:length(variables_vec)){
    plot(dat[,variables_vec[j]],y,xlab=paste('x',j),ylab=("y"))
    sum=(sum(bhat)-bhat[1,])
    abline(bhat[1,],sum,col=2)
  }
  colnames(bhat)=c("Est.Beta")
  return(bhat)
}
