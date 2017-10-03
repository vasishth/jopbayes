plotpostpred<-function(paramnames=c("beta","sigma_e","sigma_u","sigma_w","rt_tilde"),
                       niter=4000,
                       m=NULL,
                       Dat=rDat,samp_num=10,plotmatrix=c(2,samp_num)){
mmatrix<-extract(m,pars=paramnames)
ndata<-dim(mmatrix$rt_tilde)[2]
postpreddata<-data.frame(t(mmatrix$rt_tilde))
simids<-paste("sim",1:niter,sep="")
colnames(postpreddata)<-simids
Datsim<-(cbind(Dat,postpreddata))
sample_simids<-sample(simids,samp_num)
## specific to the SR/OR data:
SR<-subset(Datsim,type=="subj-ext")
OR<-subset(Datsim,type=="obj-ext")

op<-par(mfrow=plotmatrix,pty="s")

for(s in sample_simids){
  hist(SR[[s]],main="SR",
       xlim=c(0,max(SR$rt)))
  abline(v=mean(SR$rt),col="red",lwd=2)
  abline(v=quantile(SR$rt,probs=c(0.025)),
         col="red",
         lty=2,
         lwd=2)
  abline(v=quantile(SR$rt,probs=c(0.975)),
         col="red",
         lty=2,
         lwd=2)
}

for(s in sample_simids){
  hist(OR[[s]],col="yellow",main="OR",
       xlim=c(0,max(SR$rt)))
  abline(v=mean(OR$rt),col="red",lwd=2)
  abline(v=quantile(OR$rt,probs=c(0.025)),
         col="red",lty=2,
         lwd=2)
 abline(v=quantile(OR$rt,probs=c(0.975)),
         col="red",lty=2,
         lwd=2)
}
}
