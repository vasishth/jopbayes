ggplotpostpred<-function(paramnames=c("beta","sigma_e","sigma_u","sigma_w","rt_tilde"),
                       niter=4000,
                       m=NULL,
                       Dat=rDat,samp_num=10,modelid="M0"){
  mmatrix<-rstan::extract(m,pars=paramnames)
  ndata<-dim(mmatrix$rt_tilde)[2]
  postpreddata<-data.frame(t(mmatrix$rt_tilde))
  simids<-paste("sim",1:niter,sep="")
  colnames(postpreddata)<-simids
  Datsim<-(cbind(Dat,postpreddata))
  sample_simids<-sample(simids,samp_num)
    ## specific to the SR/OR data:
  SR<-subset(Datsim,type=="subj-ext")
  OR<-subset(Datsim,type=="obj-ext")

  mycols<-c("rt",sample_simids)
  
  long_sr <- SR[,mycols] %>% gather(mycols, value)
  
  maintitle<-paste(modelid,": subject relatives",sep="")
  
  p_sr<-ggplot(long_sr,aes(factor(mycols), value))+
    geom_violin(alpha = 0.3)+magnifytext()+
    ggtitle(maintitle)+xlab("observed/simulated data")+
    ylab("reading time (ms)")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))#+
    #geom_jitter(shape=16, position=position_jitter(0.2))
  
  long_or <- OR[,mycols] %>% gather(mycols, value)
  
   maintitle<-paste(modelid,": object relatives",sep="")
  p_or<-ggplot(long_or,aes(factor(mycols), value))+
    geom_violin(alpha = 0.3)+magnifytext()+
    ggtitle(maintitle)+xlab("observed/simulated data")+
    ylab("reading time (ms)")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))#+
    #eom_jitter(shape=16, position=position_jitter(0.2))
  
  multiplot(p_sr,p_or,cols=2)
}    
    
