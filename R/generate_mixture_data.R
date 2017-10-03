generate_mixture_data<-function(fakedata=d,par_ests=par_ests){

beta<-par_ests[which(names(par_ests)=="beta")]
delta<-par_ests[which(names(par_ests)=="delta")]
prob_sr<-par_ests[which(names(par_ests)=="prob_sr")]
prob_or<-par_ests[which(names(par_ests)=="prob_or")]
sigmap_e<-par_ests[which(names(par_ests)=="sigmap_e")]
sigma_e<-par_ests[which(names(par_ests)=="sigma_e")]
sigma_u<-par_ests[which(names(par_ests)=="sigma_u")]
sigma_w<-par_ests[which(names(par_ests)=="sigma_w")]
SO<-fakedata$SO

N<-dim(fakedata)[1]
fakedata$subj<-as.integer(as.factor(fakedata$subj))
fakedata$item<-as.integer(as.factor(fakedata$item))
## subj intercepts
u_int<-rnorm(length(unique(fakedata$subj)),mean=0,sd=sigma_u)
## item intercepts
w_int<-rnorm(length(unique(fakedata$item)),mean=0,sd=sigma_w)

rt_tilde<-rep(NA,N)
for(i in 1:N){
  ## SR
  if(SO[i]==1){ 
    reanalysis_sr <- rbinom(n=1,size=1,prob = prob_sr)
    if(reanalysis_sr) {
      rt_tilde[i] <-  rlnorm(1,beta + u_int[fakedata[i,]$subj] + w_int[fakedata[i,]$item] + delta, sigmap_e)
         } else {
            rt_tilde[i] <- rlnorm(1,beta + u_int[fakedata[i,]$subj] + w_int[fakedata[i,]$item], sigma_e)
                }
  }
  ## OR:       
  else { 
         reanalysis_or <- rbinom(n=1,size=1,prob = prob_or)
         if(reanalysis_or) {
         rt_tilde[i] <-  rlnorm(1,beta + u_int[fakedata[i,]$subj] + w_int[fakedata[i,]$item] + delta, sigmap_e)
         } else {
            rt_tilde[i] <- rlnorm(1,beta + u_int[fakedata[i,]$subj] + w_int[fakedata[i,]$item], sigma_e)
                }
       }
}

fakedata$rt_tilde<-rt_tilde
fakedata
}