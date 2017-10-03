generate_lnorm_data<-function(fakedata=headnoun,par_ests=par_ests){

beta<-c(par_ests[1:2])
sigma_e<-par_ests[3]
sigma_u<-par_ests[4]
sigma_w<-par_ests[5]

so<-fakedata$so

N<-dim(fakedata)[1]

fakedata$subj<-as.integer(as.factor(fakedata$subj))
fakedata$item<-as.integer(as.factor(fakedata$item))
## subj intercepts
u_int<-rnorm(length(unique(fakedata$subj)),mean=0,sd=sigma_u)
## item intercepts
w_int<-rnorm(length(unique(fakedata$item)),mean=0,sd=sigma_w)

rt_tilde<-rep(NA,N)

for(i in 1:N){
  rt_tilde[i] <- rlnorm(1,beta[1] + u_int[fakedata[i,]$subj] + w_int[fakedata[i,]$item] + beta[2]*so[i],sigma_e) 
}   

fakedata$rt_tilde<-rt_tilde
fakedata
}

library(MASS)
#m1 is a maximal model 
gen_fake_lnorm<-function(nitem=16,
                         nsubj=40,
                         beta=fixef(m1),
                         ranefsd=c(attr(VarCorr(m1)$subj,"stddev"),attr(VarCorr(m1)$item,"stddev")),corr=c(-.6,-.6),
                         sigma_e=attr(VarCorr(m1),"sc")){
  ## prepare data frame:
g1<-data.frame(item=1:nitem,
               cond=rep(letters[1:2],8))
g2<-data.frame(item=1:nitem,
               cond=rep(letters[2:1],8))

fakedat<-rbind(g1[rep(seq_len(nrow(g1)), 
                      nsubj/2),],
               g2[rep(seq_len(nrow(g2)), 
                      nsubj/2),])

fakedat$subj<-subj

fakedat$so<-ifelse(fakedat$cond=="a",-1/2,1/2)

Sigma.u<-matrix(c(ranefsd[1]^2,
                  corr[1]*ranefsd[1]*ranefsd[2],
                  corr[1]*ranefsd[1]*ranefsd[2],
                  ranefsd[2]^2),nrow=2)

Sigma.w<-matrix(c(ranefsd[3]^2,
                  corr[2]*ranefsd[3]*ranefsd[4],
                  corr[2]*ranefsd[3]*ranefsd[4],
                  ranefsd[4]^2),nrow=2)

## subj ranef
u<-mvrnorm(n=length(unique(fakedat$subj)),
               mu=c(0,0),Sigma=Sigma.u)
# item ranef
w<-mvrnorm(n=length(unique(fakedat$item)),
           mu=c(0,0),Sigma=Sigma.w)

N<-dim(fakedat)[1]
rt<-rep(NA,N)
for(i in 1:N){
  rt[i] <- rlnorm(1,beta[1] + 
                    u[fakedat[i,]$subj,1] +
                    w[fakedat[i,]$item,1] + 
                    (beta[2]
                     +u[fakedat[i,]$subj,2]
                         +w[fakedat[i,]$item,2])*fakedat$so[i],sigma_e) 
}   

fakedat$rt<-rt
fakedat
}

