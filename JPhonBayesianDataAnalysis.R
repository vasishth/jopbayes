## ----setup,include=FALSE,cache=FALSE,echo=FALSE--------------------------
set.seed(123)

library(knitr)
library(coda)
library(plyr)
# library(rjags)
library(ggplot2)
library(xtable)
library(dplyr)
library(tibble)


library(rstan)
library(parallel)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

library(loo)
library(lme4)

library(rstantools)
library(bayesplot)
library(stringr)

opts_chunk$set(fig.path='fig-', 
               fig.align='center', 
               fig.show='hold')
options(replace.assign=TRUE,show.signif.stars=FALSE)
options(replace.assign=TRUE,width=75)
opts_chunk$set(dev='postscript')

## ----loadfunctionsdata,echo=FALSE----------------------------------------
source("R/RFunctions.R")
source("R/ggplotpostpred.R")
source("R/plotpostpred.R")
source("R/multiplot.R")
#source("R/generate_mixture_data.R")
#source("R/generate_lnorm_data.R")
source("R/stan_summary.R")
source("R/plotresults.R")

## ----priors-from-file, echo=F--------------------------------------------
# To be sure that I'm using the same priors as in the file, I'm extracting them:
code <- paste(readLines("models/VarInt.stan"),collapse="\n")
priors_beta1 <- as.numeric(str_match(code,"beta\\[1\\] ~ normal\\((.*?),(.*?)\\);")[-1])
priors_beta2 <- as.numeric(str_match(code,"beta\\[2\\] ~ normal\\((.*?),(.*?)\\);")[-1])
priors_sigma_e <- as.numeric(str_match(code,"sigma_e ~ normal\\((.*?),(.*?)\\);")[-1])
priors_sigma_u <- as.numeric(str_match(code,"sigma_u ~ normal\\((.*?),(.*?)\\);")[-1])
priors_sigma_w <- as.numeric(str_match(code,"sigma_w ~ normal\\((.*?),(.*?)\\);")[-1])

## ----figpriors,echo=FALSE------------------------------------------------
op<-par(mfrow=c(2,2),pty="s")
par(oma = rep(0, 4), mar = c(2.7, 2.7, 0.1, 0.1), mgp = c(1.7, 0.4, 0))
b<-seq(-priors_beta1[2]*2,priors_beta1[2]*2,by=0.01)
plot(b,dnorm(b,mean=priors_beta1[1],sd=priors_beta1[2]),type="l",ylab="density",
     xlab=expression(beta[1]))
plot(b,dnorm(b,mean=priors_beta2[1],sd=priors_beta2[2]),type="l",ylab="density",
     xlab=expression(beta[2]))
sig<-seq(0,priors_sigma_e[2]*3,by=0.01)
plot(sig,dnorm(sig,mean=priors_sigma_e[1],sd=priors_sigma_e[2]),type="l",ylab="density",
     xlab=expression(sigma))
plot(sig,dnorm(sig,mean=priors_sigma_u[1],sd=priors_sigma_u[2]),type="l",ylab="density",
     xlab=expression(sigma[u[0]]))

## ----loaddatamandarin,echo=FALSE-----------------------------------------
datM <- read.delim("data/songyuan.txt", stringsAsFactors = F)
# Add a factor 
# aspirated (voiceless) -1, unaspirated (voiced) +1:
datM$voice <- ifelse(datM$TargetConsonant %in% c("t", "k"), -1, +1)
## isolate relevant columns:
datM<-datM[,c(2,3,7,14,15,18)]

## rename columns:
colnames(datM)[3]<-"wordtype"
colnames(datM)[5]<-"vduration"

# center gender predictor
datM$gend<-ifelse(datM$gender=="f",1,-1)

# fully crossed:
#xtabs(~subject+wordtype,datM)
## nearly balanced: 10 data points per subj per condition
#xtabs(~subject+voice,datM)
#xtabs(~wordtype+voice,datM)
#xtabs(~voice+gend,datM)

## ----loaddataen,echo=FALSE-----------------------------------------------
datE <- read.delim("data/english.txt", stringsAsFactors = F)
## voiceless stops -1, voiced stops +1:
datE$voice <- ifelse(datE$TargetConsonant %in% c("kh", "kjh", "kwh", "th", "twh"),-1,+1)
datE$gender <- factor(datE$gender, levels = c("f", "m"))
datE$gend<-ifelse(datE$gender=="f",1,-1)

## get relevant columns:
datE<-datE[,c(2,3,7,14,15,20,21)]
colnames(datE)[5]<-"vduration"

## ----stripplots,echo=FALSE,fig.width=8,fig.height=4----------------------
datM$subject <- substr(datM$subject, 5, 7)
pm1 <- aggregate(msVOT ~ subject, datM[datM$voice==-1 & datM$gender=="m",], mean)
pm2 <- aggregate(msVOT ~ subject, datM[datM$voice==-1 & datM$gender=="f",], mean)
plotmat <- rbind(pm1[order(pm1$msVOT), ], pm2[order(pm2$msVOT), ])
datM$subject <- factor(datM$subject, levels=plotmat$subject)

## Reorder the levels of datE$subject so that they're arranged by mean 
datE$subject <- substr(datE$subject, 5, 7)
pm1 <- aggregate(msVOT ~ subject, datE[datE$voice==-1 & datE$gender=="m",], mean)
pm2 <- aggregate(msVOT ~ subject, datE[datE$voice==-1 & datE$gender=="f",], mean)
plotmat <- rbind(pm1[order(pm1$msVOT), ], pm2[order(pm2$msVOT), ])
datE$subject <- factor(datE$subject, levels=plotmat$subject)

## Is there a way in an Rnw file to specify an exact width, to match that of 
## the inpage page size of the published article?  That way we could make each
## of these be exactly half the page width, so they'll fit side by side.
op<-par(mfrow=c(1,2))
par(oma = c(1,1,0,0), mar = c(2.5, 1.9, 0.1, 0.1), mgp = c(1.7, 0.4, 0))
pchs <- c(20, 18); names(pchs) <- c("unaspirated", "aspirated")
stripchart(msVOT ~ subject, datM[datM$voice==-1, ], method="jitter", vertical=T,
  pch=pchs["aspirated"], las=2, xlim=c(1, 25), at=c(1:10, 16:25),
  ylim=c(2,213), ylab="")
boxplot(msVOT ~ gender, datM[datM$voice==-1, ], at=c(14, 12), 
  range=0, add=TRUE, yaxt="n")
stripchart(msVOT ~ subject, datM[datM$voice==+1, ], method="jitter", vertical=T,
  pch=pchs["unaspirated"], col="gray", at=c(1:10, 16:25), add=T)
boxplot(msVOT ~ gender, datM[datM$voice==+1, ], at=c(14, 12), 
  range=0, border="gray", add=TRUE, yaxt="n")
mtext("subjects ordered within gender by mean long-lag VOT", side=1, cex=0.95,outer=T)
mtext(" Mandarin", line=-1, adj=0)
mtext("VOT (ms)",side=2,outer=T)

stripchart(msVOT ~ subject, datE[datE$voice==-1, ], method="jitter", vertical=T,
  pch=pchs["aspirated"], las=2, xlim=c(1, 25), at=c(1:10, 16:25),
  ylim=c(2,213), ylab="")
boxplot(msVOT ~ gender, datE[datE$voice==-1, ], at=c(14, 12), 
  range=0, add=TRUE, yaxt="n")
stripchart(msVOT ~ subject, datE[datE$voice==+1, ], method="jitter", vertical=T,
  pch=pchs["unaspirated"], col="gray", at=c(1:10, 16:25), add=T)
boxplot(msVOT ~ gender, datE[datE$voice==+1, ], at=c(14, 12), 
  range=0, border="gray", add=TRUE, yaxt="n")
#mtext("numbers at VOT=0 are counts of voiced tokens with lead VOT ", side=1, line=2.4, cex=0.95)
mtext(" English", line=-1, adj=0)
text(c(2, 4:10, 16, 17, 20, 21, 25), rep(0,13), 
  xtabs(~ subject, datE[datE$msVOT < 0, ])[c(2, 4:10, 11, 12, 15, 16, 20)])
#op <- par(mfrow=c(1,2),pty="s")
#boxplot(msVOT~gender,subset(datM,voice==-1),
#        ylim=c(0,200),main="Mandarin, aspirated",xlab="gender")
#boxplot((msVOT)~gender,subset(datM,voice==1),
#        ylim=c(0,200),main="Mandarin, unaspirated",xlab="gender")

#boxplot(msVOT~gender,subset(datE,voice==-1 & msVOT>0),
#        ylim=c(0,200),main="English, voiceless",xlab="gender")
#boxplot((msVOT)~gender,subset(datM,voice==1),
#        ylim=c(0,200),main="English, voiced",xlab="gender")

## ----question1M,echo=FALSE-----------------------------------------------
library(lme4)
m1M <- lmer(msVOT ~ 1 + gend + (1 | subject)+(1 | wordtype), datM[datM$voice==-1, ] )
#summary(m1M)

#library(car)
#qqPlot(residuals(m1M))
#xtabs(~TargetConsonant+voice,datE)

## ----question1E,echo=FALSE-----------------------------------------------
m1E <- lmer(msVOT ~ gend + (1 | subject) + (1 | WorldBet), datE[datE$voice==-1,])
#summary(m1E)

## ----question2M,echo=FALSE-----------------------------------------------
# use unaspirated data to compute vowel duration and se:
datMvoiced<-subset(datM,voice==1)

subj<-unique(datMvoiced$subject)
means<-ses<-rep(NA,length(subj))
for(i in 1:length(subj)){
  dat_temp<-subset(datMvoiced,subject==subj[i])
  n<-dim(dat_temp)[1]
  means[i]<-mean(dat_temp$vduration)
  ses[i]<-sd(dat_temp$vduration)/sqrt(n)
}

subj_vduration<-data.frame(subject=subj,meanvdur=means,se=ses)
subjectdf<-data.frame(factor_subj=subj,integer_subj=1:length(subj))
subj_vdurationM<-subj_vduration<-merge(subj_vduration,subjectdf,by.x="subject",by.y="factor_subj")

## add vowel dur and se to main data frame:
dat2M<-merge(datM,subj_vduration,by.x="subject",by.y="subject")

## to-do: add measurement error
m2M <- lmer(msVOT ~ scale(meanvdur,scale=F) + (1 | subject)+(1 | wordtype), dat2M[dat2M$voice==-1, ])
#summary(m2M)

## ----question2E,echo=FALSE-----------------------------------------------
datEvoiced<-subset(datE,voice==1)

subj<-unique(datEvoiced$subject)
means<-ses<-rep(NA,length(subj))
for(i in 1:length(subj)){
  dat_temp<-subset(datEvoiced,subject==subj[i])
  n<-dim(dat_temp)[1]
  means[i]<-mean(dat_temp$vduration)
  ses[i]<-sd(dat_temp$vduration)/sqrt(n)
}

subj_vduration<-data.frame(subject=subj,meanvdur=means,se=ses)
subjectdf<-data.frame(factor_subj=subj,integer_subj=1:length(subj))
subj_vdurationE<-subj_vduration<-merge(subj_vduration,subjectdf,by.x="subject",by.y="factor_subj")

dat2E<-merge(datE,subj_vduration,by.x="subject",by.y="subject")

## to-do: add measurement error
m2E <- lmer(msVOT ~ scale(meanvdur,scale=F) + (1 | subject)+(1 | WorldBet), dat2E[dat2E$voice==-1, ])
#summary(m2E)

## ----echo=FALSE----------------------------------------------------------
##needed for between lang comparison:
dat2M$subj<-as.integer(factor(dat2M$subject))
dat2M$item<-as.integer(factor(dat2M$wordtype))

dat3M<-dat2M[,c(11,12,6,7,4,5,8,9)]
dat3M$lang<-factor("CN")

dat2E$subj<-as.integer(factor(dat2E$subject))
dat2E$item<-as.integer(factor(dat2E$WorldBet))

dat3E<-dat2E[,c(11,12,6,7,4,5,8,9)]

dat3E$lang<-factor("EN")

dat<-rbind(dat3M,dat3E)
dat$ln<-ifelse(dat$lang=="EN",-1,1)

dat$subj<-paste(dat$lang,dat$subj,sep="")
dat$item<-paste(dat$lang,dat$item,sep="")

m3<-lmer(log(msVOT)~gend*scale(meanvdur,scale=FALSE)*ln+
           (1|subj)+(1|item),subset(dat,msVOT>0))
#summary(m3)

## ----bayesiananalysis,echo=FALSE,message=FALSE, error=FALSE,cache=TRUE----
dat<-subset(dat,msVOT>0)
dat$logVOT<-log(dat$msVOT)

Mdat<-subset(dat,lang=="CN")
#xtabs(~subj+item,Mdat)
#xtabs(~subj+gend,Mdat)
#xtabs(~subj+voice,Mdat)

Mdat<-subset(Mdat,voice==-1)
Mdat.stan<-list(subj=as.integer(factor(Mdat$subj)),
                item=as.integer(factor(Mdat$item)),
                gend=Mdat$gend,
                J=length(unique(Mdat$subj)),
                K=length(unique(Mdat$item)),
                N=dim(Mdat)[1],
                y=Mdat$msVOT)

#str(Mdat.stan)

Edat<-subset(dat,lang=="EN")
#xtabs(~subj+item,Edat)
#xtabs(~subj+gend,Edat)
#xtabs(~subj+voice,Edat)
Edat<-subset(Edat,voice==-1)

Edat.stan<-list(subj=as.integer(factor(Edat$subj)),
                item=as.integer(factor(Edat$item)),
                gend=Edat$gend,
                J=length(unique(Edat$subj)),
                K=length(unique(Edat$item)),
                N=dim(Edat)[1],
                y=Edat$msVOT)

#str(Edat.stan)

m1Mstan <- stan(file = "models/VarInt.stan", 
                data = Mdat.stan,control=list(adapt_delta=.9))

m1Estan <- stan(file = "models/VarInt.stan", 
                data = Edat.stan,control=list(adapt_delta=.9))

## ----q1summaries,echo=FALSE----------------------------------------------

nicer_summary <- function(summary_stan) {
  summary_stan <- bind_cols(tibble("pred"=rownames(summary_stan)),select(as_tibble(summary_stan),-se_mean,-sd))
  summary_stan[c("mean","2.5%","97.5%","n_eff")]<- round(summary_stan[c("mean","2.5%","97.5%","n_eff")])
  summary_stan[c("Rhat")]<- format(summary_stan$Rhat,digits=2,nsmall=2)
  summary_stan[c("pred")]<- str_replace(summary_stan$pred,"\\_","\\\\_")
  return(summary_stan)
}


m1Msummary <-nicer_summary(summary(m1Mstan,pars=c("beta[1]","beta[2]","sigma_e","sigma_u","sigma_w"),probs=c(0.025,0.975))$summary)
m1Mbeta2<-rstan::extract(m1Mstan,pars="beta[2]")$`beta[2]`

m1Esummary<-nicer_summary(summary(m1Estan,pars=c("beta[1]","beta[2]","sigma_e","sigma_u","sigma_w"),probs=c(0.025,0.975))$summary)
m1Ebeta2<-rstan::extract(m1Estan,pars="beta[2]")$`beta[2]`

m1Mbeta2prob <- round(mean(m1Mbeta2>0),2)
Mbeta2l <- round(quantile(m1Mbeta2,.025))
Mbeta2h <- round(quantile(m1Mbeta2,.975))

m1Ebeta2prob<- round(mean(m1Ebeta2>0),2)
Ebeta2l <- round(quantile(m1Ebeta2,.025))
Ebeta2h <- round(quantile(m1Ebeta2,.975))


## ----q1plots,echo=FALSE--------------------------------------------------
q1Mtrace<-traceplot(m1Mstan,pars=c("beta[1]","beta[2]",
                         "sigma_e","sigma_u","sigma_w"))
q1Mhist<-mcmc_hist(as.array(m1Mstan),pars = c("beta[1]","beta[2]","sigma_e","sigma_u","sigma_w"))

q1Etrace<-traceplot(m1Estan,pars=c("beta[1]","beta[2]",
                         "sigma_e","sigma_u","sigma_w"))
q1Ehist<-mcmc_hist(as.array(m1Estan),pars = c("beta[1]","beta[2]","sigma_e","sigma_u","sigma_w"))

## ----results='asis',echo=F-----------------------------------------------
cat(paste0(apply(bind_cols(m1Msummary,m1Esummary[-1]),1, function(r) paste0(r, collapse=" & ")),collapse=" \\\\ \n "))

## ----figq1Mtrace,echo=FALSE----------------------------------------------
q1Mtrace

## ----figq1Etrace,echo=FALSE----------------------------------------------
q1Etrace

## ----meanvotvdurplot,echo=FALSE,fig.width=8.5,fig.height=9---------------
meansM <- aggregate(vduration ~ subject, subset(datM,voice==1), mean)
names(meansM)[ncol(meansM)] <- "meanvdur"
meansM$sdvdur <- aggregate(vduration ~ subject, subset(datM,voice==1), sd)$vduration
# Add estimates of mean and sd for VOT in long-lag stops.
meansM$meanVOT <- aggregate(vduration ~ subject, subset(datM,voice==-1), mean)$vduration
meansM$sdVOT <- aggregate(vduration ~ subject, subset(datM,voice==-1), sd)$vduration
# Do the same for the English data frame.
meansE <- aggregate(vduration ~ subject, subset(datE,voice==1), mean)
names(meansE)[ncol(meansE)] <- "meanvdur"
meansE$sdvdur <- aggregate(vduration ~ subject, subset(datE,voice==1), sd)$vduration
meansE$meanVOT <- aggregate(vduration ~ subject, subset(datE,voice==-1), mean)$vduration
meansE$sdVOT <- aggregate(vduration ~ subject, subset(datE,voice==-1), sd)$vduration

# Plot the mean VOT +/- 1 sd against mean vduration +/- 1 sd.
par(oma=rep(0,4), mar=c(2.7, 3.3, 0.1, 0.1), mgp=c(1.8, 0.4, 0), pty="s")
op<-par(mfrow=c(1,2))
plot(meanVOT ~ meanvdur, meansM, xlim=c(95,260), ylim=c(125,350), pch=19,
  xlab="mean vowel duration after unaspirated stops (ms)",
  ylab="mean VOT in aspirated stops (ms)")
for(subj.i in 1:nrow(meansM)) {
  lines(meansM$meanvdur[subj.i] + c(-1, +1)*meansM$sdvdur[subj.i], 
    rep(meansM$meanVOT[subj.i], 2))
  lines(rep(meansM$meanvdur[subj.i], 2), 
    meansM$meanVOT[subj.i] + c(-1, +1)*meansM$sdVOT[subj.i])
}
mtext(" Mandarin", line=-1, adj=0)

plot(meanVOT ~ meanvdur, meansE, xlim=c(95,260), ylim=c(125,350), pch=19,
  xlab="mean vowel duration after voiced stops (ms)",
  ylab="mean VOT in voiceless stops (ms)")
for(subj.i in 1:nrow(meansE)) {
  lines(meansE$meanvdur[subj.i] + c(-1, +1)*meansE$sdvdur[subj.i], 
    rep(meansE$meanVOT[subj.i], 2))
  lines(rep(meansE$meanvdur[subj.i], 2), 
    meansE$meanVOT[subj.i] + c(-1, +1)*meansE$sdVOT[subj.i])
}
mtext(" English", line=-1, adj=0)

## ----q2lm,echo=FALSE-----------------------------------------------------
lmMandarin<-summary(lm(meanVOT~meanvdur,meansM))
lmEnglish<-summary(lm(meanVOT~meanvdur,meansE))

## ----lmxtableMandarin,echo=FALSE,results='asis'--------------------------
xtable(lmMandarin,caption="Linear model showing the effect of mean vowel duration on mean VOT in Mandarin.",label="tab:lmMandarin")

## ----lmxtableEnglish,echo=FALSE,results='asis'---------------------------
xtable(lmEnglish,caption="Linear model showing the effect of mean vowel duration on mean VOT in English.",label="tab:lmEnglish")

## ----priors-from-file2, echo=F-------------------------------------------
# To be sure that I'm using the same priors as in the file, I'm extracting them:
code <- paste(readLines("models/MeasErrVarInt.stan"),collapse="\n")
priors_beta1 <- as.numeric(str_match(code,"beta\\[1\\] ~ normal\\((.*?),(.*?)\\);")[-1])
priors_beta2 <- as.numeric(str_match(code,"beta\\[2\\] ~ normal\\((.*?),(.*?)\\);")[-1])
priors_sigma_e <- as.numeric(str_match(code,"sigma_e ~ normal\\((.*?),(.*?)\\);")[-1])
priors_sigma_u <- as.numeric(str_match(code,"sigma_u ~ normal\\((.*?),(.*?)\\);")[-1])
priors_sigma_w <- as.numeric(str_match(code,"sigma_w ~ normal\\((.*?),(.*?)\\);")[-1])
priors_vdur <- as.numeric(str_match(code,"true_mvdur ~ normal\\((.*?),(.*?)\\);")[-1])

## ----measurementerror,echo=FALSE,message=FALSE, error=FALSE,cache=TRUE----
# Mandarin

## repeated code:
datMvoiced<-subset(datM,voice==1)

## compute estimates of mean vowel dur
subj<-unique(datMvoiced$subject)
means<-ses<-rep(NA,length(subj))
for(i in 1:length(subj)){
  dat_temp<-subset(datMvoiced,subject==subj[i])
  n<-dim(dat_temp)[1]
  # means[i]<-mean(log(dat_temp$vduration))
  # ses[i]<-sd(log(dat_temp$vduration))/sqrt(n)
  means[i]<-mean((dat_temp$vduration))
  ses[i]<-sd((dat_temp$vduration))/sqrt(n)
}

subj_vduration<-data.frame(subject=subj,
                           subj=as.integer(factor(subj)),
                           meanvdur=means,se=ses)

Mdat2.stan<-list(subj=as.integer(factor(Mdat$subj)),
                item=as.integer(factor(Mdat$item)),
                gend=Mdat$gend,
                meanvdur=(subj_vduration$meanvdur-mean(subj_vduration$meanvdur))/sd(subj_vduration$meanvdur),
                se=subj_vduration$se/sd(subj_vduration$meanvdur),
                J=length(unique(Mdat$subj)),
                K=length(unique(Mdat$item)),
                N=dim(Mdat)[1],
                y=Mdat$msVOT)

# English
datEvoiced<-subset(datE,voice==1)

subj<-unique(datEvoiced$subject)
means<-ses<-rep(NA,length(subj))
for(i in 1:length(subj)){
  dat_temp<-subset(datEvoiced,subject==subj[i])
  n<-dim(dat_temp)[1]
  # means[i]<-mean(log(dat_temp$vduration))
  # ses[i]<-sd(log(dat_temp$vduration))/sqrt(n)
   means[i]<-mean(dat_temp$vduration)
   ses[i]<-sd(dat_temp$vduration)/sqrt(n)
}

subj_vduration<-data.frame(subject=subj,meanvdur=means,se=ses)

Edat2.stan<-list(subj=as.integer(factor(Edat$subj)),
                item=as.integer(factor(Edat$item)),
                gend=Edat$gend,
                meanvdur=(subj_vduration$meanvdur-mean(subj_vduration$meanvdur))/sd(subj_vduration$meanvdur),
                se=subj_vduration$se/sd(subj_vduration$meanvdur),
                J=length(unique(Edat$subj)),
                K=length(unique(Edat$item)),
                N=dim(Edat)[1],
                y=Edat$msVOT)

## ----measurementerrormodel,echo=FALSE,message=FALSE, error=FALSE,cache=TRUE----
#str(Mdat2.stan)
m2Mmeaserrstan <- stan(file = "models/MeasErrVarInt.stan", 
                data = Mdat2.stan,
                control = list(adapt_delta = 0.999,max_treedepth=12))

m2Emeaserrstan <- stan(file = "models/MeasErrVarInt.stan", 
                data = Edat2.stan,
                control = list(adapt_delta = 0.999,max_treedepth=12))  

## ----measurementerror-summaries,echo=FALSE,error=FALSE,message=FALSE,cache=TRUE----
m2Msummary<-nicer_summary(summary(m2Mmeaserrstan,pars=c("beta[1]",
                              "beta[2]",
                              "sigma_e",
                              "sigma_u",
                              "sigma_w"),
                              probs=c(0.025,0.975))$summary)

m2Esummary<- nicer_summary(summary(m2Emeaserrstan,pars=c("beta[1]",
                              "beta[2]",
                              "sigma_e",
                              "sigma_u",
                              "sigma_w"),
                              probs=c(0.025,0.975))$summary)

## ----m2nomeaserr,echo=FALSE,cache=TRUE,error=FALSE,message=FALSE---------
m2MNOmeaserrstan <- stan(file = "models/NOMeasErrVarInt.stan", 
                data = Mdat2.stan,
                control = list(adapt_delta = 0.99))

m2ENOmeaserrstan <- stan(file = "models/NOMeasErrVarInt.stan", 
                data = Edat2.stan,
                control = list(adapt_delta = 0.99)) 

## ----nomeasurementerror-summaries,echo=FALSE,error=FALSE,message=FALSE,cache=TRUE----
m2MNOsummary<-nicer_summary(summary(m2MNOmeaserrstan,pars=c("beta[1]",
                              "beta[2]",
                              "sigma_e",
                              "sigma_u",
                              "sigma_w"),
                              probs=c(0.025,0.975))$summary)

m2ENOsummary<- nicer_summary(summary(m2ENOmeaserrstan,pars=c("beta[1]",
                              "beta[2]",
                              "sigma_e",
                              "sigma_u",
                              "sigma_w"),
                              probs=c(0.025,0.975))$summary)

## ----results='asis',echo=F-----------------------------------------------
cat(paste0(apply(bind_cols(m2Msummary,m2Esummary[-1]),1, function(r) paste0(r, collapse=" & ")),collapse=" \\\\ \n "))

## ----echo=FALSE----------------------------------------------------------
m2Mtrace<-traceplot(m2Mmeaserrstan,pars=c("beta[1]",
                              "beta[2]",
                              "sigma_e",
                              "sigma_u",
                              "sigma_w"))
m2Mtrace

## ----echo=FALSE----------------------------------------------------------
m2Etrace<-traceplot(m2Mmeaserrstan,pars=c("beta[1]",
                              "beta[2]",
                              "sigma_e",
                              "sigma_u",
                              "sigma_w"))
m2Etrace

## ----data,echo=F,results="hide"------------------------------------------
datvoiced<-subset(dat,voice==1)
 
subj<-unique(datvoiced$subj)
means<-ses<-rep(NA,length(subj))
for(i in 1:length(subj)){
  dat_temp<-subset(datvoiced,subj==subj[i])
  n<-dim(dat_temp)[1]
  means[i]<-mean((dat_temp$vduration))
  ses[i]<-sd((dat_temp$vduration))/sqrt(n)
}

subj_vduration<-data.frame(subject=subj,meanvdur=means,se=ses)

datvless<-subset(dat,voice==-1)

dat.stan<-list(subj=as.integer(factor(datvless$subj)),
                item=as.integer(factor(datvless$item)),
                gend=datvless$gend,
                lang=datvless$ln,
                meanvdur=(subj_vduration$meanvdur-mean(subj_vduration$meanvdur))/sd(subj_vduration$meanvdur),
                se=subj_vduration$se/sd(subj_vduration$meanvdur),
                J=length(unique(datvless$subj)),
                K=length(unique(datvless$item)),
                N=dim(datvless)[1],
                y=datvless$msVOT)

## ----q3-model,echo=FALSE,message=FALSE, error=FALSE,cache=TRUE-----------
#str(Mdat2.stan)
m3astan <- stan(file = "models/VarIntCross.stan", 
                data = dat.stan,
                control = list(adapt_delta = 0.99))

m3bstan <- stan(file = "models/MeasErrVarIntCross.stan", 
                data = dat.stan,
                control = list(adapt_delta = 0.99))
 

## ----q3-summaries,echo=FALSE,message=FALSE,cache=TRUE--------------------
m3asummary <- nicer_summary(summary(m3astan,pars=c("beta",
                              "sigma_e",
                              "sigma_u",
                              "sigma_w"),
                              probs=c(0.025,0.975))$summary)

m3bsummary <- nicer_summary(summary(m3bstan,pars=c("beta",
                              "sigma_e",
                              "sigma_u",
                              "sigma_w"),
                              probs=c(0.025,0.975))$summary)


## ----m3lmer,echo=FALSE---------------------------------------------------
# summary(m3almer<-lmer((msVOT)~gend*ln+(1|subj)+(1|item),subset(dat,voice==-1)))
# summary(m3blmer<-lmer((msVOT)~scale(meanvdur)*ln+(1|subj)+(1|item),subset(dat,voice==-1)))

## ----results='asis',echo=F-----------------------------------------------
cat(paste0(apply(m3asummary,1, function(r) paste0(r, collapse=" & ")),collapse=" \\\\ \n "))

## ----results='asis',echo=F-----------------------------------------------
cat(paste0(apply(m3bsummary,1, function(r) paste0(r, collapse=" & ")),collapse=" \\\\ \n "))

## ----echo=FALSE----------------------------------------------------------
traceplot(m3astan,pars=c("beta","sigma_e","sigma_u","sigma_w"))

## ----echo=FALSE----------------------------------------------------------
traceplot(m3bstan,pars=c("beta","sigma_e","sigma_u","sigma_w"))

## ----results="asis",echo=F-----------------------------------------------
cat("\\begin{verbatim}\n")
cat(paste(readLines("models/VarInt.stan"),collapse="\n"))
cat("\\end{verbatim}\n")

## ----results="asis",echo=F-----------------------------------------------
cat("\\begin{verbatim}\n")
cat(paste(readLines("models/MeasErrVarInt.stan"),collapse="\n"))
cat("\\end{verbatim}\n")

## ----results="asis",echo=F-----------------------------------------------
cat("\\begin{verbatim}\n")
cat(paste(readLines("models/VarIntCross.stan"),collapse="\n"))
cat("\\end{verbatim}\n")

## ----results="asis",echo=F-----------------------------------------------
cat("\\begin{verbatim}\n")
cat(paste(readLines("models/MeasErrVarIntCross.stan"),collapse="\n"))
cat("\\end{verbatim}\n")

