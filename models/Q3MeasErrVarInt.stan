data {
  int<lower=1> N;                  //number of data points
  real y[N];                       //dep variable log vot
  int<lower=1> J;                  //number of subjects
  int<lower=1> K;                  //number of items
  real meanvdur[J];                // noisy centered mean vdur  
  real<lower=0> se[J];             // se mean vdur 
  real<lower=-1,upper=1> gend[N];  //predictor
  real<lower=-1,upper=1> ln[N];  //predictor
  int<lower=1, upper=J> subj[N];   //subject id
  int<lower=1, upper=K> item[N];   //item id
}

parameters {
  vector[8] beta;              //fixed intercept and slopes
  real true_mvdur[J]; // true unknown value mvdur
  vector[J] u;                 //subject intercepts
  vector[K] w;                 //item intercepts
  real<lower=0> sigma_e;       //error sd
  real<lower=0> sigma_u;       //subj sd
  real<lower=0> sigma_w;       //item sd
}

model {
  real mu;
  //priors
  for(j in 1:J){
  true_mvdur[j] ~ normal(0,1);         
  meanvdur[j] ~ normal(true_mvdur[j],se[j]); // measurement model
  }
  beta ~ normal(0,5);
  sigma_e ~ normal(0,1);
  sigma_u ~ normal(0,1);
  sigma_w ~ normal(0,1);
  u ~ normal(0,sigma_u);    //subj random effects
  w ~ normal(0,sigma_w);    //item random effects
  // likelihood
  for (i in 1:N){
    mu = beta[1] + u[subj[i]] + w[item[i]] + 
         beta[2]*gend[i]+
         beta[3]*ln[i]+
         beta[4]*true_mvdur[subj[i]]+
         beta[5]*gend[i]*true_mvdur[subj[i]]+
         beta[6]*ln[i]*true_mvdur[subj[i]]+
         beta[7]*gend[i]*ln[i]+
         beta[8]*gend[i]*ln[i]*true_mvdur[subj[i]]
         ;
    y[i] ~ normal(mu,sigma_e);
  }
}
