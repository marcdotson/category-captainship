// Morgan Bale 
// Testing version 2 of SDiD which adds weights

data {
  int<lower=0> N; //num stores
  int<lower=0> TT; //num time periods 
  int<lower=1> S; //num of treated stores 
  int<lower=1> C; //num of control stores
  vector[N] treat; //which stores are treated
  vector[TT] post; //which time periods are post 
  vector[S] Y_treat_pre; //treated units in pre period 
  vector[C] Y_con_pre; //control units in pre period 
  matrix[N,TT] Y; //control and treatment unit sales over time 
  matrix[N,TT] D; //treatment indicator 
}


parameters {
  real<lower=0> sigma; //standard deviation of Y?
  real mu; //treatment effect 
  real alpha; //individual fe
  real gamma; //time fe 
  real eta; //intercept in sc equation 
  vector[N] tau; //treatment effect
  matrix<lower=0>[S,C] w;     //weights determined by synthetic control 
}

transformed parameters {
  //vector[N] omega;
  //for(n in 1:N) {
    //omega[n] = 1 / sqrt(w[n]); 
  //}
}

// there needs to be a model for the weights and the DiD
model {
  // priors 
  alpha ~ normal(0,1);
  gamma ~ normal(0,1);
  tau ~ normal(0,1); 
  sigma ~ cauchy(0, .01);
  //sc equation for weights
  for(s in 1:S) {
    for(c in 1:C) {
    Y_treat_pre[s] ~ normal(eta + w[s,c]*Y_con_pre[c]);
  }}
  //DiD equation 
  for(n in 1:N) {
    for(t in 1:TT) {
    Y[n, t] ~ normal(mu + alpha*treat[n] + gamma*post[t] + tau[n]*D[n,t], sigma*omega[n]);
    }
  }
}

