// Morgan Bale 
// Testing a version of SDiD 

data {
  int<lower=0> N; //num stores
  int<lower=0> TT; //num time periods 
  vector[N] treat; //which stores are treated
  vector[TT] post; //which time periods are post 
  matrix[N,TT] Y; //control and treatment unit sales over time 
  matrix[N,TT] D; //treatment indicator 
}


parameters {
  real<lower=0> sigma; //standard deviation of Y?
  real mu; //treatment effect 
  real alpha; //individual fe
  real gamma; //time fe 
  vector[N] tau; //treatment effect
}

// there needs to be a model for the weights and the DiD
model {
  // priors 
  alpha ~ normal(0,1);
  gamma ~ normal(0,1);
  tau ~ normal(0,1); 
  sigma ~ cauchy(0, .01);
  //DiD equation 
  for(n in 1:N) {
    for(t in 1:TT) {
    Y[n, t] ~ normal(mu + alpha*treat[n] + gamma*post[t] + tau[n]*D[n,t], sigma);
    }
  }
}

