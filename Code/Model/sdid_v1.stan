// Morgan Bale 
// Testing a version of SDiD 

data {
  int<lower=0> N; //num individuals
  int<lower=0> TT; //num time periods 
  matrix[N,TT] Y; //control and treatment unit sales over time 
  matrix[N,TT] D; //treatment indicator 
}


parameters {
  real<lower=0> sigma; //standard deviation of Y?
  real tau; //treatment effect 
  vector[N] alpha; //individual fe
  vector[TT] gamma; //time fe 
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
    Y[n, t] ~ normal(alpha[n] + gamma[t] + D[n,t]*tau, sigma);
    }
  }
}

