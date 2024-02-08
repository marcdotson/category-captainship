// Morgan Bale 
// Testing version 2 of SDiD which adds weights

data {
  int<lower=0> N; //num stores (control stores + 1 treat store00)
  int<lower=0> TT; //num time periods 
  int<lower=1> C; //num of control stores
  int<lower=1> I; //last pre period 
  vector[N] treat; //which stores are treated
  vector[TT] post; //which time periods are post 
  vector[I] Y_treat_pre; //treated units in pre period 
  matrix[C, I] Y_con_pre; //control units in pre period 
  matrix[N,TT] Y; //control and treatment unit sales over time 
  matrix[N,TT] D; //treatment indicator 
}


parameters {
  real<lower=0> sigma; //standard deviation of Y?
  real mu; //treatment effect 
  real alpha; //individual fe
  real gamma; //time fe 
  real eta; //intercept in sc equation 
  real nu; //standard deviation in SC equation 
  real tau; //treatment effect
  vector<lower=0>[C] w;     //weights determined by synthetic control 
}

transformed parameters {
  //transform weights into omega paramter for DiD
  vector[N] omega;
  for(c in 1:C) {
    omega[c] = 1 / sqrt(w[c]); 
  }
  omega[N] = 1; 
}

// there needs to be a model for the weights and the DiD
model {
  // priors 
  alpha ~ normal(0,1);
  gamma ~ normal(0,1);
  tau ~ normal(0,1); 
  sigma ~ cauchy(0, .01);
  nu ~ cauchy(0, .01);
  //sc equation for weights
  for(i in 1:I) {
    for(c in 1:C) {
      Y_treat_pre[i] ~ normal(eta + w[c]*Y_con_pre[c, i], nu);
    }
  }
  //DiD equation 
  for(n in 1:N) {
    for(t in 1:TT) {
    Y[n, t] ~ normal(mu + alpha*treat[n] + gamma*post[t] + tau*D[n,t], sigma*omega[n]);
    }
  }
}

