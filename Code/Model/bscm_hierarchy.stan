//
// This model is adapted from Gupta's paper
// Morgan Bale
// Feb 2022

// Data
data{
  int N_train; //Number of observations in the pre-treatment periods
  int N_test; //Number of observations in the post-treatment periods
  int p; //Number of control units
  int s; //number of control store covariates
  real y_train[N_train]; //Treated unit in the pre-treatment periods
  matrix[N_train, p] X_train; //Control unit matrix in the pre-treatment
  matrix[N_test, p] X_test; //Control unit matrix in the post-treatment
  matrix[p, s] Z;  //control store covariates 
}

// The parameters accepted by the model. 
parameters{
  real<lower=0> sigma2; //Error term variance
  real<lower=0> eta2; //Error term variance for Beta eq
  vector[p] beta; 
  //Hyperparameters prior
  vector[s] theta;        //hierarchical effect 
  //real<lower=0> tau; //Global shrinkage
  real beta_0; //intercept 
  //vector<lower=0>[p] lambda; //Local shrinkage
}

transformed parameters{
  real<lower=0> sigma; //Error term sd
  real<lower=0> eta; //Error term sd for Beta eq
  //vector<lower=0>[p] lambda2; 
  vector[N_train] X_beta; //Synthetic control unit prediction in the pre-treatment period
  sigma = sqrt(sigma2);
  eta = sqrt(eta2); 
  X_beta = beta_0 + X_train*beta;
  //lambda2 = lambda .* lambda; 
}

// The model to be estimated. 
model{
  //Pre-treatment estimation
  //lambda ~ cauchy(0, tau); 
  //tau ~ cauchy(0, sigma);
  sigma ~ cauchy(0,10);
  eta ~ cauchy(0, 10); 
  theta ~ normal(0, 1); 
  beta_0 ~ cauchy(0,10);
  beta ~ normal(Z*theta, eta);
  y_train ~ normal(X_beta, sigma);
}

generated quantities{
  //Post-treatment prediction & Log-likelihood
  vector[N_train] y_fit; //Fitted synthetic control unit in the pre-treatment
  vector[N_test] y_test; //Predicted synthetic control unit in the post-treatment
  vector[N_train] log_lik; //Log-likelihood
  y_fit = beta_0 + X_train * beta;

  for(i in 1:N_test){
  y_test[i] = normal_rng(beta_0 + X_test[i,] * beta, sigma);
    }

  for (t in 1:N_train) {
  log_lik[t] = normal_lpdf(y_train[t] | y_fit[t], sigma);
    }
}
