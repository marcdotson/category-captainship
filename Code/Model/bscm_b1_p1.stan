//
// This model comes from Gupta's web index B.1
// Morgan Bale
// October 2021

// Data
data{
  int N_train; //Number of observations in the pre-treatment periods
  int N_test; //Number of observations in the post-treatment periods
  int p; //Number of control units
  real y_train[N_train]; //Treated unit in the pre-treatment periods
  matrix[N_train, p] X_train; //Control unit matrix in the pre-treatment
  matrix[N_test, p] X_test; //Control unit matrix in the post-treatment
}

// The parameters accepted by the model. 
parameters{
  real<lower=0> sigma2; //Error term variance
  vector[p] beta; 
  //Hyperparameters prior
  real<lower=0> tau; //Global shrinkage
  vector<lower=0>[p] lambda; //Local shrinkage
}

transformed parameters{
  //vector[p] beta; //Control unit weights
  real<lower=0> sigma; //Error term sd
  vector<lower=0>[p] lambda2; //Local shrinkage squared
  vector[N_train] X_beta; //Synthetic control unit prediction in the pre-treatment period
  sigma = sqrt(sigma2);
  X_beta = X_train*beta;
  lambda2 = lambda .* lambda; 
}

// The model to be estimated. 
model{
  //Pre-treatment estimation
  beta ~ normal(0, lambda2);
  lambda ~ cauchy(0, tau); 
  tau ~ cauchy(0, sigma);
  sigma ~ cauchy(0,10);
  y_train ~ normal(X_beta, sigma);
}

generated quantities{
  //Post-treatment prediction & Log-likelihood
  vector[N_train] y_fit; //Fitted synthetic control unit in the pre-treatment
  vector[N_test] y_test; //Predicted synthetic control unit in the post-treatment
  vector[N_train] log_lik; //Log-likelihood

  y_fit = X_train * beta;
  
  for(i in 1:N_test){
  y_test[i] = normal_rng(X_test[i,] * beta, sigma);
    }

  for (t in 1:N_train) {
  log_lik[t] = normal_lpdf(y_train[t] | y_fit[t], sigma);
    }
}

