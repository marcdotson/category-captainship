//
// This model comes from Gupta's web index B.1 (adjusted)
// Morgan Bale
// October 2021

// Data
data{
  int N_train; //Number of observations in the pre-treatment periods
  int N_test; //Number of observations in the post-treatment periods
  int S; //Number of control units
  real y_train[N_train]; //Treated unit in the pre-treatment periods
  matrix[N_train, S] X_train; //Control unit matrix in the pre-treatment
  matrix[N_test, S] X_test; //Control unit matrix in the post-treatment
}

// The parameters accepted by the model. 
parameters{
  real beta_0; //Intercept
  real<lower=0> sigma2; //Error term variance
  vector[S] beta_raw; //Control unit weights (will be transformed)
  //Hyperparameters prior
  vector<lower=0, upper=pi()/2>[S] lambda_unif;
  real<lower=0> tau; //Global shrinkage
}

transformed parameters{
  vector[S] beta; //Control unit weights
  real<lower=0> sigma; //Error term sd
  vector<lower=0>[S] lambda; //Local shrinkage
  vector[N_train] X_beta; //Synthetic control unit prediction in the pre-treatment period
  lambda = tau * tan(lambda_unif); // => lambda ~ cauchy(0, tau)
  for(s in 1:S){
  beta[s] = lambda[s] * beta_raw[s];
    }
  sigma = sqrt(sigma2);
  X_beta = beta_0 + X_train*beta;
}

// The model to be estimated. 
model{
  //Pre-treatment estimation
  beta_raw ~ normal(0, 1); //=> beta ~ normal(0, lambda^2)
  tau ~ cauchy(0, sigma);
  sigma ~ cauchy(0,10);
  beta_0 ~ cauchy(0,10);
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

