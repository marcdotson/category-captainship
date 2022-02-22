//
// This file creates synthetic data to use with bscm_hierarchy model
// Morgan Bale
// Feb 2022

// Data
data{
  int N_train; //Number of observations in the pre-treatment periods
  int N_test; //Number of observations in the post-treatment periods
  int p; //Number of control units
  int K; //number of control store covariates
  matrix[N_train, p] X_train; //Control unit matrix in the pre-treatment
  matrix[N_test, p] X_test; //Control unit matrix in the post-treatment
  matrix[K, p] Z;  //control store covariates 
}

//gen simluated data
generated quantities {
  vector[p] beta;          //vector of weights for control stores
  vector[K] theta;         //effect of store level covariates
  vector[N_train] y_train; //Treated unit in the pre-treatment periods
  real beta_0; //intercept 
  real<lower=0> sigma;     //variance of the beta equation
  real<lower=0> epsilon;     //variance of the likelihood equation
  
  for (k in 1:K) {
    theta[k] = normal_rng(0, 5);
  }
  
  sigma=normal_rng(0,5); 
  
  for (pp in 1:p) {
    beta[pp] = normal_rng(theta'*Z[,pp], sigma);
  }
  
  epsilon = normal_rng(0, 5);
  beta_0 = normal_rng(0, 5);
  for (n in 1:N_train) {
    y_train[n] = normal_rng(beta_0 + X_train[n,]*beta, epsilon); 
    }

}

