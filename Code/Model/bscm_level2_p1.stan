//
// This model comes from Gupta's web index B.1 (adjusted)
// Trying to get store and brand treated effects to use in Stage 2
// Morgan Bale
// Jan 2022

// Data
data{
  int N_train; //Number of observations in the pre-treatment periods
  int N_test; //Number of observations in the post-treatment periods
  int Sc;      //Number of control stores
  int St;      //Number of treated stores
  int B;      //Number of brands 
  matrix[N_train, B] y_train[St];        //Treated unit in the pre-treatment periods
  matrix[N_train, Sc] X_train[B];    //Control unit matrix in the pre-treatment
  matrix[N_test, Sc] X_test[B];       //Control unit matrix in the post-treatment
}

// The parameters accepted by the model. 
parameters{
  real beta_0; //Intercept
  real<lower=0> sigma2; //Error term variance
  matrix[Sc, B] beta[St]; //Control unit weights, move to transformed parameters if using beta_raw
  real<lower=0> tau; //global shrinkage 
  matrix<lower=0>[Sc, B] lambda; //local shrinkage 
}

transformed parameters{
  real<lower=0> sigma; //Error term sd
  matrix<lower=0>[B, B] lambda2; 
  matrix[N_train,B] X_beta[St]; //Synthetic control unit prediction in the pre-treatment period
  sigma = sqrt(sigma2);
  lambda2 = lambda' * lambda; 
  
  for(b in 1:B) {
    for(s in 1:St) {
      X_beta[s,,b] = beta_0 + X_train[b,,]*beta[s,,b];
    }
  }
}

// The model to be estimated. 
model{
  //Pre-treatment estimation
  tau ~ cauchy(0, sigma); 
  sigma ~ cauchy(0,10);
  beta_0 ~ cauchy(0,10);
  for(b in 1:B) {
    lambda[,b] ~ cauchy(0, tau); 
    for(s in 1:St) {
      beta[s,,b] ~ normal(0, lambda2[b,b]); 
    }
  }
  for(b in 1:B) {
    for(s in 1:St) {
    y_train[,b][s] ~ normal(X_beta[,b][s], sigma); 
    }
  }
}

generated quantities{
  //Post-treatment prediction & Log-likelihood
  matrix[N_train, B] y_fit[St]; //Fitted synthetic control unit in the pre-treatment
  matrix[N_test, B] y_test[St]; //Predicted synthetic control unit in the post-treatment
  matrix[N_train, B] log_lik[St]; //Log-likelihood
  for(b in 1:B) {
    for(s in 1:St) {
      y_fit[s,,b] = beta_0 + X_train[b,,]*beta[s,,b]; 
    }
  }

  for(i in 1:N_test){
    for(b in 1:B) {
      for(s in 1:St) {
         y_test[s,i,b] = normal_rng(beta_0 + X_test[,i][b] * beta[s,,b], sigma);
      }
    }
  }
    
  for (t in 1:N_train) {
    for(b in 1:B) {
      for(s in 1:St) {
         log_lik[s,t,b] = normal_lpdf(y_train[s,t,b] | y_fit[s,t,b], sigma);
      }
    }
  }
}

