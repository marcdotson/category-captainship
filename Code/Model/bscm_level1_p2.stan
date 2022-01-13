//
// This model comes from Gupta's web index B.1 (adjusted)
// Morgan Bale
// October 2021

// Data
data{
  int N_train; //Number of observations in the pre-treatment periods
  int N_test; //Number of observations in the post-treatment periods
  int S;      //Number of stores
  int B;      //Number of brands 
  matrix[N_train, B] y_train;        //Treated unit in the pre-treatment periods
  matrix[N_train, S] X_train[B];    //Control unit matrix in the pre-treatment
  matrix[N_test, S] X_test[B];       //Control unit matrix in the post-treatment
}

// The parameters accepted by the model. 
parameters{
  real beta_0; //Intercept
  real<lower=0> sigma2; //Error term variance
  matrix[S, B] beta_raw; 
  real<lower=0> tau; //global shrinkage 
  matrix<lower=0, upper=pi()/2>[S, B] lambda_unif; 
}

transformed parameters{
  real<lower=0> sigma; //Error term sd
  matrix<lower=0>[S, B] lambda; //local shrinkage 
  matrix[S, B] beta; //Control unit weights, move to transformed parameters if using beta_raw
  matrix[N_train,B] X_beta; //Synthetic control unit prediction in the pre-treatment period
  sigma = sqrt(sigma2);
  lambda=tau*tan(lambda_unif); 
  
  for(b in 1:B) {
    for(s in 1:S) {
      beta[s, b] = lambda[s, b]*beta_raw[s,b]; 
    }
  }

  for(b in 1:B) {
    //for(s in 1:S) {
    X_beta[,b] = beta_0 + X_train[][b] * beta[,b];
  }//}
}

// The model to be estimated. 
model{
  //Pre-treatment estimation
  tau ~ cauchy(0, sigma); 
  sigma ~ cauchy(0,10);
  beta_0 ~ cauchy(0,10);
  for(b in 1:B) {
    beta_raw[,b] ~ normal(0, 1); 
  }
  for(b in 1:B) {
    y_train[,b] ~ normal(X_beta[,b], sigma); 
  }
}

generated quantities{
  //Post-treatment prediction & Log-likelihood
  matrix[N_train, B] y_fit; //Fitted synthetic control unit in the pre-treatment
  matrix[N_test, B] y_test; //Predicted synthetic control unit in the post-treatment
  matrix[N_train, B] log_lik; //Log-likelihood
  for(b in 1:B) {
    y_fit[,b] = beta_0 + X_train[][b] * beta[,b]; 
  }

  for(i in 1:N_test){
    for(b in 1:B) {
      y_test[i,b] = normal_rng(beta_0 + X_test[,i][b] * beta[,b], sigma);
    }}
    
  for (t in 1:N_train) {
    for(b in 1:B) {
      log_lik[t,b] = normal_lpdf(y_train[t,b] | y_fit[t,b], sigma);
    }}
}

