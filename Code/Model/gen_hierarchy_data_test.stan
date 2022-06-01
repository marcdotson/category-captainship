//
// This file creates synthetic data to use with bscm_hierarchy_testing model
// Morgan Bale
// May 2022

// Data
data{
  int TT; //Number of total time periods
  int C; //Number of control stores
  int N; //number of treated stores 
  int K; //number of treated store covariates
  matrix[N, TT] D; //treatment indicator for every time period for every treated store 
  matrix[C, TT] X; //Control unit store observations in every time period
  matrix[K, N] Z;  //treated store covariates 
}

//gen simluated data
generated quantities {
  vector[N] alpha;          //treatment effect for each treated store
  vector[K] theta;         //effect of store level covariates
  vector[N] theta_0;            //intercept for alpha equation 
  matrix[N,C] beta;          //vector of weights for control stores for each treated store
  matrix[N, TT] Y;            //Treated unit sales in every time period
  real beta_0;              //intercept 
  real<lower=0> nu;     //variance of the beta equation
  real<lower=0> sigma;     //variance of the likelihood equation
  
  for (k in 1:K) {
    theta[k] = normal_rng(0, 3);
    //theta[k] =0;
  }
  sigma=inv_gamma_rng(2,1); 
  nu = inv_gamma_rng(2,1);
  beta_0 = cauchy_rng(0, 3);
  
  for(n in 1:N) {
    theta_0[n] = normal_rng(0, 3);
    alpha[n]=normal_rng(theta_0[n] + theta'*Z[,n], nu);
    for(c in 1:C) {
      beta[n,c]=normal_rng(0,1);
    }
  }
  for (n in 1:N) {
    for(t in 1:TT) {
    Y[n,t] = normal_rng(beta_0 + beta[n,]*X[,t] + alpha[n]*D[n,t], sigma); //create treated unit in all time periods
    }}
}



