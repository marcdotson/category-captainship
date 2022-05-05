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
  matrix[TT, C] X; //Control unit store observations in every time period
  matrix[K, N] Z;  //treated store covariates 
}

//gen simluated data
generated quantities {
  vector[N] alpha;          //treatment effect for each treated store
  vector[K] theta;         //effect of store level covariates
  matrix[N,C] beta;          //vector of weights for control stores for each treated store
  matrix[N, TT] Y;            //Treated unit sales in every time period
  real beta_0;              //intercept 
  real<lower=0> sigma;     //variance of the beta equation
  real<lower=0> epsilon;     //variance of the likelihood equation
  
  for (k in 1:K) {
    theta[k] = normal_rng(0, 5);
  }
  
  sigma=inv_gamma_rng(2,1); 
  epsilon = inv_gamma_rng(2,1);
  beta_0 = normal_rng(0, 10);
  for(n in 1:N) {
    alpha[n]=normal_rng(theta'*Z[,n], sigma);
    for(c in 1:C) {
      beta[n,c]=normal_rng(0,10);
    }
  }
  for (n in 1:N) {
    for(t in 1:TT) {
    Y[n,t] = normal_rng(beta_0 + X[t,]*beta[n,]' + alpha[n]*D[n,t], epsilon); //create treated unit in all time periods
    }}
}



