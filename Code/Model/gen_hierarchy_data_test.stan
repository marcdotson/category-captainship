//
// This file creates synthetic data to use with bscm_hierarchy_testing model
// Morgan Bale
// April 2022

// Data
data{
  int TT; //Number of total time periods
  int S; //Number of control stores
  int I; //time of intervetion 
  //int K; //number of treated store covariates
  vector[TT] D; //treatment indicator for every time period 
  matrix[TT, S] X; //Control unit store observations in every time period
  //matrix[K, S] Z;  //control store covariates 
}

//gen simluated data
generated quantities {
  vector[TT] alpha; //treatment effect 
  vector[S] beta;          //vector of weights for control stores
  //vector[K] theta;         //effect of store level covariates
  vector[TT] Y; //Treated unit sales in every time period
  real beta_0; //intercept 
  //real<lower=0> sigma;     //variance of the beta equation
  real<lower=0> epsilon;     //variance of the likelihood equation
  
  //for (k in 1:K) {
    //theta[k] = normal_rng(0, 5);
  //}
  
  //sigma=normal_rng(0,5); 
  
  //for (s in 1:S) {
    //alpha[s] = normal_rng(theta'*Z[,pp], sigma);
  //}
  
  epsilon = normal_rng(0, 10);
  beta_0 = normal_rng(0, 10);
  for(a in 1:I) {alpha[a]=0;}
  for(a in (I+1):TT) {alpha[a]=normal_rng(0,5);}
  for(s in 1:S) {beta[s]=normal_rng(0,10);}
  for (t in 1:TT) {
    Y[t] = normal_rng(beta_0 + X[t,]*beta + alpha[t]*D[t], epsilon); //create treated unit in all time periods
    }
    //delta_0=normal_rng(0,5);
    //delta=normal_rng(0,5);
}



