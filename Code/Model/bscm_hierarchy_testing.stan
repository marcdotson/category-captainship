//
// This model is adapted from Gupta's paper
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
  matrix[N, TT] Y; //Treated unit sales in every time period
}

// The parameters accepted by the model. 
parameters{
  real<lower=0> sigma;      //variance for alpha equation
  real<lower=0> epsilon;   //variance for likelihood
  vector[N] alpha;     //treatment effect 
  vector[K] theta;      //hierarchical effect
  matrix[N,C] beta;          //vector of weights for control stores for each treated store
  real beta_0;        //intercept 
}

// transformed parameters{
//   vector[TT] X_beta_alpha; //likelihood equation 
//   X_beta_alpha = beta_0 + X*beta + alpha' *D;
// }

// The model to be estimated. 
model{
  //Pre-treatment estimation
  sigma ~ inv_gamma(3,1);
  epsilon ~ inv_gamma(3,1); 
  theta ~ normal(0, 5); 
  beta_0 ~ normal(0,10);
  //beta ~ normal(0,10); 
  alpha ~ normal(theta'*Z, sigma);
  for (n in 1:N) {
    beta[n,] ~ normal(0, 10);
    for(t in 1:TT) {
    Y[n,t] ~ normal(beta_0 + X[t,]*beta[n,]' + alpha[n]*D[n,t], epsilon); //likelihood
    }}
}

generated quantities {
  matrix[N,TT] Y_hat; //predicted values 

  for (n in 1:N) {
    for(t in 1:TT) {
    Y_hat[n,t] = normal_rng(beta_0 + X[t,]*beta[n,]' + alpha[n]*D[n,t], epsilon); //create treated unit in all time periods
    }}
}

// generated quantities{
//   //Post-treatment prediction & Log-likelihood
//   vector[N_train] y_fit; //Fitted synthetic control unit in the pre-treatment
//   vector[N_test] y_test; //Predicted synthetic control unit in the post-treatment
//   vector[N_train] log_lik; //Log-likelihood
//   vector[N_test] alpha;
//   y_fit = beta_0 + X_train * beta;
// 
//   for(i in 1:N_test){
//   y_test[i] = normal_rng(beta_0 + X_test[i,] * beta, sigma); //create predicted SC in post treatment period 
//     }
// 
//   for (t in 1:N_train) {
//   log_lik[t] = normal_lpdf(y_train[t] | y_fit[t], sigma); //how well does the SC match actual data pre treatment 
//     }
//     
//   //find treatment effect: alpha for each post period unit (weeks)
//   for (i in 1:N_test) {
//     alpha[i] = y_post[i] - y_test[i]; 
//   }
//   
// }
