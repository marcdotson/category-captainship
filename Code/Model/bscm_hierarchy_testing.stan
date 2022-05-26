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
  matrix[N,TT] D; //treatment indicator for every time period for every treated store 
  matrix[C, TT] X; //Control unit store observations in every time period
  matrix[K, N] Z;  //treated store covariates 
  matrix[N, TT] Y; //Treated unit sales in every time period
}

// The parameters accepted by the model. 
parameters{
  //real<lower=0> sigma2;      //variance for likelihood and beta prior
  real<lower=0> sigma;   //variance for likelihood
  real<lower=0> nu;        //variance for alpha equation
  //real<lower=0> tau;      //globl shrinkage 
  //vector<lower=0>[C] lambda;  //local shrinkage 
  vector[N] alpha;     //treatment effect 
  vector[K] theta;      //hierarchical effect
  vector[N] theta_0;      //intercept for higher level alpha equation
  matrix[N,C] beta;          //vector of weights for control stores for each treated store
  real beta_0;        //intercept for Y equation
}

//transformed parameters {
  //real<lower=0> sigma;  //error term sd 
  //vector<lower=0>[C] lambda2;  //lambda^2 
  //sigma=sqrt(sigma2); 
  //lambda2=lambda .* lambda; 
//}

// The model to be estimated. 
model{
  //Pre-treatment estimation
  nu ~ inv_gamma(3,1);
  sigma ~ inv_gamma(3,1); 
  theta ~ normal(0, 3); 
  beta_0 ~ cauchy(0,3);
  //lambda ~ cauchy(0, tau); //horseshoe prior stuff 
  //tau ~ cauchy(0, sigma); 
  //sigma ~ cauchy(0, 3); 
  theta_0 ~ normal(0,2);
  for (n in 1:N) {
    beta[n,] ~ normal(0, 1);
    alpha[n] ~ normal(theta_0[n] + theta'*Z[,n], nu);
    for(t in 1:TT) {
    Y[n,t] ~ normal(beta_0 + beta[n,]*X[,t] + alpha[n]*D[n,t], sigma); //likelihood
    }}
}

generated quantities {
  matrix[N,TT] Y_hat; //predicted values
  matrix[N,TT] Y_fit; //predicted synthetic control
  for (n in 1:N) {
    for(t in 1:TT) {
      Y_fit[n,t] = normal_rng(beta_0 + beta[n,]*X[,t], sigma); //synthetic control in all time periods
      Y_hat[n,t] = normal_rng(beta_0 +  beta[n,]*X[,t] + alpha[n]*D[n,t], sigma); //create treated unit in all time periods
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
