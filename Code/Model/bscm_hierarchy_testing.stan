//
// This model is adapted from Gupta's paper
// Morgan Bale
// Feb 2022

// Data
data{
  int TT; //Number of total time periods
  int S; //Number of control stores
  //int K; //number of treated store covariates
  vector[TT] Y; //Treated unit sales in every time period
  vector[TT] D; //treatment indicator for every time period 
  matrix[TT, S] X; //Control unit store observations in every time period
  //matrix[K, S] Z;  //control store covariates 
}

// The parameters accepted by the model. 
parameters{
  //real<lower=0> sigma; //variance for alpha equation
  real<lower=0> epsilon; //variance for likelihood
  vector[TT] alpha; //treatment effect 
  vector[S] beta; //synthetic control weights
  //Hyperparameters prior
  //vector[K] theta;        //hierarchical effect 
  real beta_0; //intercept 
}

transformed parameters{
  vector[TT] X_beta_alpha; //likelihood equation 
  X_beta_alpha = beta_0 + X*beta + alpha' *D;
}

// The model to be estimated. 
model{
  //Pre-treatment estimation
  //sigma ~ normal(0,5);
  epsilon ~ normal(0,10); 
  //theta ~ normal(0, 5); 
  beta_0 ~ normal(0,10);
  beta ~ normal(0,10); 
  //alpha ~ normal(0, 5);
  //alpha ~ normal(theta'*Z, sigma);
  Y ~ normal(X_beta_alpha, epsilon);
}

generated quantities {
  vector[TT] Y_hat; //predicted values 
  vector[TT] synth_control; 
  
  synth_control = beta_0 + X*beta;
  for(t in 1:TT) {
    Y_hat[t] = normal_rng(X_beta_alpha[t], epsilon);
  }
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
