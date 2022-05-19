//
// This model is adapted from Gupta's paper
// Morgan Bale
// Feb 2022

// Data
data{
  int N_train; //Number of observations in the pre-treatment periods
  int N_test; //Number of observations in the post-treatment periods
  int p; //Number of control units
  int K; //number of control store covariates
  real y_train[N_train]; //Treated unit in the pre-treatment periods
  matrix[N_train, p] X_train; //Control unit matrix in the pre-treatment
  matrix[N_test, p] X_test; //Control unit matrix in the post-treatment
  vector[N_test] y_post; //Treated unit data in the post period
  matrix[K, p] Z;  //control store covariates 
}

// The parameters accepted by the model. 
parameters{
  real<lower=0> sigma; //variance for beta equation
  real<lower=0> epsilon; //variance for likelihood
  vector[p] beta; 
  //Hyperparameters prior
  vector[K] theta;        //hierarchical effect 
  //real<lower=0> tau; //Global shrinkage
  real beta_0; //intercept 
  //vector<lower=0>[p] lambda; //Local shrinkage
}

transformed parameters{
  //vector<lower=0>[p] lambda2; 
  vector[N_train] X_beta; //Synthetic control unit prediction in the pre-treatment period
  X_beta = beta_0 + X_train*beta;
  //lambda2 = lambda .* lambda; 
}

// The model to be estimated. 
model{
  //Pre-treatment estimation
  //lambda ~ cauchy(0, tau); 
  //tau ~ cauchy(0, sigma);
  sigma ~ normal(0,5);
  epsilon ~ normal(0,5); 
  theta ~ normal(0, 5); 
  beta_0 ~ normal(0,5);
  beta ~ normal(theta'*Z, sigma);
  y_train ~ normal(X_beta, epsilon);
}

generated quantities{
  //Post-treatment prediction & Log-likelihood
  vector[N_train] y_fit; //Fitted synthetic control unit in the pre-treatment
  vector[N_test] y_test; //Predicted synthetic control unit in the post-treatment
  vector[N_train] log_lik; //Log-likelihood
  vector[N_test] alpha;
  y_fit = beta_0 + X_train * beta;

  for(i in 1:N_test){
  y_test[i] = normal_rng(beta_0 + X_test[i,] * beta, sigma); //create predicted SC in post treatment period 
    }

  for (t in 1:N_train) {
  log_lik[t] = normal_lpdf(y_train[t] | y_fit[t], sigma); //how well does the SC match actual data pre treatment 
    }
    
  //find treatment effect: alpha for each post period unit (weeks)
  for (i in 1:N_test) {
    alpha[i] = y_post[i] - y_test[i]; 
  }
  
}
