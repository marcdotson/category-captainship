Synthetic Control Model Testing
================
Morgan and Cameron Bale
4/29/2022

The purpose of this file is to test how including the treatment effect
(alpha) influences the model

##### DATA

Make synthetic data

``` r
TT=80
C=20                   #num control stores --> assuming all treated stores use same control store pool 
K=3                   #num of store covariates in alpha equation 
N=15                   #num of treated stores 
I=40       #week of intervention for each treated store

#X is the sales for the control stores in all time periods
X <- matrix(rexp(TT*C), nrow = TT, ncol = C)  

D <- matrix(NA, nrow=N, ncol=TT)
for(n in 1:N) {
  zero <- rep(0, times=I)
  one <- rep(1, times=(TT-I))
  D[n,] <- c(zero, one)
}
  
Z <- matrix(rbinom(K*N, 1, .3), nrow=K, ncol=N)

#put in list for stan
sim_values <- list(TT=TT, C=C, N=N, K=K, X=X, D=D, Z=Z)
```

Generate data in stan: the first stan model printed here is to generate
synthetic data using stan to test our actual model

    ## S4 class stanmodel 'gen_hierarchy_data_test' coded as follows:
    ## //
    ## // This file creates synthetic data to use with bscm_hierarchy_testing model
    ## // Morgan Bale
    ## // May 2022
    ## 
    ## // Data
    ## data{
    ##   int TT; //Number of total time periods
    ##   int C; //Number of control stores
    ##   int N; //number of treated stores 
    ##   int K; //number of treated store covariates
    ##   matrix[N, TT] D; //treatment indicator for every time period for every treated store 
    ##   matrix[TT, C] X; //Control unit store observations in every time period
    ##   matrix[K, N] Z;  //treated store covariates 
    ## }
    ## 
    ## //gen simluated data
    ## generated quantities {
    ##   vector[N] alpha;          //treatment effect for each treated store
    ##   vector[K] theta;         //effect of store level covariates
    ##   matrix[N,C] beta;          //vector of weights for control stores for each treated store
    ##   matrix[N, TT] Y;            //Treated unit sales in every time period
    ##   real beta_0;              //intercept 
    ##   real<lower=0> sigma;     //variance of the beta equation
    ##   real<lower=0> epsilon;     //variance of the likelihood equation
    ##   
    ##   for (k in 1:K) {
    ##     theta[k] = normal_rng(0, 10);
    ##   }
    ##   
    ##   sigma=inv_gamma_rng(2,1); 
    ##   epsilon = inv_gamma_rng(2,1);
    ##   beta_0 = normal_rng(0, 10);
    ##   for(n in 1:N) {
    ##     alpha[n]=normal_rng(theta'*Z[,n], sigma);
    ##     for(c in 1:C) {
    ##       beta[n,c]=normal_rng(0,10);
    ##     }
    ##   }
    ##   for (n in 1:N) {
    ##     for(t in 1:TT) {
    ##     Y[n,t] = normal_rng(beta_0 + X[t,]*beta[n,]' + alpha[n]*D[n,t], epsilon); //create treated unit in all time periods
    ##     }}
    ## }
    ## 
    ## 
    ## 

    ## 
    ## SAMPLING FOR MODEL 'gen_hierarchy_data_test' NOW (CHAIN 1).
    ## Chain 1: Iteration: 1 / 1 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0 seconds (Warm-up)
    ## Chain 1:                0.000514 seconds (Sampling)
    ## Chain 1:                0.000514 seconds (Total)
    ## Chain 1:

##### MODEL

This model uses a synthetic control combined with heterogeneous
treatment effects to estimate sales for treated stores. The model is
restricted to meet the synthetic control requirements in the pre period,
so that the difference between the synthetic control and actual sales in
the post period can be attributed to alpha, our treatment effect. alpha
is described by store/category characteristics making theta our effect
of those characteristics on the average treatment effect alpha. Note:
alpha is an average treatment effect across all post period weeks. Each
treated store gets its own weights determined from the same sample of
control stores. In this example, we have 15 treated stores and 20
control stores. So each treated store gets its own synthetic control
from the 20 control stores. This form of the model would allow us to
still use horseshoe priors on beta, but those are not implemented here
yet.

    ## S4 class stanmodel 'bscm_hierarchy_testing' coded as follows:
    ## //
    ## // This model is adapted from Gupta's paper
    ## // Morgan Bale
    ## // May 2022
    ## 
    ## // Data
    ## data{
    ##   int TT; //Number of total time periods
    ##   int C; //Number of control stores
    ##   int N; //number of treated stores 
    ##   int K; //number of treated store covariates
    ##   matrix[N, TT] D; //treatment indicator for every time period for every treated store 
    ##   matrix[TT, C] X; //Control unit store observations in every time period
    ##   matrix[K, N] Z;  //treated store covariates 
    ##   matrix[N, TT] Y; //Treated unit sales in every time period
    ## }
    ## 
    ## // The parameters accepted by the model. 
    ## parameters{
    ##   real<lower=0> sigma;      //variance for alpha equation
    ##   real<lower=0> epsilon;   //variance for likelihood
    ##   vector[N] alpha;     //treatment effect 
    ##   vector[K] theta;      //hierarchical effect
    ##   matrix[N,C] beta;          //vector of weights for control stores for each treated store
    ##   real beta_0;        //intercept 
    ## }
    ## 
    ## // transformed parameters{
    ## //   vector[TT] X_beta_alpha; //likelihood equation 
    ## //   X_beta_alpha = beta_0 + X*beta + alpha' *D;
    ## // }
    ## 
    ## // The model to be estimated. 
    ## model{
    ##   //Pre-treatment estimation
    ##   sigma ~ inv_gamma(3,1);
    ##   epsilon ~ inv_gamma(3,1); 
    ##   theta ~ normal(0, 5); 
    ##   beta_0 ~ normal(0,10);
    ##   //beta ~ normal(0,10); 
    ##   alpha ~ normal(theta'*Z, sigma);
    ##   for (n in 1:N) {
    ##     beta[n,] ~ normal(0, 10);
    ##     for(t in 1:TT) {
    ##     Y[n,t] ~ normal(beta_0 + X[t,]*beta[n,]' + alpha[n]*D[n,t], epsilon); //likelihood
    ##     }}
    ## }
    ## 
    ## generated quantities {
    ##   matrix[N,TT] Y_hat; //predicted values 
    ## 
    ##   for (n in 1:N) {
    ##     for(t in 1:TT) {
    ##     Y_hat[n,t] = normal_rng(beta_0 + X[t,]*beta[n,]' + alpha[n]*D[n,t], epsilon); //create treated unit in all time periods
    ##     }}
    ## }
    ## 
    ## // generated quantities{
    ## //   //Post-treatment prediction & Log-likelihood
    ## //   vector[N_train] y_fit; //Fitted synthetic control unit in the pre-treatment
    ## //   vector[N_test] y_test; //Predicted synthetic control unit in the post-treatment
    ## //   vector[N_train] log_lik; //Log-likelihood
    ## //   vector[N_test] alpha;
    ## //   y_fit = beta_0 + X_train * beta;
    ## // 
    ## //   for(i in 1:N_test){
    ## //   y_test[i] = normal_rng(beta_0 + X_test[i,] * beta, sigma); //create predicted SC in post treatment period 
    ## //     }
    ## // 
    ## //   for (t in 1:N_train) {
    ## //   log_lik[t] = normal_lpdf(y_train[t] | y_fit[t], sigma); //how well does the SC match actual data pre treatment 
    ## //     }
    ## //     
    ## //   //find treatment effect: alpha for each post period unit (weeks)
    ## //   for (i in 1:N_test) {
    ## //     alpha[i] = y_post[i] - y_test[i]; 
    ## //   }
    ## //   
    ## // }

###### RESULTS

Check results: the traceplots look good, the sampler recovers the beta
and theta parameters. The synthetic control matches the treatment group
in the pre period.
![](../Figures/bscm/hierarchy-recovery-1.png)<!-- -->![](../Figures/bscm/hierarchy-recovery-2.png)<!-- -->![](../Figures/bscm/hierarchy-recovery-3.png)<!-- -->![](../Figures/bscm/hierarchy-recovery-4.png)<!-- -->![](../Figures/bscm/hierarchy-recovery-5.png)<!-- -->![](../Figures/bscm/hierarchy-recovery-6.png)<!-- -->![](../Figures/bscm/hierarchy-recovery-7.png)<!-- -->![](../Figures/bscm/hierarchy-recovery-8.png)<!-- -->![](../Figures/bscm/hierarchy-recovery-9.png)<!-- -->![](../Figures/bscm/hierarchy-recovery-10.png)<!-- -->![](../Figures/bscm/hierarchy-recovery-11.png)<!-- -->![](../Figures/bscm/hierarchy-recovery-12.png)<!-- -->![](../Figures/bscm/hierarchy-recovery-13.png)<!-- -->

Predicted Y (yhat) v Y: yhat is created in the generated quantities
section of `bscm_hierarchy_testing.stan`  
![](../Figures/bscm/unnamed-chunk-4-1.png)<!-- -->![](../Figures/bscm/unnamed-chunk-4-2.png)<!-- -->![](../Figures/bscm/unnamed-chunk-4-3.png)<!-- -->

Synthetic control v Y: compare the synthetic control for selected
treated stores to the actual sales (Y) for those treated stores
![](../Figures/bscm/hierarchy-pre-treatment-1.png)<!-- -->![](../Figures/bscm/hierarchy-pre-treatment-2.png)<!-- -->![](../Figures/bscm/hierarchy-pre-treatment-3.png)<!-- -->![](../Figures/bscm/hierarchy-pre-treatment-4.png)<!-- -->![](../Figures/bscm/hierarchy-pre-treatment-5.png)<!-- -->

Compare true alphas to estimated alphas:

![](../Figures/bscm/unnamed-chunk-5-1.png)<!-- --> Test selected:

Compare estimated average values for alpha to the true alpha in treated
store 1

``` r
sc_data <- sc_data %>% mutate(alpha=y-sc)

#this is the treatment effect (alpha) in store 1 in the PRE PERIOD. we expect this to be 0, anything above 0 is noise? 
store1_pre <- sc_data %>% filter(treat_store==1, week<(I[1]+1)) 
mean(store1_pre$alpha)
```

    ## [1] 0.01276064

``` r
#this is the treatment effect (alpha) in store 1 in the POST period. we expect this to be equal to the true alpha 
store1_post <- sc_data %>% filter(treat_store==1, week>I[1]) 
mean(store1_post$alpha)
```

    ## [1] 13.93839

``` r
#compare to true alpha for store 1
sim_alpha[1]
```

    ## [1] 13.70962

Compare estimated average values for alpha to the true alpha in treated
store 5

``` r
#this is the treatment effect (alpha) in store 5 in the PRE PERIOD. we expect this to be 0, anything above 0 is noise? 
store1_pre <- sc_data %>% filter(treat_store==5, week<(I[1]+1)) 
mean(store1_pre$alpha)
```

    ## [1] 0.01243866

``` r
#this is the treatment effect (alpha) in store 5 in the POST period. we expect this to be equal to the true alpha
store1_post <- sc_data %>% filter(treat_store==5, week>I[1]) 
mean(store1_post$alpha)
```

    ## [1] 0.4252446

``` r
#compare to true alpha for store 5
sim_alpha[5]
```

    ## [1] 0.1730932
