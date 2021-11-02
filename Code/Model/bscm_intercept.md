Bayesian Synthetic Control with intercept
================
Morgan Bale
10/28/2021

The purpose of this file is to add an intercept to the Bayesian
synthetic control method tested in `testing_bscm`. The model comes from
the web index of the Gupta et al, part B.1 BSCM-Horseshoe.

##### DATA

Function for making synthetic data: values picked from simulation
studies done in Gupta et al

``` r
gen_b1_data <- function(N_train=40,     #num of obs in pre treatment
                        N_test=40,      #num of obs in post treatment 
                        p=5,            #num of control units
                        tau=0.01,        #global shrinkage 
                        mu=c(15, 35, 10, 20, 30), 
                        beta_0=5
                        ) {
  
  # data for control units
  X_train <- matrix(NA, nrow = N_train, ncol = p)
  
  X_test <- matrix(NA, nrow=N_test, ncol=p)            #control unit matrix in post treatment 
  
  for(j in 1:p) {
    X_train[,j] <- rnorm(N_train, mean=mu[j], sd=10)     #control unit matrix in pre treatment
    X_test[, j] <- rnorm(N_test, mean=mu[j], sd=10)
  }
  
  
  
  #lambda_unif <- runif(p, min=0, max=pi/2)         #hyper parameter prior 
  #lambda <- tau * tan(lambda_unif)                     #local shrinkage 
  
  #beta_raw <- c(.2, .8, 0, 0, 0)                            #control unit weights before transformation 
  #beta <- c()                                    #control unit weights
  #for(j in 1:p) {
    #beta[j] = lambda[j] * beta_raw[j]
  #}
  
  beta <- c(-.5, 2, 0, 0, 0) 
  
  #model 
  epsilon <- rnorm(N_train, mean=0, sd=1)
  y_train <- beta_0 + X_train%*%beta + epsilon

  #make a list for stan
  list(N_train=N_train, N_test=N_test, p=p, tau=tau, X_train=X_train, X_test=X_test, beta=beta, y_train=as.vector(y_train), beta_0=beta_0)}
```

Create synthetic data

``` r
set.seed(2020)
b1_data <- gen_b1_data()
#str(b1_data)
```

###### MODEL

Run model using `bcsm_b1_intercept.stan`

``` r
b1_model <- stan_model(file="bscm_b1_intercept.stan")
```

    ## hash mismatch so recompiling; make sure Stan code ends with a blank line

``` r
print(b1_model)
```

    ## S4 class stanmodel 'bscm_b1_intercept' coded as follows:
    ## //
    ## // This model comes from Gupta's web index B.1
    ## // Morgan Bale
    ## // October 2021
    ## 
    ## // Data
    ## data{
    ##   int N_train; //Number of observations in the pre-treatment periods
    ##   int N_test; //Number of observations in the post-treatment periods
    ##   int p; //Number of control units
    ##   real y_train[N_train]; //Treated unit in the pre-treatment periods
    ##   matrix[N_train, p] X_train; //Control unit matrix in the pre-treatment
    ##   matrix[N_test, p] X_test; //Control unit matrix in the post-treatment
    ## }
    ## 
    ## // The parameters accepted by the model. 
    ## parameters{
    ##   real<lower=0> sigma2; //Error term variance
    ##   //vector[p] beta_raw; //Control unit weights (will be transformed)
    ##   vector[p] beta; 
    ##   //Hyperparameters prior
    ##   //vector<lower=0, upper=pi()/2>[p] lambda_unif;
    ##   //real<lower=0> tau; //Global shrinkage
    ##   real beta_0; //intercept 
    ## }
    ## 
    ## transformed parameters{
    ##   //vector[p] beta; //Control unit weights
    ##   real<lower=0> sigma; //Error term sd
    ##   //vector<lower=0>[p] lambda; //Local shrinkage
    ##   vector[N_train] X_beta; //Synthetic control unit prediction in the pre-treatment period
    ##   //lambda = tau * tan(lambda_unif); // => lambda ~ cauchy(0, tau)
    ##   //for(j in 1:p){
    ##   //beta[j] = lambda[j] * beta_raw[j];
    ##     //}
    ##   sigma = sqrt(sigma2);
    ##   //X_beta = beta_0 + X_train*beta;
    ##   X_beta = beta_0 + X_train*beta;
    ## }
    ## 
    ## // The model to be estimated. 
    ## model{
    ##   //Pre-treatment estimation
    ##   //beta_raw ~ normal(0, 1); //=> beta ~ normal(0, lambda^2)
    ##   beta ~ normal(0, 1);
    ##   //tau ~ cauchy(0, sigma);
    ##   sigma ~ cauchy(0,10);
    ##   beta_0 ~ cauchy(0,10);
    ##   y_train ~ normal(X_beta, sigma);
    ## }
    ## 
    ## generated quantities{
    ##   //Post-treatment prediction & Log-likelihood
    ##   vector[N_train] y_fit; //Fitted synthetic control unit in the pre-treatment
    ##   vector[N_test] y_test; //Predicted synthetic control unit in the post-treatment
    ##   vector[N_train] log_lik; //Log-likelihood
    ##   //y_fit = beta_0 + X_train * beta;
    ##   y_fit = beta_0 + X_train * beta;
    ##   for(i in 1:N_test){
    ##   y_test[i] = normal_rng(X_test[i,] * beta, sigma);
    ##     }
    ##   //for(i in 1:N_test){
    ##   //y_test[i] = normal_rng(beta_0 + X_test[i,] * beta, sigma);
    ##     //}
    ##   for (t in 1:N_train) {
    ##   log_lik[t] = normal_lpdf(y_train[t] | y_fit[t], sigma);
    ##     }
    ## }
    ## 

``` r
draws <- sampling(b1_model, data=b1_data, seed=2020, cores=3)
```

###### RESULTS

Check results

``` r
#traceplots
traceplot(draws, pars="beta")
```

![](bscm_intercept_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
traceplot(draws, pars="beta_0")
```

![](bscm_intercept_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
traceplot(draws, pars="sigma")
```

![](bscm_intercept_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

``` r
mcmc_recover_hist(As.mcmc.list(draws, pars="beta"), true=as.vector(t(b1_data$beta)))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](bscm_intercept_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
mcmc_recover_hist(As.mcmc.list(draws, pars="beta_0"), true=as.vector(t(b1_data$beta_0)))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](bscm_intercept_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

Fitted Synthetic Control for pre treatment

``` r
#synthetic control for pre treatment
y_fit <- summary(draws, pars="y_fit")

sc_pre <- tibble(y_fit[[1]][,1])

lower <- y_fit[[1]][,4]
upper <- y_fit[[1]][,8]

sc_pre <- sc_pre %>% bind_cols(lower, upper)
```

    ## New names:
    ## * NA -> ...2
    ## * NA -> ...3

``` r
sc_pre <- sc_pre %>% mutate(week=rep(1:b1_data$N_train))

names(sc_pre) <- c("synthetic_control","lower", "upper", "week")

#sc_pre %>% ggplot(aes(x=week, y=synthetic_control))+ geom_ribbon(aes(ymin=lower, ymax=upper), fill="gray80") + geom_line() + ggtitle("Synthetic Control in the Pre Treatment") + labs(x="Week", y="Control Observations") 
```

Treated unit in the pre treatment

``` r
y_train <- b1_data$y_train

sc_data <- sc_pre %>% bind_cols(y_train)
```

    ## New names:
    ## * NA -> ...5

``` r
names(sc_data)[5] <- "treatment_group"

sc_data %>% ggplot(aes(x=week))+ geom_ribbon(aes(ymin=lower, ymax=upper), fill="gray80") + geom_line(aes(y=treatment_group), color="darkred") + geom_line(aes(y=synthetic_control), color="steelblue") + 
  labs(x="Week", y="Pre Treatment Value") + ggtitle("Pre Treatment Synthetic Control (blue) vs Treatment Group (red)") 
```

![](bscm_intercept_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Predicted Synthetic Control for post treatment

``` r
y_test <- summary(draws, pars="y_test")

sc_post <- tibble(y_test[[1]][,1])

lower <- y_test[[1]][,4]
upper <- y_test[[1]][,8]

sc_post <- sc_post %>% bind_cols(lower, upper)
```

    ## New names:
    ## * NA -> ...2
    ## * NA -> ...3

``` r
sc_post <- sc_post %>% mutate(week=rep((b1_data$N_train+1):(b1_data$N_train+b1_data$N_test)))

names(sc_post) <- c("synthetic_control", "lower", "upper", "week")

#sc_post %>% ggplot(aes(x=week, y=synthetic_control)) + geom_ribbon(aes(ymin=lower, ymax=upper), fill="gray80") + geom_line() + ggtitle("Synthetic Control in the Post Treatment") + labs(x="Week", y="Control Value")
#gray is 95% CI 
```

Make treatment data for post period: this code only works if number of
post treatment periods=number pre treatment periods

``` r
y_post <- y_train + 30
y_post <- y_post[1:b1_data$N_test]

y_post <- as_tibble(y_post) %>% mutate(week=rep((b1_data$N_train+1):(b1_data$N_train+b1_data$N_test)))

sc_post <- sc_post %>% left_join(y_post, by="week")

names(sc_post)[5] <- "treatment_group"

total_sc_data <- sc_data %>% bind_rows(sc_post)

total_sc_data %>% ggplot(aes(x=week)) + geom_ribbon(aes(ymin=lower, ymax=upper), fill="gray80") + geom_line(aes(y=treatment_group), color="darkred") + geom_line(aes(y=synthetic_control), color="steelblue") +
  labs(x="Week", y="Value") + ggtitle("Synthetic Control (blue) vs Treatment Group (red)") + geom_vline(xintercept=b1_data$N_train) 
```

![](bscm_intercept_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
