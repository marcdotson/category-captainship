Synthetic Control Model Testing with Cereal (ND)
================
Morgan Bale
5/13/2022

The purpose of this file is to test how our hierarchical model works
with real data. The data was cleaned using the cereal cleaning file. We
use ND to test the model, since we are most sure which retailer was
treated in ND. The data comes from our Google Drive folder “cleaned data
sets” and the model description can be found in
`05_bscm-hierarchy-testing.Rmd`.

##### DATA

Target reset date: Jan 1st 2012, Data years: 2011-2012, CC: General
Mills, Validator: Kellogg, State: ND, Treated Retailer: 903

Load and clean data: there are 4 control stores and 5 treated stores.

``` r
load("../Data/nd_cereal_clean.RData")
#select columns we want
nd_cereal <- nd_cereal %>% dplyr::select(store_code_uc, upc, week_end, units, price, year, upc_ver_uc, parent_code, retailer_code, sales, manufacturer)
```

Create higher level store characteristics

``` r
#standardized number of UPCs --> num_UPC/max(num_UPC)
num_upc <- nd_cereal %>% dplyr::select(store_code_uc, upc) %>% distinct() %>% group_by(store_code_uc) %>% summarise(n=n()) %>% mutate(num_upcs=n/max(n)) #some variation, not a ton...

nd_cereal <- nd_cereal %>% left_join(num_upc, by="store_code_uc")

#variation in prices --> max(price) - min(price) for each store 
price_variation <- nd_cereal %>% dplyr::select(store_code_uc, price) %>% distinct() %>% group_by(store_code_uc) %>% summarise(max_price=max(price), min_price=min(price)) %>% mutate(price_diff=max_price-min_price)

nd_cereal <- nd_cereal %>% left_join(price_variation, by="store_code_uc")
```

There are 53 weeks in 2011 and 52 weeks in 2012. CC is implemented
around Jan 1st, 2012. We will use 2012-01-07 as the first post treatment
period. Find distinct weeks and order by date. Make treatment and post
variable.

``` r
weeks <- nd_cereal %>% dplyr::select(week_end) %>% distinct()

weeks <- weeks[order(as.Date(weeks$week_end, format="%d/%m/%Y")),]

weeks <- weeks %>% mutate(week_num=rep(1:nrow(weeks)))

nd_cereal <- nd_cereal %>% left_join(weeks, by="week_end")

nd_cereal <- nd_cereal %>% mutate(treat=if_else(retailer_code==903, 1, 0))

#separate data out by CC, validator, PL, other and treated versus control 
cc_treat <- nd_cereal %>% filter(manufacturer=="GM", treat==1)
cc_control <- nd_cereal %>% filter(manufacturer=="GM", treat==0)

pl_treat <- nd_cereal %>% filter(manufacturer=="PL", treat==1)
pl_control <- nd_cereal %>% filter(manufacturer=="PL", treat==0)

#aggregate sales to store level instead of UPC level 
pl_control_sales <- pl_control %>% group_by(store_code_uc, week_num) %>% summarise(total_sales=sum(sales)) #420 obs, 105 weeks for each of 4 stores
```

    ## `summarise()` has grouped output by 'store_code_uc'. You can override using the `.groups` argument.

``` r
#add store numbers
treat_stores <- unique(pl_treat$store_code_uc)
pl_treat <- pl_treat %>% mutate(store_num=if_else(store_code_uc==790659, 1, if_else(store_code_uc==791868, 2,
                                                                        if_else(store_code_uc==7326857, 3, 
                                                                                if_else(store_code_uc==7329996, 4, 5)))))

#aggregate sales to store level instead of UPC level 
pl_treat_sales <- pl_treat %>% group_by(store_num, week_num) %>% summarise(total_sales=sum(sales)) #525 obs, 105 weeks for each of 5 stores
```

    ## `summarise()` has grouped output by 'store_num'. You can override using the `.groups` argument.

Collect data & test with PL first

``` r
TT <- nrow(weeks)  #num of time periods in total 
C <- length(unique(pl_control$store_code_uc))  #num of control stores
N <- length(unique(pl_treat$store_code_uc))  #num of treated stores

#X is a TTxC matrix of control store sales 
X <- matrix(NA, nrow=TT, ncol=C)
control_stores <- unique(pl_control_sales$store_code_uc)
k=1
for(c in control_stores) {
    X[,k] <- pl_control_sales %>% filter(store_code_uc==c) %>% pull(total_sales)
    k=k+1
}

#create Y NxTT matrix
Y <- matrix(NA, nrow=N, ncol=TT)
for(n in 1:N) {
  Y[n,] <- pl_treat_sales %>% filter(store_num==n) %>% pull(total_sales)
}

#create Z KxN matrix 
tZ <- pl_treat %>% dplyr::select(store_num, price_diff, num_upcs) %>% distinct() %>% dplyr::select(price_diff, num_upcs) %>% as.matrix()
Z <- t(tZ)
K <- nrow(Z)

#create D a NxTT matrix for which time periods a treated store is treated in
#week 54 starts post period 
D_prep <- pl_treat_sales %>% mutate(post=if_else(week_num %in% c(1:53), 0, 1)) %>% dplyr::select(store_num, post)
D <- matrix(NA, nrow=N, ncol=TT)
for(n in 1:N) {
  D[n,] <- D_prep %>% filter(store_num==n) %>% pull(post)
}

b1_data <- list(TT=TT, C=C, N=N, X=X, K=K, Z=Z, D=D, Y=Y) #data list for stan
```

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
control stores. In this example, we have 5 treated stores and 4 control
stores. So each treated store gets its own synthetic control from the 4
control stores. This form of the model would allow us to still use
horseshoe priors on beta, but those are not implemented here yet.

    ## hash mismatch so recompiling; make sure Stan code ends with a blank line

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
    ##   theta ~ normal(0, 10); 
    ##   //beta_0 ~ normal(0,10);
    ##   //beta ~ normal(0,10); 
    ##   //alpha ~ normal(theta'*Z, sigma);
    ##   for (n in 1:N) {
    ##     //beta[n,] ~ normal(0, 10);
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

Check results
![](../Figures/bscm/hierarchy-recovery-1.png)<!-- -->![](../Figures/bscm/hierarchy-recovery-2.png)<!-- -->![](../Figures/bscm/hierarchy-recovery-3.png)<!-- -->![](../Figures/bscm/hierarchy-recovery-4.png)<!-- -->![](../Figures/bscm/hierarchy-recovery-5.png)<!-- -->![](../Figures/bscm/hierarchy-recovery-6.png)<!-- -->

    ## Inference for Stan model: bscm_hierarchy_testing.
    ## 4 chains, each with iter=2000; warmup=1000; thin=1; 
    ## post-warmup draws per chain=1000, total post-warmup draws=4000.
    ## 
    ##            mean se_mean    sd    2.5%     25%    50%    75%  97.5% n_eff Rhat
    ## alpha[1]   1.33    0.46 29.02  -55.72  -18.40   1.48  21.25  58.10  4032    1
    ## alpha[2]  66.44    0.44 29.89    8.17   46.40  67.11  86.30 125.87  4655    1
    ## alpha[3] -23.98    0.44 29.29  -82.63  -44.18 -24.00  -4.10  32.43  4483    1
    ## alpha[4]  -6.29    0.46 29.69  -64.74  -26.22  -6.53  13.36  51.50  4241    1
    ## alpha[5] -96.90    0.44 29.46 -155.72 -115.80 -97.42 -77.51 -39.05  4405    1
    ## theta[1]  -0.03    0.12 10.20  -19.82   -7.03   0.05   7.12  19.33  7395    1
    ## theta[2]  -0.22    0.12 10.02  -19.93   -6.80  -0.10   6.50  19.16  6670    1
    ## 
    ## Samples were drawn using NUTS(diag_e) at Fri May 13 15:51:04 2022.
    ## For each parameter, n_eff is a crude measure of effective sample size,
    ## and Rhat is the potential scale reduction factor on split chains (at 
    ## convergence, Rhat=1).

Synthetic control v Y: compare the synthetic control for selected
treated stores to the actual sales (Y) for those treated stores
![](../Figures/bscm/hierarchy-pre-treatment-1.png)<!-- -->![](../Figures/bscm/hierarchy-pre-treatment-2.png)<!-- -->![](../Figures/bscm/hierarchy-pre-treatment-3.png)<!-- -->![](../Figures/bscm/hierarchy-pre-treatment-4.png)<!-- -->![](../Figures/bscm/hierarchy-pre-treatment-5.png)<!-- -->
