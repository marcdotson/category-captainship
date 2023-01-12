// Stage 2 of bscm level1 p1 
//

data {
   int W;    // num weeks in the post period     
   int B;   // num of brands 
   int N;   //num treat effect (B*W)
   int K;   //num of characteristics
   vector[N] treat_effect; // treat-synthetic control 
   matrix[K, N] Z; //store brand characteristics
}


parameters {
  real theta_0;
  real<lower=0> sigma;
  vector[K] theta; 
}


model {
  treat_effect ~ normal(theta_0 + theta'*Z, sigma);
}

