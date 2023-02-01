// Stage 2 of bscm level1 p1 
//

data {
   int B;   //num of brands 
   int N;   //num post periods
   int b;   //num of brand characteristics
   int s;   //num of store characteristics 
   int St;  //num of treated stores
   matrix[N, B] treat_effect[St]; // treat-synthetic control 
   matrix[b, B] Z; //brand characteristics
   matrix[s, St] X; //store characteristics 
}


parameters {
  real theta_0;
  real<lower=0> sigma;
  vector[b] theta; 
  vector[s] lambda; 
}


model {
  for(st in 1:St) {
    for(n in 1:N) {
      for(bb in 1:B) {
        treat_effect[st, n, bb] ~ normal(theta_0 + theta'*Z[,bb] + lambda'*X[,st], sigma);
      }
    }
  }
}

