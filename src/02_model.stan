data {
  int<lower=0> N;
  int<lower=0> I;
  int<lower=0> T;
  int<lower=0> N2ID[N];
  int<lower=0> N2TIME[N];
  vector[N] X;
  vector[N] X_lag1;
  vector[N] X_lag2;
  vector[N] X_lag3;
  vector[N] Y;
}

parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
  real<lower=0> sigma_mu;
  real<lower=0> sigma_y;
  
  // real mu[T, I];
  real mu[N];

}

transformed parameters {
  // real nu[T, I];
  real nu[N];
  
  // for (t in 1:T) {
  //   nu[t, 1] = mu[t, 1] + alpha*(X[t]) - beta*(X[T+t]);
  //   nu[t, 2] = mu[t, 2] + alpha*(X[T+t]) - beta*(X[t]);
  // }
  
  for (n in 1:N) {
    if (n <= T) {
      nu[n] = mu[n] +  alpha*(X[n]) - beta*(X[n+T]);
    } else {
      nu[n] = mu[n] +  alpha*(X[n+T]) - beta*(X[n]);
    }
  }
}


model {
  // for (t in 2:T) {
  //   for (i in 1:I) {
  //     mu[t, i] ~ normal(mu[t-1, i], sigma_mu);
  //   }
  // }
  
  for (n in 1:N) {
    if (n % T != 1) {
      mu[n] ~ normal(mu[n-1], sigma_mu);
    }
  }
  
  
  for (n in 1:N) {
    Y[n] ~ normal(nu[n], sigma_y);
  }
}

generated quantities {
  vector[N] y_pred;
  
  for (n in 1:N) {
    y_pred[n] = normal_rng(nu[n], sigma_y);
  }
}

