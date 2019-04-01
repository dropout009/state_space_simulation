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
  real<lower=0> alpha[I];
  real<lower=0> beta[I];
  real<lower=0, upper=1> delta[I];
  real<lower=0> sigma_mu[I];
  real<lower=0> sigma_y[I];
  
  // real<lower=0> alpha_alpha;
  // real<lower=0> alpha_beta;
  // real<lower=0> alpha_delta;
  // real<lower=0> beta_alpha;
  // real<lower=0> beta_beta;
  // real<lower=0> beta_delta;
  // 
  real mu[T, I];

}

transformed parameters {
  real nu[T, I];
  real adstock[T, I];
  
  for (t in 1:T) {
    
    adstock[t, 1] = X[t] + delta[1]*X_lag1[t] + (delta[1]^2)*X_lag2[t]+ (delta[1]^3)*X_lag3[t];
    adstock[t, 2] = X[t] + delta[2]*X_lag1[T+t] + (delta[2]^2)*X_lag2[T+t]+ (delta[2]^3)*X_lag3[T+t];
    
    nu[t, 1] = mu[t, 1] + alpha[1]*(adstock[t, 1]) - beta[1]*(adstock[t, 2]);
    nu[t, 2] = mu[t, 2] + alpha[2]*(adstock[t, 2]) - beta[2]*(adstock[t, 1]);
  }
}


model {
  
  // for (i in 1:I) {
  //   alpha ~ beta(1, 1);
  //   beta ~ beta(1, 1);
  //   delta ~ beta(1, 1);
  // }
  
  for (t in 3:T) {
    for (i in 1:I) {
      mu[t, i] ~ normal(2*mu[t-1, i] - mu[t-2, i], sigma_mu[i]);
    }
  }
  
  for (n in 1:N) {
    Y[n] ~ normal(nu[N2TIME[n], N2ID[n]], sigma_y[N2ID[n]]);
  }
}

generated quantities {
  real y_pred[T, I];
  
  for (n in 1:N) {
    y_pred[N2TIME[n], N2ID[n]] = normal_rng(nu[N2TIME[n], N2ID[n]], sigma_y[N2ID[n]]);
  }
}

