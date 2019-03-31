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
  real<lower=0, upper=1> delta;
  real<lower=0> sigma_mu;
  real<lower=0> sigma_y;
  
  real mu[T, I];

}

transformed parameters {
  real nu[T, I];
  real adstock[T, I];
  
  for (t in 1:T) {
    
    adstock[t, 1] = X[t] + delta*X_lag1[t] + (delta^2)*X_lag2[t]+ (delta^3)*X_lag3[t];
    adstock[t, 2] = X[t] + delta*X_lag1[T+t] + (delta^2)*X_lag2[T+t]+ (delta^3)*X_lag3[T+t];
    
    nu[t, 1] = mu[t, 1] + alpha*(adstock[t, 1]) - beta*(adstock[t, 2]);
    nu[t, 2] = mu[t, 2] + alpha*(adstock[t, 2]) - beta*(adstock[t, 1]);
  }
}


model {
  for (t in 3:T) {
    for (i in 1:I) {
      mu[t, i] ~ normal(2*mu[t-1, i] - mu[t-2, i], sigma_mu);
    }
  }
  
  for (n in 1:N) {
    Y[n] ~ normal(nu[N2TIME[n], N2ID[n]], sigma_y);
  }
}

generated quantities {
  vector[N] y_pred;
  
  for (n in 1:N) {
    y_pred[n] = normal_rng(nu[N2TIME[n], N2ID[n]], sigma_y);
  }
}

