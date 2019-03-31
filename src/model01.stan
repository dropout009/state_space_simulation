//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] X1;
  // vector[N] X1_lag1;
  // vector[N] X1_lag2;
  // vector[N] X1_lag3;
  vector[N] X2;
  // vector[N] X2_lag1;
  // vector[N] X2_lag2;
  // vector[N] X2_lag3;
  vector[N] Y1;
  vector[N] Y2;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0> alpha;
  // real<lower=0> beta;
  real<lower=0> gamma;
  // real<lower=0, upper=1>  delta;
  real<lower=0> sigma_mu;
  real<lower=0> sigma_y;
  
  vector[N] mu1;
  vector[N] mu2;


}

transformed parameters {
  vector[N] nu1;
  vector[N] nu2;

  // for (n in 1:N) {
  //   nu1[n] = mu1[n] + alpha * (X1[n] + delta*X1_lag1[n] + delta^2*X1_lag2[n] + delta^3*X1_lag3[n]);
  //   nu2[n] = mu2[n] + alpha * (X2[n] + delta*X2_lag1[n] + delta^2*X2_lag2[n] + delta^3*X2_lag3[n]);
  // }
  
  
  for (n in 1:N) {
    nu1[n] = mu1[n] + alpha * (X1[n]) - gamma * (X2[n]);
    nu2[n] = mu2[n] + alpha * (X2[n]) - gamma * (X1[n]);
  }
}
// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for (n in 2:N) {
    mu1[n] ~ normal(mu1[n-1], sigma_mu);
    mu2[n] ~ normal(mu2[n-1], sigma_mu);
  }
  
  
  for (n in 1:N) {
    Y1[n] ~ normal(nu1[n], sigma_y);
    Y2[n] ~ normal(nu2[n], sigma_y);
  }
}

generated quantities {
  vector[N] y1_pred;
  vector[N] y2_pred;
  
  for (n in 1:N) {
    y1_pred[n] = normal_rng(nu1[n], sigma_y);
    y2_pred[n] = normal_rng(nu2[n], sigma_y); 
  }
}

