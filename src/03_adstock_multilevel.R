library(tidyverse)
library(tidymodels)
library(magrittr)
library(lubridate)

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)



library(tidybayes)

create_mu = function(mu_init, sigma_mu, D) {
  mu = numeric(D)
  
  mu[1] = mu_init
  mu[2] = mu[1] +  rnorm(1, 0, sigma_mu)
  for (d in 3:D) {
    mu[d] = mu[d-1] + rnorm(1, 0, sigma_mu)
  }
  
  return(mu)
}

create_mu = function(mu_init, sigma_mu, D) {
  mu = numeric(D)
  
  mu[1] = mu_init
  mu[2] = mu[1] +  rnorm(1, 0, sigma_mu)
  for (d in 3:D) {
    mu[d] = 2*mu[d-1] - mu[d-2] + rnorm(1, 0, sigma_mu)
  }
  
  return(mu)
}

date_list = seq.Date(as_date("2018-01-01"), as_date("2018-02-28"), 1)
D = length(date_list)

alpha = 0.1
gamma = 0.05
delta = 0.7


sigma_mu = 0.1
sigma_y = 1


df = tibble(Date = date_list,
            X1 = runif(D, min = 0, max = 50),
            X2 = runif(D, min = 0, max = 50)) %>% 
  mutate(X1_lag1 = lag(X1),
         X1_lag2 = lag(X1, 2),
         X1_lag3 = lag(X1, 3),
         X2_lag1 = lag(X2),
         X2_lag2 = lag(X2, 2),
         X2_lag3 = lag(X2, 3),
         mu1 = sin(1:D/D * 2 * pi)*5 + 30,
         mu2 = cos(1:D/D * 2 * pi)*5 + 30,
         nu1 = mu1 + alpha*(X1 + delta*X1_lag1 + delta^2*X1_lag2 + delta^3*X1_lag3)  - gamma*(X2 + delta*X2_lag1 + delta^2*X2_lag2 + delta^3*X2_lag3),
         nu2 = mu2 + alpha*(X2 + delta*X2_lag1 + delta^2*X2_lag2 + delta^3*X2_lag3)  - gamma*(X1 + delta*X1_lag1 + delta^2*X1_lag2 + delta^3*X1_lag3),
         Y1 = nu1  + rnorm(D, 0, sigma_y),
         Y2 = nu2  + rnorm(D, 0, sigma_y),
  )

df1 = df %>% 
  select(Date, 
         X = X1, X_lag1 = X1_lag1, X_lag2 = X1_lag2, X_lag3 = X1_lag3,
         mu = mu1, nu = nu1,
         Y = Y1) %>% 
  mutate(ID = 1)


df2 = df %>% 
  select(Date, 
         X = X2, X_lag1 = X2_lag1, X_lag2 = X2_lag2, X_lag3 = X2_lag3,
         mu = mu2, nu = nu2,
         Y = Y2) %>% 
  mutate(ID = 2)

df_input = bind_rows(df1, df2) %>% 
  drop_na() %>% 
  group_by(ID) %>% 
  mutate(Time = row_number()) %>% 
  ungroup()



df_input %>% 
  ggplot(aes(Date, Y, color=factor(ID))) +
  geom_point() +
  geom_line(aes(y = mu))


data = df_input %$% 
  list(N = nrow(df_input),
       T = max(Time),
       I = max(ID),
       N2ID = ID,
       N2TIME = Time,
       X = X,
       X_lag1 = X_lag1,
       X_lag2 = X_lag2,
       X_lag3 = X_lag3,
       Y = Y)


fit = stan("src/03_adstock_multilevel.stan", 
           #init = init_list_list,
           data = data)

fit

library(tidyposterior)

library(bayesplot)

fit %>%  
  gather_draws(mu[r], y_pred[r]) %>% 
  ggplot(aes(r, .value, color = .variable)) +
  stat_lineribbon(aes(y = .value))


df_pred = fit %>%  
  spread_draws(y_pred[r]) %>% 
  ungroup() %>% 
  mutate(ID = if_else(r <= data$T, 1, 2),
         r = if_else(ID == 1, r, r - data$T)) %>%
  group_by(ID, r) %>% 
  summarise(y_pred = mean(y_pred)) %>% 
  ungroup()

df_merged = bind_cols(df_pred, df_input)

df_merged %>% 
  ggplot(aes(Date, y_pred, color = factor(ID))) +
  geom_line() +
  geom_point(aes(y=Y)) +
  scale_x_date(date_labels = "%Y-%m-%d")


fit %>%  
  spread_draws(mu[r]) %>% 
  ungroup() %>% 
  mutate(ID = if_else(r <= data$T, 1, 2),
         r = if_else(ID == 1, r, r - data$T)) %>%
  group_by(ID, r) %>% 
  summarise(mu_pred = mean(mu)) %>% 
  ungroup() %>% 
  bind_cols(df_input) %>% 
  ggplot(aes(Date, mu_pred, color = factor(ID))) +
  geom_line() +
  geom_point(aes(y=mu)) +
  scale_x_date(date_labels = "%Y-%m-%d")






















