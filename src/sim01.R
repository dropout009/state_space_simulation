library(tidyverse)
library(tidymodels)
library(magrittr)
library(lubridate)


create_mu = function(mu_init, sigma_mu, D) {
  mu = numeric(D)
  
  mu[1] = mu_init
  mu[2] = mu[1] +  rnorm(1, 0, sigma_mu)
  for (d in 3:D) {
    mu[d] = mu[d-1] + rnorm(1, 0, sigma_mu)
  }
  
  return(mu)
}

date_list = seq.Date(as_date("2018-01-01"), as_date("2018-02-28"), 1)
D = length(date_list)

alpha = 0.1
beta = 0.1
gamma = 0.05
delta = 0.7


sigma_mu = 1
sigma_y = 1


df = tibble(Date = date_list,
            X1 = runif(D, min = 0, max = 100),
            X2 = runif(D, min = 0, max = 100)) %>% 
  mutate(X1_lag1 = lag(X1),
         X1_lag2 = lag(X1, 2),
         X1_lag3 = lag(X1, 3),
         X2_lag1 = lag(X2),
         X2_lag2 = lag(X2, 2),
         X2_lag3 = lag(X2, 3),
         mu1 = create_mu(30, sigma_mu, D),
         mu2 = create_mu(30, sigma_mu, D),
         #nu1 = mu1 + alpha * (X1 + delta*X1_lag1 + delta^2*X1_lag2 + delta^3*X1_lag3),
         #nu2 = mu2 + alpha * (X2 + delta*X2_lag1 + delta^2*X2_lag2 + delta^3*X2_lag3),
         nu1 = mu1 + alpha*(X1)  - gamma*(X2),
         nu2 = mu2 + alpha*(X2)  - gamma*(X1),
         Y1 = nu1  + rnorm(D, 0, sigma_y),
         Y2 = nu2  + rnorm(D, 0, sigma_y),
         )
         

df %>% 
  gather(variable, value, mu1, mu2) %>% 
  ggplot(aes(Date, value, color=variable)) +
  geom_line()


df %>% 
  gather(variable, value, mu1, mu2, Y1, Y2) %>% 
  ggplot(aes(Date, value, color=variable)) +
  geom_line()





df %>% 
  gather(variable, value, nu1, nu2) %>% 
  ggplot(aes(Date, value, color=variable)) +
  geom_line()


df %>% 
  gather(variable, value, X1, X2) %>% 
  ggplot(aes(Date, value, color=variable)) +
  geom_line()


df_input = df %>% 
  drop_na()


data = df_input %>% 
  select(X1, X1_lag1, X1_lag2, X1_lag3, X2, X2_lag1, X2_lag2, X2_lag3, Y1, Y2) %>% 
  as.list()

data = df_input %>% 
  select(X1,  X2, Y1, Y2) %>% 
  as.list()
data['N'] = nrow(df_input)

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


init_list = list(
  alpha = 0.1,
  beta = 0.1,
  gamma = 0.05,
  #delta = 0.7,
  sigma_mu = 0.1,
  sigma_y = 1
)
init_list_list = list(init_list, init_list, init_list, init_list)

fit = stan("src/model01.stan", 
           #init = init_list_list,
           data = data)

fit

library(tidybayes)

df_spread = fit %>% 
  spread_draws(alpha, gamma, mu1[r], mu2[r]) 

fit %>%  
  gather_draws(mu1[r], mu2[r], y1_pred[r], y2_pred[r]) %>% 
  ggplot(aes(r, .value, color = .variable)) +
  stat_lineribbon(aes(y = .value))

df_pred = df_input %>% 
  select(Date, X1, X2) %>% 
  add_fitted_draws(fit)









library(shinystan)
launch_shinystan(fit)

df %>% as.list()

df_input %>% View()

fit %>% summary()





