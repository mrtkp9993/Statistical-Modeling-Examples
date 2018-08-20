/*
* Linear Regression
* -----------------------------------------------------
* Copyright: Murat Koptur <muratkoptur@yandex.com>
* Date: 05/08/2018
* License: GPLv3
*/

data {
  int<lower=0> N; // number of observations
  vector[N] x; // day x_i
  vector[N] y; // weigth in grams on day x_i
}

parameters {
  real alpha; // intercept
  real beta; // slope
  real<lower=0> sigma; // std deviation
}

model {
  alpha ~ normal(0, 100);
  beta ~ normal(0, 100);
  sigma ~ cauchy(0, 10);
  y ~ normal(alpha + beta * x, sigma);
}

generated quantities {
  // http://mc-stan.org/loo/reference/extract_log_lik.html
  vector[N] log_lik;
  for (n in 1:N)
    log_lik[n] = normal_lpdf(y[n] | alpha + beta * x[n], sigma);
}