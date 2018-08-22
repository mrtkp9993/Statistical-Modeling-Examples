/*
* Multinomial Logistic Regression
* -----------------------------------------------------
* Copyright: Murat Koptur <muratkoptur@yandex.com>
* Date: 11/08/2018
* License: GPLv3
*/

data {
  int<lower=0> N; // number of observations
  int<lower=2> K; // number of possible outcomes
  int<lower=1> D; // D is dimension of x_n vectors
  vector[D] x[N];
  int<lower=1,upper=K> y[N];
}

parameters {
  matrix[K, D] beta;
}

model {
  for (k in 1:K)
    beta[k] ~ normal(0, 1);
  for (n in 1:N)
    y[n] ~ categorical_logit(beta * x[n]);
}
