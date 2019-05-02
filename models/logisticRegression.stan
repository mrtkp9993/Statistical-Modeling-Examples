/*
* Logistic Regression
* -----------------------------------------------------
* Copyright: Murat Koptur <muratkoptur@yandex.com>
* Date: 09/08/2018
* License: GPLv3
*/

data {
  int<lower=0> N_train;
  int<lower=0> N_test;
  int<lower=0> D;
  row_vector[D] x_train[N_train];
  row_vector[D] x_test[N_test];
  int<lower=0, upper=1> y_train[N_train];
}

parameters {
  real alpha;
  vector[D] beta;
}

model {
  alpha ~ normal(0, 10);
  beta ~ student_t(1,0,2.5); // weakly informative priors
  for (n in 1:N_train)
    y_train[n] ~ bernoulli_logit(inv_logit(x_train[n] * beta + alpha));
}

generated quantities {
  int<lower=0,upper=1> y_pred[N_test];
  for (n in 1:N_test) {
    y_pred[n] = bernoulli_logit_rng(x_test[n] * beta + alpha);
  }
}
