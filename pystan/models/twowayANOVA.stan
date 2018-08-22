/*
* One-way ANOVA
* -----------------------------------------------------
* Copyright: Murat Koptur <muratkoptur@yandex.com>
* Date: 17/08/2018
* License: GPLv3
*/

data {
  int<lower=0> N;
  int<lower=0, upper=1> x1[N];
  int<lower=0, upper=1> x2[N];
  int y[N];
}

parameters {
  real alpha;
  real beta_x1;
  real beta_x2;
  real beta_x3;
  real<lower=0> sigma;
}

model {
  alpha ~ normal(0, 10);
  beta_x1 ~ normal(0, 10);
  beta_x2 ~ normal(0, 10);
  beta_x3 ~ normal(0, 10);
  sigma ~ normal(0, 5);
  for (i in 1:N)
    y[i] ~ normal(alpha + beta_x1 * x1[i] + beta_x2 * x2[i] + beta_x3 * x1[i] * x2[i], sigma);
}
