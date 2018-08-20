/*
* Multiple Linear Regression
* -----------------------------------------------------
* Copyright: Murat Koptur <muratkoptur@yandex.com>
* Date: 06/08/2018
* License: GPLv3
*/

data {
  int<lower=0> N; // number of observations
  vector[N] fat; // grams of fat
  vector[N] weight; // weight in ounces of one serving
  vector[N] cups; // number of cups in one serving
  vector[N] rating; // a rating of the cereals
}

parameters {
  real b_fat; // coefficents
  real b_weight;
  real b_cups;
  real beta;
  real<lower=0> sigma; // std deviation
}

model {
  b_fat ~ normal(0, 10);
  b_weight ~ normal(0, 10);
  b_cups ~ normal(0, 10);
  beta ~ normal(0, 10);
  sigma ~ cauchy(0, 5);
  rating ~ normal(beta + b_fat * fat + b_weight * weight +
                  b_cups * cups, sigma);
}

generated quantities {
  real rating_pred[N]; // predictions
  real log_lik[N];
  for (n in 1:N)
    rating_pred[n] = normal_rng(beta + b_fat * fat[n] + b_weight * weight[n] +
                                b_cups * cups[n], sigma);
  for (n in 1:N)
    log_lik[n] = normal_lpdf(rating[n] | beta + b_fat * fat[n] + b_weight * weight[n] + 
                                         b_cups * cups[n], sigma);
}
