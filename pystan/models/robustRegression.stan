/*
* Robust Regression
* -----------------------------------------------------
* Copyright: Murat Koptur <muratkoptur@yandex.com>
* Date: 08/08/2018
* License: GPLv3
*/

data {
  int<lower=0> N; // number of observations
  vector[N] X1; // Aspect Ratio
  vector[N] X2; // Lift-to-Drag Ratio
  vector[N] X3; // Weight
  vector[N] X4; // Thrust
  vector[N] Y; // Cost
}

parameters {
  real b_X1;
  real b_X2;
  real b_X3;
  real b_X4;
  real beta;
  real<lower=0> sigma;
  real<lower=1> nu;
}

model {
  b_X1 ~ normal(0, 1e6);
  b_X2 ~ normal(0, 1e6);
  b_X3 ~ normal(0, 1e6);
  b_X4 ~ normal(0, 1e6);
  beta ~ normal(0, 1e3);
  sigma ~ normal(0, 5);
  nu ~ gamma(2, 0.1);
  Y ~ student_t(nu,
                beta + b_X1 * X1 + b_X2 * X2 + b_X3 * X3 + b_X4 * X4,
                sigma);
}

generated quantities {
  real Y_pred[N]; // predictions
  for (n in 1:N) {
    Y_pred[n] = student_t_rng(nu,
                              beta + b_X1 * X1[n] +
                              b_X2 * X2[n] + b_X3 * X3[n] +
                              b_X4 * X4[n],
                              sigma);
  }
}
