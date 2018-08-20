/*
* Ordered Logistic Regression
* -----------------------------------------------------
* Copyright: Murat Koptur <muratkoptur@yandex.com>
* Date: 13/08/2018
* License: GPLv3
*/

data {
  int<lower=0> N;
  int<lower=0> D;
  int<lower=0> K;
  row_vector[D] x[N];
  int<lower=1, upper=K> y[N];
}

parameters {
  vector[D] beta;
  ordered[K-1] c;
}

model {
  for (n in 1:N)
    y[n] ~ ordered_logistic(x[n] * beta, c);
}
