data {
  int<lower=1> N; // sample size (data.frame rows)
  int<lower=1> Kx; // number of covariates for beta mean
  
  int<lower=1> n[N]; // # of attempts (binomial parameter)
  int<lower=0> y[N]; // # of successes (outcome)

  matrix[N, Kx] x; // covariate matrix for beta mean
}

parameters {
  vector[Kx] bx; // coeffs for beta mean
}

generated parameters {
  vector[N] mu_beta;

  for (i in 1:N) {
    mu_beta[i] = inv_logit(x[i]*bx);
  }
}

model {
  y ~ binomial(n, mu_beta);
}

generated quantities {
  vector[N] log_lik;
  
  for (i in 1:N) {

    log_lik[i] =
    log(
      exp( binomial_lpmf(y[i] | n[i], mu_beta[i]) )
      );
  }
}
