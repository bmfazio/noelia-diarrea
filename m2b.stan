// generated with brms 2.7.0

data { 
  int<lower=1> N;  // total number of observations 
  vector[N] Y;     // response variable 
  
  // data for smooth terms
  int Ks;
  matrix[N, Ks] Xs;
  
  // data of smooth s(age1)
  int nb_1;  // number of bases 
  int knots_1[nb_1]; 
  matrix[N, knots_1[1]] Zs_1_1; 
  int prior_only;  // should the likelihood be ignored? 
} 
transformed data { 
} 
parameters { 
  real temp_Intercept;  // temporary intercept 
  // parameters for smooth terms
  vector[Ks] bs;
  // parameters of smooth s(age1)
  vector[knots_1[1]] zs_1_1; 
  real<lower=0> sds_1_1; 
  real<lower=0> sigma;  // residual SD 
} 
transformed parameters { 
  vector[knots_1[1]] s_1_1 = sds_1_1 * zs_1_1; 
} 
model { 
  vector[N] mu = temp_Intercept + rep_vector(0, N) + Xs * bs + Zs_1_1 * s_1_1;
  // priors including all constants 
  target += student_t_lpdf(temp_Intercept | 3, 4, 10); 
  target += normal_lpdf(zs_1_1 | 0, 1); 
  target += student_t_lpdf(sds_1_1 | 3, 0, 10)
  - 1 * student_t_lccdf(0 | 3, 0, 10); 
  target += student_t_lpdf(sigma | 3, 0, 10)
  - 1 * student_t_lccdf(0 | 3, 0, 10); 
  // likelihood including all constants 
  if (!prior_only) { 
    target += normal_lpdf(Y | mu, sigma);
  } 
} 
generated quantities { 
  // actual population-level intercept 
  real b_Intercept = temp_Intercept; 
} 