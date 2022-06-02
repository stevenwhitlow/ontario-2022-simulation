// polling model
data {
  // Number of days between elections:
  int<lower = 1> num_days;
  // Number of polls:
  int<lower = 1> num_polls;
  // Number of parties:
  int<lower = 2> num_parties;
  // Number of provinces plus national polling average:
  //int n_provinces = 6;
  // Previous election results (national):
  real prev_election[num_parties];
  // Number of polling firms:
  int<lower = 1> num_firms;

  matrix<lower = 0., upper = 1>[num_parties, num_polls] y;
  matrix<lower = 0.>[num_parties, num_polls] se;
  int<lower = 1, upper = num_days> time[num_polls];
  int<lower = 1, upper = num_firms> house[num_polls];

  real delta_loc;
  real<lower = 0.> zeta_scale;
  real<lower = 0.> tau_scale;
}

parameters {
  //Innovations:
  vector[num_parties] epsilon[num_days]; // innovations in latent vote preference state
  corr_matrix[num_parties] omega; // correlation matrix for epsilon
  real<lower = 0.> tau[num_parties]; //scale for innovations
  //House effects:
  //real delta_raw[num_parties, num_firms]; 
  matrix[num_parties, num_firms] delta_raw;
  //Scale parameter for house effects:
  real<lower = 0.> zeta;

  real<lower = 0> sigma[num_firms]; // scale for firm-specific errors
}
transformed parameters {

  //simplex[num_parties] xi[num_days];
  real xi[num_days, num_parties];

  // mu_{i,j} = x_{t,i} + delta_{i,f} for party i, poll j, time t, firm f
  // Observed poll results is distributed N(mu_{i,j}, se_{i,j}) given theoretical SE of poll 
  real mu[num_parties, num_polls];
  // matrix[num_parties, num_polls] mu;


  // this is necessary. If not centered the model is unidentified
  matrix[num_parties, num_firms] delta;
  delta = (delta_raw - mean(delta_raw)) / sd(delta_raw) * zeta;

  //populate initial latent vote preference state on the date of the previous election
  xi[1, ] = prev_election;

  for (i in 1:(num_parties)) {
    for (j in 2:(num_days)) {
      xi[j,i] = xi[j - 1, i] + tau[i] * epsilon[j-1][i];
    }
  }

  for (i in 1:(num_parties)) {
    for (j in 1:num_polls) {
      mu[i, j] = xi[time[j], i] + delta[i, house[j]];
    }
  }

}

model {
  // house effects
  to_vector(delta_raw) ~ normal(0., 1.);
  zeta ~ normal(0., zeta_scale);
  // latent state innovations

  // prior for correlation matrix of innovations, on standardised scale (so SD = 1)
  omega ~ lkj_corr(1); // LKJ prior on the correlation matrix 
  epsilon ~ multi_normal(rep_vector(0, num_parties), omega);  

  // scale of innovations
  tau ~ normal(0, tau_scale);
  sigma ~ normal(1, 0.5);

  // daily polls
  for (i in 1:(num_parties)) {
  for (j in 1:(num_polls)) {
   // print("Party ", i, "Poll ", j, "xi[time[j], i] ",  xi[time[j] , i], "delta[i, house[j]] ",  delta[i, house[j]], "mu[i,j] ", xi[i,j])
   if (se[i,j] == 0) {
     print("Party: ", i, "Poll: ", j)
   }
    y[i,j] ~ normal(mu[i,j], sigma[house[j]]*se[i,j]);
  }
  }
}
