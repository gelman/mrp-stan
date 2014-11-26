data {
  int<lower = 1> N; // Number of cells
  int<lower = 1> J_stt; // Number of state categories
  int<lower = 1> J_eth; // Number of ethniticty categories
  int<lower = 1> J_inc; // Number of income categories
  int<lower = 1> J_age; // Number of age categories
  int response_y[N]; // Vector of total yes in cell_i
  int response_n[N]; // Vector of total no in cell_i
  int stt[N]; // Cell's state
  int inc[N]; // Cell's income
  int eth[N]; // Cell's ethnicity
  int age[N]; // Cell's age
}
transformed data {
  int total_respondents[N];
  
  for(i in 1:N)
    total_respondents[i] <- response_y[i] + response_n[i];
}
parameters {
  vector[J_stt] stt_effect_std;
  vector[J_age] age_effect_std;
  vector[J_eth] eth_effect_std;
  vector[J_inc] inc_effect_std;

  vector<lower = 0>[4] effect_sd;
  real mu;
}
transformed parameters {
  vector[J_stt] stt_effect;
  vector[J_age] age_effect;
  vector[J_eth] eth_effect;
  vector[J_inc] inc_effect;
  
  stt_effect <- effect_sd[1] * stt_effect_std;
  age_effect <- effect_sd[2] * age_effect_std;
  eth_effect <- effect_sd[3] * eth_effect_std;
  inc_effect <- effect_sd[4] * inc_effect_std;
}
model {
  vector[N] mu_modeled;

  stt_effect_std ~ normal(0,1);
  age_effect_std ~ normal(0,1);
  inc_effect_std ~ normal(0,1);
  eth_effect_std ~ normal(0,1);

  effect_sd ~ normal(1,1);
  mu ~ normal(0,1);

  for(i in 1:N)
    mu_modeled[i] <- mu + stt_effect[stt[i]] 
                  + age_effect[age[i]]
                  + inc_effect[inc[i]]
                  + eth_effect[eth[i]];
  
  response_y ~ binomial_logit(total_respondents,mu_modeled);
}
