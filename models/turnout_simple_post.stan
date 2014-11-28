data {
  int<lower = 1> N; // Number of cells
  int<lower = 1> J_stt; // Number of state categories
  int<lower = 1> J_eth; // Number of ethniticty categories
  int<lower = 1> J_inc; // Number of income categories
  int<lower = 1> J_age; // Number of age categories
  int<lower = 1> K; // Number of fixed effects
  int response_y[N]; // Vector of total yes in cell_i
  int response_n[N]; // Vector of total no in cell_i
  int stt[N]; // Cell's state
  int inc[N]; // Cell's income
  int eth[N]; // Cell's ethnicity
  int age[N]; // Cell's age
  int pop_counts[N]; // Number of people in cell_i
  row_vector[K] X[N]; // Matrix of fixed effects
}
transformed data {
  int total_respondents[N];
  int N_cat; // number of categories
  
  for(i in 1:N)
    total_respondents[i] <- response_y[i] + response_n[i];

  N_cat <- J_age * J_eth * J_inc;
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
  vector[N] mu_modeled;
  
  stt_effect <- effect_sd[1] * stt_effect_std;
  inc_effect <- effect_sd[2] * inc_effect_std;
  age_effect <- effect_sd[3] * age_effect_std;
  eth_effect <- effect_sd[4] * eth_effect_std;

  for(i in 1:N)
    mu_modeled[i] <- mu + stt_effect[stt[i]] 
                  + age_effect[age[i]]
                  + inc_effect[inc[i]]
                  + eth_effect[eth[i]];
}
model {
  stt_effect_std ~ normal(0,1);
  age_effect_std ~ normal(0,1);
  inc_effect_std ~ normal(0,1);
  eth_effect_std ~ normal(0,1);

  effect_sd ~ normal(1,1);
  mu ~ normal(0,1);
  
  response_y ~ binomial_logit(total_respondents,mu_modeled);
}
generated quantities {
  vector<lower = 0, upper = 1>[N] pp_repub_prob;
  vector<lower = 0, upper = 1>[J_stt] stt_repub_sup;
  int N_stt; // Temp to state
  real agg; // Temp collector
  int ind;

  for(i in 1:N)
    pp_repub_prob[i] <- inv_logit(mu_modeled[i]);
 
  for(i in 1:J_stt){
    N_stt <- 0;
    agg <- 0;
    for(m in 1:N_cat){
      ind <- i + (m - 1) * J_stt;
      N_stt <- N_stt + pop_counts[ind];
      agg <- agg + pop_counts[ind] * pp_repub_prob[ind];
    }
    stt_repub_sup[i] <- agg / N_stt;
  }
}
