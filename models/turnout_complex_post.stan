data {
  int<lower = 1> N; // Number of cells
  int<lower = 1> K; // Number of fixed effects
  int<lower = 1> S; // Number of random effects
  int<lower = 1> J_stt; // Number of state categories
  int<lower = 1> J_eth; // Number of ethniticty categories
  int<lower = 1> J_inc; // Number of income categories
  int<lower = 1> J_age; // Number of age categories
  int<lower = 1> J_reg; //
  int<lower = 1> J_reg_eth;
  int<lower = 1> J_reg_inc;
  int<lower = 1> J_reg_age;
  int<lower = 1> J_stt_eth;
  int<lower = 1> J_stt_inc;
  int<lower = 1> J_stt_age;
  int<lower = 1> J_eth_inc;
  int<lower = 1> J_eth_age;
  int<lower = 1> J_inc_age;
  int<lower = 1> J_stt_eth_inc;
  int<lower = 1> J_stt_eth_age;
  int<lower = 1> J_stt_inc_age;
  int<lower = 1> J_eth_inc_age;
  int response_y[N]; // Vector of total yes in cell_i
  int response_n[N]; // Vector of total no in cell_i
  int<lower = 1, upper = J_stt> stt[N]; // Cell's state
  int<lower = 1, upper = J_inc> inc[N]; // Cell's income
  int<lower = 1, upper = J_eth> eth[N]; // Cell's ethnicity
  int<lower = 1, upper = J_age> age[N]; // Cell's age
  int<lower = 1, upper = J_reg> reg[N]; // Cell's region
  int<lower = 1, upper = J_reg_eth> reg_eth[N];
  int<lower = 1, upper = J_reg_inc> reg_inc[N];
  int<lower = 1, upper = J_reg_age> reg_age[N];
  int<lower = 1, upper = J_stt_eth> stt_eth[N];
  int<lower = 1, upper = J_stt_inc> stt_inc[N];
  int<lower = 1, upper = J_stt_age> stt_age[N];
  int<lower = 1, upper = J_eth_inc> eth_inc[N];
  int<lower = 1, upper = J_eth_age> eth_age[N];
  int<lower = 1, upper = J_inc_age> inc_age[N];
  int<lower = 1, upper = J_stt_eth_inc> stt_eth_inc[N];
  int<lower = 1, upper = J_stt_eth_age> stt_eth_age[N];
  int<lower = 1, upper = J_stt_inc_age> stt_inc_age[N];
  int<lower = 1, upper = J_eth_inc_age> eth_inc_age[N];
  int pop_counts[N]; // Number of people in cell_i
  row_vector[K] X[N]; // Matrix of fixed effects
  vector[N] z_inc;
}
transformed data {
  int total_respondents[N];
  int N_cat; // number of categories
  
  for(i in 1:N)
    total_respondents[i] <- response_y[i] + response_n[i];

  N_cat <- J_age * J_eth * J_inc;
}
parameters {
  vector[J_stt] stt_effect_intercept_std;
  vector[J_age] age_effect_intercept_std;
  vector[J_eth] eth_effect_intercept_std;
  vector[J_reg] reg_effect_intercept_std;
  vector[J_stt] stt_effect_coef_std;
  vector[J_age] age_effect_coef_std;
  vector[J_eth] eth_effect_coef_std;
  vector[J_reg] reg_effect_coef_std;
  vector[J_inc] inc_effect_std;
  vector[J_reg_eth] reg_eth_effect_std;
  vector[J_reg_inc] reg_inc_effect_std;
  vector[J_reg_age] reg_age_effect_std;
  vector[J_stt_eth] stt_eth_effect_std;
  vector[J_stt_inc] stt_inc_effect_std;
  vector[J_stt_age] stt_age_effect_std;
  vector[J_eth_inc] eth_inc_effect_std;
  vector[J_eth_age] eth_age_effect_std;
  vector[J_inc_age] inc_age_effect_std;
  vector[J_stt_eth_age] stt_eth_age_effect_std;
  vector[J_stt_eth_inc] stt_eth_inc_effect_std;
  vector[J_stt_inc_age] stt_inc_age_effect_std;
  vector[J_eth_inc_age] eth_inc_age_effect_std;

  vector<lower = 0>[S] effect_sd;
  vector[K] beta;
  real mu;
}
transformed parameters {
  vector[J_stt] stt_effect_intercept;
  vector[J_age] age_effect_intercept;
  vector[J_eth] eth_effect_intercept;
  vector[J_reg] reg_effect_intercept;
  vector[J_inc] inc_effect;
  vector[J_stt] stt_effect_coef;
  vector[J_age] age_effect_coef;
  vector[J_eth] eth_effect_coef;
  vector[J_reg] reg_effect_coef;
  vector[J_reg_eth] reg_eth_effect;
  vector[J_reg_inc] reg_inc_effect;
  vector[J_reg_age] reg_age_effect;
  vector[J_stt_eth] stt_eth_effect;
  vector[J_stt_inc] stt_inc_effect;
  vector[J_stt_age] stt_age_effect;
  vector[J_eth_inc] eth_inc_effect;
  vector[J_eth_age] eth_age_effect;
  vector[J_inc_age] inc_age_effect;
  vector[J_stt_eth_age] stt_eth_age_effect;
  vector[J_stt_inc_age] stt_inc_age_effect;
  vector[J_stt_eth_inc] stt_eth_inc_effect;
  vector[J_eth_inc_age] eth_inc_age_effect;
  
  vector[N] mu_modeled;

  inc_effect <- effect_sd[5] * inc_effect_std;
  
  stt_effect_intercept <- effect_sd[1] * stt_effect_intercept_std;
  reg_effect_intercept <- effect_sd[2] * reg_effect_intercept_std;
  age_effect_intercept <- effect_sd[3] * age_effect_intercept_std;
  eth_effect_intercept <- effect_sd[4] * eth_effect_intercept_std;
  stt_effect_coef <- effect_sd[6] * stt_effect_coef_std;
  reg_effect_coef <- effect_sd[7] * reg_effect_coef_std;
  age_effect_coef <- effect_sd[8] * age_effect_coef_std;
  eth_effect_coef <- effect_sd[9] * eth_effect_coef_std;

  reg_eth_effect <- effect_sd[10] * reg_eth_effect_std;
  reg_inc_effect <- effect_sd[11] * reg_inc_effect_std;
  reg_age_effect <- effect_sd[12] * reg_age_effect_std;

  stt_eth_effect <- effect_sd[13] * stt_eth_effect_std;
  stt_inc_effect <- effect_sd[14] * stt_inc_effect_std;
  stt_age_effect <- effect_sd[15] * stt_age_effect_std;

  eth_inc_effect <- effect_sd[16] * eth_inc_effect_std;
  eth_age_effect <- effect_sd[17] * eth_age_effect_std;
  inc_age_effect <- effect_sd[18] * inc_age_effect_std;

  stt_eth_age_effect <- effect_sd[19] * stt_eth_age_effect_std;
  stt_inc_age_effect <- effect_sd[20] * stt_inc_age_effect_std;
  stt_eth_inc_effect <- effect_sd[21] * stt_eth_inc_effect_std;
  eth_inc_age_effect <- effect_sd[22] * eth_inc_age_effect_std;

  for(i in 1:N)
    mu_modeled[i] <- mu + X[i] * beta 
                  + stt_effect_intercept[stt[i]] 
                  + age_effect_intercept[age[i]]
                  + inc_effect[inc[i]]
                  + eth_effect_intercept[eth[i]]
                  + reg_effect_intercept[reg[i]]
                  + (reg_effect_coef[reg[i]]
                     + stt_effect_coef[stt[i]]
                     + eth_effect_coef[eth[i]]
                     + age_effect_coef[age[i]]) * z_inc[i]
                  + reg_eth_effect[reg_eth[i]]
                  + reg_inc_effect[reg_inc[i]]
                  + reg_age_effect[reg_age[i]]
                  + stt_eth_effect[stt_eth[i]]
                  + stt_inc_effect[stt_inc[i]]
                  + stt_age_effect[stt_age[i]]
                  + eth_inc_effect[eth_inc[i]]
                  + eth_age_effect[eth_age[i]]
                  + inc_age_effect[inc_age[i]]
                  + stt_eth_age_effect[stt_eth_age[i]]
                  + stt_inc_age_effect[stt_inc_age[i]]
                  + stt_eth_inc_effect[stt_eth_inc[i]]
                  + eth_inc_age_effect[eth_inc_age[i]];
}
model {
  beta ~ normal(0,1);
 
  stt_effect_intercept_std ~ normal(0,1);
  age_effect_intercept_std ~ normal(0,1);
  reg_effect_intercept_std ~ normal(0,1);
  eth_effect_intercept_std ~ normal(0,1);

  inc_effect_std ~ normal(0,1);

  stt_effect_coef_std ~ normal(0,1);
  age_effect_coef_std ~ normal(0,1);
  reg_effect_coef_std ~ normal(0,1);
  eth_effect_coef_std ~ normal(0,1);

  reg_eth_effect_std ~ normal(0,1);
  reg_inc_effect_std ~ normal(0,1);
  reg_age_effect_std ~ normal(0,1);

  stt_eth_effect_std ~ normal(0,1);
  stt_inc_effect_std ~ normal(0,1);
  stt_age_effect_std ~ normal(0,1);

  eth_inc_effect_std ~ normal(0,1);
  eth_age_effect_std ~ normal(0,1);
  inc_age_effect_std ~ normal(0,1);

  stt_eth_age_effect_std ~ normal(0,1);
  stt_eth_inc_effect_std ~ normal(0,1);
  stt_inc_age_effect_std ~ normal(0,1);
  eth_inc_age_effect_std ~ normal(0,1);

  effect_sd ~ normal(0.5,1);
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
