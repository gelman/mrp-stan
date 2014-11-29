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
  int lp_vec_len;
  
  for(i in 1:N)
    total_respondents[i] <- response_y[i] + response_n[i];

  N_cat <- J_age * J_eth * J_inc;
  lp_vec_len <- S + 1 + 1 + 1 + 1;
  // S random effect contributions to log_posterior (vectorized)
  // 1 for fixed effect
  // 1 for random effect standard deviation priors
  // 1 for likelihood
  // 1 for mu fixed
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
  vector[lp_vec_len] lp_vec;
  lp_vec[1] <- normal_log(beta,0,1); 

  lp_vec[2] <- normal_log(stt_effect_intercept_std,0,1);
  lp_vec[3] <- normal_log(age_effect_intercept_std,0,1);
  lp_vec[4] <- normal_log(reg_effect_intercept_std,0,1);
  lp_vec[5] <- normal_log(eth_effect_intercept_std,0,1);

  lp_vec[6] <- normal_log(inc_effect_std,0,1);

  lp_vec[7] <- normal_log(stt_effect_coef_std,0,1);
  lp_vec[8] <- normal_log(age_effect_coef_std,0,1);
  lp_vec[9] <- normal_log(reg_effect_coef_std,0,1);
  lp_vec[10] <- normal_log(eth_effect_coef_std,0,1);

  lp_vec[11] <- normal_log(reg_eth_effect_std,0,1);
  lp_vec[12] <- normal_log(reg_inc_effect_std,0,1);
  lp_vec[13] <- normal_log(reg_age_effect_std,0,1);

  lp_vec[14] <- normal_log(stt_eth_effect_std,0,1);
  lp_vec[15] <- normal_log(stt_inc_effect_std,0,1);
  lp_vec[16] <- normal_log(stt_age_effect_std,0,1);

  lp_vec[17] <- normal_log(eth_inc_effect_std,0,1);
  lp_vec[18] <- normal_log(eth_age_effect_std,0,1);
  lp_vec[19] <- normal_log(inc_age_effect_std,0,1);

  lp_vec[20] <- normal_log(stt_eth_age_effect_std,0,1);
  lp_vec[21] <- normal_log(stt_eth_inc_effect_std,0,1);
  lp_vec[22] <- normal_log(stt_inc_age_effect_std,0,1);
  lp_vec[23] <- normal_log(eth_inc_age_effect_std,0,1) ;

  lp_vec[24] <- normal_log(effect_sd,0.5,1);
  lp_vec[25] <- normal_log(mu,0,1);
    
  lp_vec[26] <- binomial_logit_log(response_y,total_respondents,mu_modeled);
  increment_log_prob(lp_vec);
}
generated quantities {
  vector<lower = 0, upper = 1>[N] pp_repub_prob;
  vector<lower = 0, upper = 1>[J_stt] stt_repub_sup;
  vector[N_cat] N_stt_temp;
  vector[N_cat] agg; // Temp collector
  int pop_count_holder;
  int ind;

  for(i in 1:N)
    pp_repub_prob[i] <- inv_logit(mu_modeled[i]);
 
  for(i in 1:J_stt){
    for(m in 1:N_cat){
      ind <- i + (m - 1) * J_stt;
      pop_count_holder <- pop_counts[ind];
      N_stt[m] <- pop_count_holder;
      agg[m] <- pop_count_holder * pp_repub_prob[ind];
    }
    stt_repub_sup[i] <- sum(agg) / sum(N_stt);
  }
}
