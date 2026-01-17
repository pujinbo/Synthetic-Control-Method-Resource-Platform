library(stargazer)
working_dir = '.'
load(paste0(working_dir, '/generate_data/simulated_data_1.RData'))
source(paste0(working_dir,'/libraries/library_sim_vary_T.R'))
source(paste0(working_dir,'/libraries/library_analysis.R'))

load('saved_results/varying_T_p10.RData')
finals_SL = finals
load('saved_results/varying_T_weights.RData')
finals_weights = finals
load('saved_results/varying_T_one_learner.RData')
finals_one_learner= finals 
which_a = c(2, 4)

final_tables = create_table_oracle(finals_weights, finals_one_learner, finals_SL,which_N_to_select = 60, 
                                   which_T_to_select = 10, which_a)
final_table = rbind(cbind(final_tables[[2]], final_tables[[4]]), cbind(final_tables[[1]], final_tables[[3]])) 
write.table(final_table, file = './figures_and_tables/tables/Table2.txt')