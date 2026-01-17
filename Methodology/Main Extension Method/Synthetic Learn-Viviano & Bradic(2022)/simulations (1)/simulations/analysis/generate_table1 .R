library(stargazer)
working_dir = '.'
load(paste0(working_dir, '/generate_data/simulated_data_1.RData'))
source(paste0(working_dir,'/libraries/library_sim_vary_T.R'))
source(paste0(working_dir,'/libraries/library_analysis.R'))

load('saved_results/varying_T_p10.RData')
res1 <- compute_size(finals, which_N_to_select = 60, which_T_to_select = 5)
res2 <- compute_size(finals, which_N_to_select = 60, which_T_to_select = 10)
res3 <- compute_size(finals, which_N_to_select = 60, which_T_to_select = 20)

res4 <- compute_size(finals, which_N_to_select = 80, which_T_to_select = 5)
res5 <- compute_size(finals, which_N_to_select = 80, which_T_to_select = 10)
res6 <- compute_size(finals, which_N_to_select = 80, which_T_to_select = 20)

tab1 <- cbind(res1, res2, res3)
tab2 <- cbind(res4, res5, res6)
final_tab = cbind(tab1, tab2)


## Look at cos and pol 
load('./saved_results/varying_T_p10_cos_and_pol.RData')
res1 <- compute_size_cos_pol(finals, which_N_to_select = 60, which_T_to_select = 5)
res2 <- compute_size_cos_pol(finals, which_N_to_select = 60, which_T_to_select = 10)
res3 <- compute_size_cos_pol(finals, which_N_to_select = 60, which_T_to_select = 20)

res4 <- compute_size_cos_pol(finals, which_N_to_select = 80, which_T_to_select = 5)
res5 <- compute_size_cos_pol(finals, which_N_to_select = 80, which_T_to_select = 10)
res6 <- compute_size_cos_pol(finals, which_N_to_select = 80, which_T_to_select = 20)

tab1b = cbind(res1, res2, res3)
tab2b <- cbind(res4, res5, res6)

## AR-ARCH model 

load('./saved_results/varying_T_p10_size_AR_ARCH.RData')
res1 <- compute_size_AR_ARCH (finals, which_N_to_select = 60, which_T_to_select = 5)
res2 <- compute_size_AR_ARCH(finals, which_N_to_select = 60, which_T_to_select = 10)
res3 <- compute_size_AR_ARCH(finals, which_N_to_select = 60, which_T_to_select = 20)

res4 <- compute_size_AR_ARCH (finals, which_N_to_select = 80, which_T_to_select = 5)
res5 <- compute_size_AR_ARCH(finals, which_N_to_select = 80, which_T_to_select = 10)
res6 <- compute_size_AR_ARCH(finals, which_N_to_select = 80, which_T_to_select = 20)
load('./saved_results/varying_T_p10_non_stat.RData')
res1_nonstat <- compute_size_non_stat(finals, which_N_to_select = 60, which_T_to_select = 5)
res2_nonstat <- compute_size_non_stat(finals, which_N_to_select = 60, which_T_to_select = 10)
res3_nonstat <- compute_size_non_stat(finals, which_N_to_select = 60, which_T_to_select = 20) 

res4_nonstat <- compute_size_non_stat(finals, which_N_to_select = 80, which_T_to_select = 5)
res5_nonstat <- compute_size_non_stat(finals, which_N_to_select = 80, which_T_to_select = 10)
res6_nonstat <- compute_size_non_stat(finals, which_N_to_select = 80, which_T_to_select = 20) 




final_tab = cbind(rbind(tab1, tab1b, cbind(res1, res2, res3), c(res1_nonstat, res2_nonstat,
                                                                res3_nonstat)), 
                  rbind(tab2, tab2b, cbind(res4, res5, res6), 
                        c(res4_nonstat, res5_nonstat,
                          res6_nonstat)) )
colnames(final_tab) <- rep(c("T* = 5", "T* = 10", "T* = 20"), 2)
rownames(final_tab)[dim(final_tab)[1]] = 'DGP6'
write.table(final_tab, file = './figures_and_tables/tables/Table1.txt')

