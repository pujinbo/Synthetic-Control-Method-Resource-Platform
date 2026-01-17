library(stargazer)
working_directory <- '.'
load(paste0(working_directory, '/saved_results/boot_perm_smallN.RData'))




sim_boot2 <- cbind(simulation5[,1], simulation6[,1], simulation7[,1], simulation11[,1] , simulation1[,1], simulation3[,1], simulation4[,1],
                   simulation12[,1], 
                  simulation10[,1], simulation13[,1])
sim_perm2 <- cbind(simulation5[,2], simulation6[,2], simulation7[,2], simulation11[,2] , simulation1[,2], simulation3[,2], simulation4[,2],
                   simulation12[,2], 
                  simulation10[,2], simulation13[,2])

rownames(sim_boot2) <- c('alpha 0.2', 'alpha 0.3')
colnames(sim_boot2) <- c('DGP1', 'DGP2(a)', 'DGP2(b)', 'DGP2(c)', 'DGP3', 'DGP4(a)', 'DGP4(b)','DGP4(c)','DGP5', 'DGP6')


rownames(sim_perm2) <- c('alpha 0.2', 'alpha 0.3')
colnames(sim_perm2) <- c('DGP1', 'DGP2(a)', 'DGP2(b)', 'DGP2(c)', 'DGP3', 'DGP4(a)', 'DGP4(b)','DGP4(c)','DGP5', 'DGP6')


load(paste0(working_directory, '/saved_results/boot_perm_smallN2.RData'))

sim_boot1 <- cbind(simulation5[,1], simulation6[,1], simulation7[,1], simulation11[,1] , simulation1[,1], simulation3[,1], simulation4[,1],
                   simulation12[,1], 
                   simulation10[,1], simulation13[,1])
sim_perm1 <- cbind(simulation5[,2], simulation6[,2], simulation7[,2], simulation11[,2] , simulation1[,2], simulation3[,2], simulation4[,2],
                   simulation12[,2], 
                   simulation10[,2], simulation13[,2])

rownames(sim_boot1) <- c('alpha 0.2', 'alpha 0.3')
colnames(sim_boot1) <- c('DGP1', 'DGP2(a)', 'DGP2(b)', 'DGP2(c)', 'DGP3', 'DGP4(a)', 'DGP4(b)','DGP4(c)','DGP5', 'DGP6')


rownames(sim_perm1) <- c('alpha 0.2', 'alpha 0.3')
colnames(sim_perm1) <- c('DGP1', 'DGP2(a)', 'DGP2(b)', 'DGP2(c)', 'DGP3', 'DGP4(a)', 'DGP4(b)','DGP4(c)','DGP5', 'DGP6')

write.table(sim_boot1, './figures_and_tables/tables/table_T_bootstrap_T_80.txt')
write.table(sim_boot2, './figures_and_tables/tables/table_T_bootstrap_T_60.txt')
write.table(sim_perm1, './figures_and_tables/tables/table_T_perm_T_80.txt')
write.table(sim_perm2, './figures_and_tables/tables/table_T_perm_T_60.txt')