######################################################################################
## Analysis using only Southern Countries that did not adopt ObamaCare
######################################################################################

################################################################################
#### Demeaning estimator 
################################################################################



source('./do_file.R')
XXX <- read.table('./MEDCOST_STATES_CHILDLESS_ADULTS.txt')
## Upload library 
source('./library.R')
source('./my_panel_view_function.R')
employment <- read.table('./employment_BFRSS.txt')
##### Estimation 

## Remove Southern states that implemented obama care
data_ts <- XXX[,which(colnames(XXX)%in% c("X37","X45","X28","X1","X13","X12"))]
data_ts <- cbind(data_ts, Tenn = med_ts)


## Average quarterly  
data_ts_new <- matrix(nrow = 100, ncol = dim(data_ts)[2])
employment_new <- matrix(nrow = 100, ncol = dim(employment)[2])
med_ts_new  <-  rep(NA, 100)
k <- 1
for(i in 1:100){
  data_ts_new[i,] <- apply(as.matrix(data_ts[k:(k+2), ]), 2, function(x) mean(unlist(x)))
  med_ts_new[i] <- mean(med_ts[k:(k+2)]) 
  employment_new[i,] <-   apply(as.matrix(employment[k:(k+2), ]), 2, function(x) mean(unlist(x)))
  k <- k + 3
}
data_ts <- data_ts_new
data_ts <- apply(data_ts, 2, as.numeric)
mean_controls = apply(data_ts[, -dim(data_ts)[2]], 1, mean)
demeaned_data_ts = apply(data_ts, 2, function(x) x - mean_controls)
med_ts <- med_ts_new
med_ts_demeaned <- med_ts_new - mean_controls 
employment <- employment_new
k <- 1
loss <- rep(NA, 100)
my_eta <- 1/(88 * var(med_ts))
Experts1 <- generate_experts(T_0=20, T1=51, N=100, xgboost = F, factor_model = T,  
                             lasso=T, X=as.matrix(demeaned_data_ts[, -dim(data_ts)[2]]), 
                             y=med_ts_demeaned, sc_control = F, double_lasso = F, randforest = T, SVM=F, 
                             arima = F, constraint = c(0.5,1,1.5), RNN=F, did=T, external_covariates=employment)
weights_T1 <- Exp_algorithm(med_ts_demeaned[1:20], Experts1[1:20,],  eta=my_eta)
weights_save_demeaned = weights_T1 
## Recompute experts on the full sample
Y_hat1A <- apply(Experts1, 1, function(x) weights_T1%*%x) + mean_controls



###############################################################################################
################# Testing 
###############################################################################################
Y_hat1A <- apply(Experts1, 1, function(x) weights_T1%*%x) + mean_controls

bootsr <- tsboot(cbind(demeaned_data_ts[-c(20:51, 88:100),dim(data_ts)[2]],Experts1[-c(20:51, 88:100),]), 
                 function(x) function4boot_TE(x, T_0=20,   method='exponential_forecaster'), R=10000, sim="fixed", l=3)
bias <- -mean(Y_hat1A[1:20] - med_ts[1:20] )


confidence_region1 <- c(sort(bootsr$t)[9000],sort(bootsr$t)[8000])
test_stat1 <- sum((Y_hat1A[52:88] - med_ts[52:88])**2)/sqrt(length(52:88))
ate1 <- -mean((Y_hat1A[52:88] - med_ts[52:88])) - bias

bootsr <- tsboot(cbind(demeaned_data_ts[-c(20:51, 52:55, 88:100),dim(data_ts)[2]], 
                       Experts1[-c(20:51, 52:55, 88:100),]), 
                 function(x) function4boot_TE(x, T_0=20, method='exponential_forecaster'), R=10000, sim="fixed", l=3)

confidence_region2 <- c(sort(bootsr$t)[9000],sort(bootsr$t)[8000])
test_stat2 <- sum((Y_hat1A[56:88] -  med_ts[56:88])**2)/sqrt(length(56:88))
ate2 <- -mean((Y_hat1A[56:88] - med_ts[56:88])) - bias



bootsr <- tsboot(cbind(demeaned_data_ts[-c(20:51, 52:59, 88:100), 
                                        dim(data_ts)[2]], 
                       Experts1[-c(20:51, 52:59, 88:100),]), 
                 function(x) function4boot_TE(x, T_0=20,method='exponential_forecaster'), R=10000, sim="fixed", l=3)


confidence_region3 <- c(sort(bootsr$t)[9000],sort(bootsr$t)[8000])
test_stat3 <- sum((Y_hat1A[60:88] - med_ts[60:88])**2)/sqrt(length(60:88))
ate3 <- -mean((Y_hat1A[60:88] - med_ts[60:88])) - bias



bootsr <- tsboot(cbind(demeaned_data_ts[-c(20:51, 52:63, 88:100),dim(data_ts)[2]],Experts1[-c(20:51, 52:63, 88:100),]), 
                 function(x) function4boot_TE(x, T_0=20,  method='exponential_forecaster'), R=10000, sim="fixed", l=3)

confidence_region4 <- c(sort(bootsr$t)[9000],sort(bootsr$t)[8000])
test_stat4 <- sum((Y_hat1A[64:88] - med_ts[64:88])**2)/sqrt(length(64:88))
ate4 <- -mean((Y_hat1A[64:88] - med_ts[64:88])) - bias

results <- rbind(c(confidence_region1, test_stat1, ate1), c(confidence_region2, test_stat2, ate2), c(confidence_region3, test_stat3, ate3), c(confidence_region4, test_stat4, ate4))
colnames(results) <- c('acceptance region','acceptance region', 't statistic', 'ate')
rownames(results) <- c('m = 0', 'm = 1yr', 'm = 2yr', 'm = 3yr')
table_part3 <- results


################################################################ 
######### Estimator without demeaning 
################################################################


source('./do_file.R')
XXX <- read.table('./MEDCOST_STATES_CHILDLESS_ADULTS.txt')
## Upload library 
source('./library.R')
source('./my_panel_view_function.R')
employment <- read.table('./employment_BFRSS.txt')
##### Estimation 

## Remove Southern states that implemented obama care
data_ts <- XXX[,which(colnames(XXX)%in% c("X37","X45","X28","X1","X13","X12"))]
data_ts <- cbind(data_ts, Tenn = med_ts)

## Average quarterly  
data_ts_new <- matrix(nrow = 100, ncol = dim(data_ts)[2])
employment_new <- matrix(nrow = 100, ncol = dim(employment)[2])
med_ts_new  <-  rep(NA, 100)
k <- 1
for(i in 1:100){
  data_ts_new[i,] <- apply(as.matrix(data_ts[k:(k+2), ]), 2, function(x) mean(unlist(x)))
  med_ts_new[i] <- mean(med_ts[k:(k+2)]) 
  employment_new[i,] <-   apply(as.matrix(employment[k:(k+2), ]), 2, function(x) mean(unlist(x)))
  k <- k + 3
}
data_ts <- data_ts_new
data_ts <- apply(data_ts, 2, as.numeric)
med_ts <- med_ts_new 
employment <- employment_new
k <- 1
loss <- rep(NA, 100)
my_eta <- 1/(88 * var(med_ts))
Experts1 <- generate_experts(T_0=20, T1=51, N=100, xgboost = F, factor_model = T,  
                             lasso=T, X=as.matrix(data_ts[, -dim(data_ts)[2]]), 
                             y=med_ts, sc_control = F, double_lasso = F, randforest = T, SVM=F, 
                             arima = F, constraint = c(0.5,1,1.5), RNN=F, did=T, external_covariates=employment)
weights_T1 <- Exp_algorithm(med_ts[1:20], Experts1[1:20,],  eta=my_eta)
weights_save2 = weights_T1 
## Recompute experts on the full sample
Y_hat1A <- apply(Experts1, 1, function(x) weights_T1%*%x)


###############################################################################################
################# Testing 
###############################################################################################

bootsr <- tsboot(cbind(data_ts[-c(20:51, 88:100),dim(data_ts)[2]],Experts1[-c(20:51, 88:100),]), 
                 function(x) function4boot_TE(x, T_0=20,   method='exponential_forecaster'), R=10000, sim="fixed", l=3)
bias <- -mean(Y_hat1A[1:20] - med_ts[1:20] )


confidence_region1 <- c(sort(bootsr$t)[9000],sort(bootsr$t)[8000])
test_stat1 <- sum((Y_hat1A[52:88] - med_ts[52:88])**2)/sqrt(length(52:88))
ate1 <- -mean((Y_hat1A[52:88] - med_ts[52:88])) - bias

bootsr <- tsboot(cbind(data_ts[-c(20:51, 52:55, 88:100),dim(data_ts)[2]], 
                       Experts1[-c(20:51, 52:55, 88:100),]), 
                 function(x) function4boot_TE(x, T_0=20, method='exponential_forecaster'), R=10000, sim="fixed", l=3)

confidence_region2 <- c(sort(bootsr$t)[9000],sort(bootsr$t)[8000])
test_stat2 <- sum((Y_hat1A[56:88] - med_ts[56:88])**2)/sqrt(length(56:88))
ate2 <- -mean((Y_hat1A[56:88] - med_ts[56:88])) - bias



bootsr <- tsboot(cbind(data_ts[-c(20:51, 52:59, 88:100), 
                               dim(data_ts)[2]], 
                       Experts1[-c(20:51, 52:59, 88:100),]), 
                 function(x) function4boot_TE(x, T_0=20,method='exponential_forecaster'), R=10000, sim="fixed", l=3)


confidence_region3 <- c(sort(bootsr$t)[9000],sort(bootsr$t)[8000])
test_stat3 <- sum((Y_hat1A[60:88] - med_ts[60:88])**2)/sqrt(length(60:88))
ate3 <- -mean((Y_hat1A[60:88] - med_ts[60:88])) - bias



bootsr <- tsboot(cbind(data_ts[-c(20:51, 52:63, 88:100),dim(data_ts)[2]],Experts1[-c(20:51, 52:63, 88:100),]), 
                 function(x) function4boot_TE(x, T_0=20,  method='exponential_forecaster'), R=10000, sim="fixed", l=3)

confidence_region4 <- c(sort(bootsr$t)[9000],sort(bootsr$t)[8000])
test_stat4 <- sum((Y_hat1A[64:88] - med_ts[64:88])**2)/sqrt(length(64:88))
ate4 <- -mean((Y_hat1A[64:88] - med_ts[64:88])) - bias

results <- rbind(c(confidence_region1, test_stat1, ate1), c(confidence_region2, test_stat2, ate2), c(confidence_region3, test_stat3, ate3), c(confidence_region4, test_stat4, ate4))
colnames(results) <- c('acceptance region','acceptance region', 't statistic', 'ate')
rownames(results) <- c('m = 0', 'm = 1yr', 'm = 2yr', 'm = 3yr')
table_part2 <- results



###########################################################33
#################### Switch periods 
#############################################################



######################################################################################
## demeaned 
######################################################################################
source('./do_file.R')
XXX <- read.table('./MEDCOST_STATES_CHILDLESS_ADULTS.txt')
## Upload library 
source('./library.R')
source('./my_panel_view_function.R')
employment <- read.table('./employment_BFRSS.txt')
##### Estimation 
data_ts <- XXX[,which(colnames(XXX)%in% c("X37","X45","X28","X1","X13","X12"))]
employment <- read.table('./employment_BFRSS.txt')
data_ts <- cbind(data_ts, Tenn = med_ts)
### Add time periods 
data_ts_new <- matrix(nrow = 100, ncol = dim(data_ts)[2])
employment_new <- matrix(nrow = 100, ncol = dim(employment)[2])
med_ts_new  <-  rep(NA, 100)
k <- 1
for(i in 1:100){
  data_ts_new[i,] <- apply(as.matrix(data_ts[k:(k+2), ]), 2, function(x) mean(unlist(x)))
  med_ts_new[i] <- mean(med_ts[k:(k+2)]) 
  employment_new[i,] <-   apply(as.matrix(employment[k:(k+2), ]), 2, function(x) mean(unlist(x)))
  k <- k + 3
}
data_ts <- data_ts_new
data_ts <- apply(data_ts, 2, as.numeric)
med_ts <- med_ts_new 
mean_controls = apply(data_ts[, -dim(data_ts)[2]], 1, mean)
demeaned_data_ts = apply(data_ts, 2, function(x) x - mean_controls)
med_ts_demeaned <- med_ts_new - mean_controls 
employment <- employment_new

k <- 1
loss <- rep(NA, 100)
my_eta <-1/(88 * var(med_ts))
Experts1 <- generate_experts(T_0=1, T1=30, N=100, xgboost = F, factor_model = T,  
                             lasso=T, X=as.matrix(demeaned_data_ts[, -dim(data_ts)[2]]), 
                             y=med_ts_demeaned, sc_control = F, double_lasso = F, randforest = T, SVM=F, 
                             arima = F, constraint = c(0.5,1,1.5), RNN=F, did=T, external_covariates=employment)
weights_T1 <- Exp_algorithm(output = med_ts_demeaned[31:50], Experts = Experts1[31:50,],  eta=100)
## Recompute experts on the full sample
Y_hat1A <- apply(Experts1, 1, function(x) weights_T1%*%x)


###############################################################################################
################# Testing 
###############################################################################################

## Effect of different ms
## Effect of different ms
##Remove the windows used for estimation of the expersts
bootsr <- tsboot(cbind(demeaned_data_ts[-c(1:30, 88:100),dim(data_ts)[2]],Experts1[-c(1:30, 88:100),]), 
                 function(x) function4boot_TE(x, T_0=20,   method='exponential_forecaster'), R=10000, sim="fixed", l=3)
bias <- -mean(Y_hat1A[31:50] -  med_ts_demeaned[31:50] )


confidence_region1 <- c(sort(bootsr$t)[9000],sort(bootsr$t)[8000])
test_stat1 <- sum((Y_hat1A[52:88] - med_ts_demeaned[52:88])**2)/sqrt(length(52:88))
ate1 <- -mean((Y_hat1A[52:88] - med_ts_demeaned[52:88])) - bias

bootsr <- tsboot(cbind(demeaned_data_ts[-c(1:30, 52:55, 88:100),dim(data_ts)[2]], 
                       Experts1[-c(1:30, 52:55, 88:100),]), 
                 function(x) function4boot_TE(x, T_0=20, method='exponential_forecaster'), R=10000, sim="fixed", l=3)

confidence_region2 <- c(sort(bootsr$t)[9000],sort(bootsr$t)[8000])
test_stat2 <- sum((Y_hat1A[56:88] - med_ts_demeaned[56:88])**2)/sqrt(length(56:88))
ate2 <- -mean((Y_hat1A[56:88] - med_ts_demeaned[56:88])) - bias



bootsr <- tsboot(cbind(demeaned_data_ts[-c(1:30, 52:59, 88:100), 
                               dim(data_ts)[2]], 
                       Experts1[-c(1:30, 52:59, 88:100),]), function(x) function4boot_TE(x, T_0=20,method='exponential_forecaster'), R=10000, sim="fixed", l=3)


confidence_region3 <- c(sort(bootsr$t)[9000],sort(bootsr$t)[8000])
test_stat3 <- sum((Y_hat1A[60:88] - med_ts_demeaned[60:88])**2)/sqrt(length(60:88))
ate3 <- -mean((Y_hat1A[60:88] - med_ts_demeaned[60:88])) - bias



bootsr <- tsboot(cbind(demeaned_data_ts[-c(1:30, 52:63, 88:100),dim(data_ts)[2]],Experts1[-c(1:30, 52:63, 88:100),]), 
                 function(x) function4boot_TE(x, T_0=20,  method='exponential_forecaster'), R=10000, sim="fixed", l=3)

confidence_region4 <- c(sort(bootsr$t)[9000],sort(bootsr$t)[8000])
test_stat4 <- sum((Y_hat1A[64:88] - med_ts_demeaned[64:88])**2)/sqrt(length(64:88))
ate4 <- -mean((Y_hat1A[64:88] - med_ts_demeaned[64:88])) - bias

results <- rbind(c(confidence_region1, test_stat1, ate1), c(confidence_region2, test_stat2, ate2), c(confidence_region3, test_stat3, ate3), c(confidence_region4, test_stat4, ate4))
colnames(results) <- c('acceptance region','acceptance region', 't statistic', 'ate')
rownames(results) <- c('m = 0', 'm = 1yr', 'm = 2yr', 'm = 3yr')
table_part4 <- results



######################################################################################
## Not demeaned 
######################################################################################
source('./do_file.R')
XXX <- read.table('./MEDCOST_STATES_CHILDLESS_ADULTS.txt')
## Upload library 
source('./library.R')
source('./my_panel_view_function.R')
employment <- read.table('./employment_BFRSS.txt')
##### Estimation 
data_ts <- XXX[,which(colnames(XXX)%in% c("X37","X45","X28","X1","X13","X12"))]
employment <- read.table('./employment_BFRSS.txt')
data_ts <- cbind(data_ts, Tenn = med_ts)
### Add time periods 
data_ts_new <- matrix(nrow = 100, ncol = dim(data_ts)[2])
employment_new <- matrix(nrow = 100, ncol = dim(employment)[2])
med_ts_new  <-  rep(NA, 100)
k <- 1
for(i in 1:100){
  data_ts_new[i,] <- apply(as.matrix(data_ts[k:(k+2), ]), 2, function(x) mean(unlist(x)))
  med_ts_new[i] <- mean(med_ts[k:(k+2)]) 
  employment_new[i,] <-   apply(as.matrix(employment[k:(k+2), ]), 2, function(x) mean(unlist(x)))
  k <- k + 3
}
data_ts <- data_ts_new
data_ts <- apply(data_ts, 2, as.numeric)
med_ts <- med_ts_new 
employment <- employment_new

k <- 1
loss <- rep(NA, 100)
my_eta <-1/(88 * var(med_ts))
Experts1 <- generate_experts(T_0=1, T1=30, N=100, xgboost = F, factor_model = T,  
                             lasso=T, X=as.matrix(data_ts[, -dim(data_ts)[2]]), 
                             y=med_ts, sc_control = F, double_lasso = F, randforest = T, SVM=F, 
                             arima = F, constraint = c(0.5,1,1.5), RNN=F, did=T, external_covariates=employment)
weights_T1 <- Exp_algorithm(output = med_ts[31:50], Experts = Experts1[31:50,],  eta=100)
## Recompute experts on the full sample
Y_hat1A <- apply(Experts1, 1, function(x) weights_T1%*%x)


###############################################################################################
################# Testing 
###############################################################################################

## Effect of different ms
## Effect of different ms
##Remove the windows used for estimation of the expersts
bootsr <- tsboot(cbind(data_ts[-c(1:30, 88:100),dim(data_ts)[2]],Experts1[-c(1:30, 88:100),]), 
                 function(x) function4boot_TE(x, T_0=20,   method='exponential_forecaster'), R=10000, sim="fixed", l=3)
bias <- -mean(Y_hat1A[31:50] - med_ts[31:50] )


confidence_region1 <- c(sort(bootsr$t)[9000],sort(bootsr$t)[8000])
test_stat1 <- sum((Y_hat1A[52:88] - med_ts[52:88])**2)/sqrt(length(52:88))
ate1 <- -mean((Y_hat1A[52:88] - med_ts[52:88])) - bias

bootsr <- tsboot(cbind(data_ts[-c(1:30, 52:55, 88:100),dim(data_ts)[2]], 
                       Experts1[-c(1:30, 52:55, 88:100),]), 
                 function(x) function4boot_TE(x, T_0=20, method='exponential_forecaster'), R=10000, sim="fixed", l=3)

confidence_region2 <- c(sort(bootsr$t)[9000],sort(bootsr$t)[8000])
test_stat2 <- sum((Y_hat1A[56:88] - med_ts[56:88])**2)/sqrt(length(56:88))
ate2 <- -mean((Y_hat1A[56:88] - med_ts[56:88])) - bias



bootsr <- tsboot(cbind(data_ts[-c(1:30, 52:59, 88:100), 
                               dim(data_ts)[2]], 
                       Experts1[-c(1:30, 52:59, 88:100),]), function(x) function4boot_TE(x, T_0=20,method='exponential_forecaster'), R=10000, sim="fixed", l=3)


confidence_region3 <- c(sort(bootsr$t)[9000],sort(bootsr$t)[8000])
test_stat3 <- sum((Y_hat1A[60:88] - med_ts[60:88])**2)/sqrt(length(60:88))
ate3 <- -mean((Y_hat1A[60:88] - med_ts[60:88])) - bias



bootsr <- tsboot(cbind(data_ts[-c(1:30, 52:63, 88:100),dim(data_ts)[2]],Experts1[-c(1:30, 52:63, 88:100),]), 
                 function(x) function4boot_TE(x, T_0=20,  method='exponential_forecaster'), R=10000, sim="fixed", l=3)

confidence_region4 <- c(sort(bootsr$t)[9000],sort(bootsr$t)[8000])
test_stat4 <- sum((Y_hat1A[64:88] - med_ts[64:88])**2)/sqrt(length(64:88))
ate4 <- -mean((Y_hat1A[64:88] - med_ts[64:88])) - bias

results <- rbind(c(confidence_region1, test_stat1, ate1), c(confidence_region2, test_stat2, ate2), c(confidence_region3, test_stat3, ate3), c(confidence_region4, test_stat4, ate4))
colnames(results) <- c('acceptance region','acceptance region', 't statistic', 'ate')
rownames(results) <- c('m = 0', 'm = 1yr', 'm = 2yr', 'm = 3yr')
table_part <- results


final_table_report = rbind(cbind(table_part2, table_part3), 
      cbind(table_part, table_part4))
write.table(final_table_report, file = './tables/TableA2.txt')
