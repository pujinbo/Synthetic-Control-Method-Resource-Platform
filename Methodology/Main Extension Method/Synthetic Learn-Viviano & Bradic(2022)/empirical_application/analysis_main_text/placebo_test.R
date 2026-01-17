


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
my_eta <- 1/(sqrt(88) * var(med_ts))
Experts1 <- generate_experts(T_0=20, T1=51, N=100, xgboost = F, factor_model = T,  
                             lasso=T, X=as.matrix(data_ts[, -dim(data_ts)[2]]), 
                             y=med_ts, sc_control = F, double_lasso = F, randforest = T, SVM=F, 
                             arima = F, constraint = c(0.5,1,1.5), RNN=F, did=T, external_covariates=employment)
weights_T1 <- Exp_algorithm(med_ts[1:20], Experts1[1:20,],  eta=my_eta)
weights_save2 = weights_T1 



#############################################
###### Placebo test 
#############################################

## Make predictions

CI <- matrix(NA, nrow=7, ncol=2)
p_value = TS <- ATE <- rep(NA, 7)
for(j in 1:7){
  k <- 1
  loss <- rep(NA, 100)
  Experts1 <- generate_experts(T_0=20, T1=51, N=100, xgboost = F, factor_model = T,  
                               lasso=T, X=as.matrix(data_ts[, -j]), 
                               y=data_ts[, j], sc_control = F, double_lasso = F, randforest = T, SVM=F, 
                               arima = F, constraint = c(0.5,1,1.5), RNN=F, did=T, external_covariates=employment)
  my_eta <- 1/(sqrt(88) * var(med_ts))
  weights_T1 <- Exp_algorithm(data_ts[1:20,j], Experts1[1:20,],  eta=my_eta)
  
  ## Recompute experts on the full sample
  Y_hat1A <- apply(Experts1, 1, function(x) weights_T1%*%x) 
  
  ## Effect of different ms
  bootsr <- tsboot(cbind(data_ts[-c(20:51, 88:100),j],Experts1[-c(20:51, 88:100),]), 
                   function(x) function4boot_TE(x, T_0=20,method='exponential_forecaster'), R=10000, sim="fixed", l=3)
  bias <- -mean(Y_hat1A[1:20] - data_ts[1:20,j] )
  
  SE_TT <- sd(bootsr$t)
  CI[j,] <- c(sort(bootsr$t)[9000],sort(bootsr$t)[1])
  TS[j] <- sum((Y_hat1A[52:88] - data_ts[52:88,j])**2)/sqrt(length(52:88))
  ATE[j] <- -mean((Y_hat1A[52:88] - data_ts[52:88,j])) - bias
  p_value[j] <- which.min((sort(bootsr$t) - TS[j])**2)/10000
}

toplot <- cbind(CI,TS,ATE)
toplot <- data.frame(toplot)

names_states <- c('North Car.', 'South Car.', 'Mississipi', 'Alabama', 'Florida' , 'Georgia', 'Tenn')
jpeg('./plots/plot3.jpeg')
plot(TS, xaxt='n', main = 'Placebo Tests with SL', type='n', xlab='', ylab='Test Statistics and 90% Acceptance Region', ylim=c(0, 0.02))
points(TS, col='red', cex=1, pch=19)
require(plotrix)
arrows(c(1:7), CI[,1], c(1:7), CI[,2], length=0.05, angle=90, code=3)
axis(1, at=c(1:7),labels=names_states, las=2)
dev.off()

CI <- matrix(NA, nrow=7, ncol=2)
TS <- ATE <- rep(NA, 7)
for(j in 1:7){
  k <- 1
  Y_hat1A <- mean(data_ts[1:51,j]) - mean(apply(data_ts[1:51,-c(j,7)], 2, mean)) + apply(data_ts[,-c(j,7)], 1, mean)
  bootsr <- tsboot(cbind(data_ts[-c(88:100),j],data_ts[-c(88:100),-c(j,7)]), 
                   function(x) function4boot_DiD(x, T_0=51), R=10000, sim="fixed", l=3)
  
  
  SE_TT <- sd(bootsr$t)
  CI[j,] <- c(sort(bootsr$t)[9000],sort(bootsr$t)[1])
  TS[j] <- sum((Y_hat1A[51:88] - data_ts[51:88,j])**2)/sqrt(length(51:88))
  ATE[j] <- -mean((Y_hat1A[51:88] -data_ts[51:88,j]))
  p_value[j] <- which.min((sort(bootsr$t) - TS[j])**2)/10000
}

toplot <- cbind(CI,TS,ATE)
toplot <- data.frame(toplot)

names_states <- c('North Car.', 'South Car.', 'Mississipi', 'Alabama', 'Florida' , 'Georgia', 'Tenn')
jpeg('./plots/plot4.jpeg')
plot(TS, xaxt='n', main = 'Placebo Tests with DiD', type='n', xlab='', ylab='Test Statistics and 90% Acceptance Region', ylim=c(0, 0.02))
points(TS, col='red', cex=1, pch=19)
require(plotrix)
arrows(c(1:7), CI[,1], c(1:7), CI[,2], length=0.05, angle=90, code=3)
axis(1, at=c(1:7),labels=names_states, las=2)
dev.off()
