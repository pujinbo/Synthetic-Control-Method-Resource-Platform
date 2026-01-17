#######################################################################3
#### Common library
#######################################################################
library(mvtnorm)
library(boot)
library("quadprog")
library(DTMCPack)
library(foreach)
library(glmnet)
library(xgboost)
library(randomForest)
library(e1071)
library(permute)
library(rnn)

working_dir <- '.'
source(paste0(working_dir,'/libraries/library.R'))



## Helper for bootstrap
## Arguments:
## Z: first element is y, other elements are either the predictions of the models trained on an HOLD OUT set(in case method is exponential of 
## LS forecaster) otherwise matrix with y as first variable and the columns being the usual variables of interest
## T_0: pre-treatment period
## Return the test statistic on a given bootstrapped sample
helper_4boot <- function(Z, T_0){
  
  N <- dim(Z)[1]
  
  index_remove <- c(T_0:N)
  y <- Z[,1]
  y_hat <- Z[,-1]
  losses <- (y_hat[index_remove] - y[index_remove])**2
  loss_t <- sum(losses)/sqrt(length(index_remove))
  return(loss_t)
}
# Please pass an integer to T_train

## We compare bootstrap with a sample splitting rule against permutation only using least squares
## M, sparsigy, is_X_AR, lag, fat_tailed_x, weak_experts: depracted 
## lvl_sign is the size 
## other arguments: please refer to Sim_Fig5-6-7.R

Simulation_bootstrap_VS_perm <- function(p, N,T_0,  M = Inf, T_train=ceiling(T_0/2),
                                 b=c(1/((2:(p))^2), 1 - sum(1/((2:(p))^2))), alpha=10, 
                                 model="ARMA", AR=0.5, MA=0.3, lvl_sign=0.05, 
                                 weak_experts=F, fat_tailed_x=F,
                                 model_beta ="Linear",
                                 lag=F, is_X_AR=T, sd_Y=1, sparsify=F,size_boot=10, non_stationary = F){
  
  sigma=matrix(NA,p,p)
  for(k in 1:p){for(j in 1:p){sigma[k,j]=0.5^abs(j-k)}}
  
  
  if(model!="Factor_model" & model !="Linear_FM"){
    ## Generate random noise
    X=rmvnorm(N,rep(0,p),sigma=diag(1, nrow=p))
    X = X%*%chol(sigma)
    ## generate X as AR(1)
    innov <- rnorm(N, sd=1 - 0.8^2)
    AR <- sapply(rep(N, p), function(x) arima.sim(model=list(Ar=0.8), x, innov=innov))
    if(is_X_AR){X <- AR + X}
    ## Add fat tailed t student noise
    if(fat_tailed_x){X <- X + rt(N, 4)}
    
    
    y <- sim_process(model=model, N=N, p=AR, q=MA, X=X, b=b, model_beta=model_beta, sd_Y=sd_Y)
  } else if (model %in% c("Factor_model", "Linear_FM")){
    DGP <- ifelse(model=="Linear_FM",2,1)
    data <- sim_process_factor_model(N=N,p=p, b=b, rho=0.6, DGP=DGP, non_stationary = non_stationary)
    X <- data[[2]]
    y <- data[[1]]
  }
  y[T_0:N] = y[T_0:N] + alpha
  
  ## Add intercept 

  y1 <- y[-c(T_0:N)]
  X1 <- X[-c(T_0:N),]
  
  
  
  y_train <- y[c(1:T_train)]
  X_train <- X[c(1:T_train),]
  

  Y_expN <- predict(lm(y_train~., data = as.data.frame(X_train)), newdata = as.data.frame(X))#  apply(X, 1, function(x) weights%*%x)
  bootsr <- tsboot(cbind(y[-c(1:T_train)],Y_expN[-c(1:T_train)]), function(x) helper_4boot(x, T_0=T_0 - T_train), R =100, sim="fixed", l=size_boot)
  quantile <- sort(bootsr$t[,1])[ceiling((1-lvl_sign)*length(bootsr$t[,1]))]
  
  
  ## Predict 
  
  losses <- (Y_expN[c(T_0:N)] - y[c(T_0:N)])**2
  loss_t <- sum(losses)/sqrt(length(T_0:N))
  

  y_hat <- predict(lm(y ~ X))
  u_hat <- y - y_hat
  ## Permute the error 
  permutation_set <- suppressMessages(shuffleSet(N, Inf, how(within = Within(type = "series"))))
  test_statistic <- apply(permutation_set, 1, function(x){ 
    uu <- u_hat[x]
    test <- sum((uu[T_0:N])**2)/sqrt(length(T_0:N))
    test
    })
  quantile_perm <- sort(test_statistic)[ceiling((1-lvl_sign)*length(test_statistic))]
  statistic_perm <- sum((u_hat[T_0:N])**2)/sqrt(length(T_0:N))
  
  
  return(c(loss_t, quantile, statistic_perm, quantile_perm))
}


### Final function for simulations for bootstrap. It returns the power of the bootstrapping method and the method via permutations
## Arguments: refer to Figure5-6-7.R


simulation_bootstrap_vs_perm_final <- function(AR=0.5,p=10, MA=0.3, N_sim = 100, N=100,lvl_sign=0.05, T_0=90, 
                                       T_train=ceiling(T_0/2), alpha=seq(from = -3, to=3, by=0.5), model="ARMA", model_beta="Linear",
                                      sd_Y=1, fat_tailed_x =F,
                                      lag=F, is_X_AR=T, verbose=T,  size_boot=10, non_stationary = F){
  
  
  k <- 1
  final_result <- matrix(0, ncol=2, nrow=length(alpha))
  for (a in alpha){
    
    ## Simulate for bootstrap
    result <- matrix(NA, nrow=N_sim, ncol=4)
    for(i in 1:N_sim){
      result[i,] <- do.call("Simulation_bootstrap_VS_perm", list(lvl_sign = lvl_sign, p=p, N=N,  T_0 = T_0, T_train=T_train,
                                                  alpha=a, AR=AR, MA=MA, model=model, model_beta=model_beta, 
                                                  fat_tailed_x=fat_tailed_x, sd_Y=sd_Y,
                                                        lag=lag, is_X_AR=is_X_AR, size_boot=size_boot, non_stationary = non_stationary))
          }
   
    counts_bootstrapped_quantile <- apply(result, 1, function(x) ifelse(x[1] > x[2], 1, 0))
    res1 <- sum(counts_bootstrapped_quantile)/N_sim
    counts_perm_quantile <- apply(result, 1, function(x) ifelse(x[3] > x[4], 1, 0))
    res2 <- sum(counts_perm_quantile)/N_sim
    
    final_result[k,] <- c(res1, res2)
    if(verbose){print(c(res1, res2))}
    k <- k + 1
    
  }
  return(final_result)}



simulation1 <- simulation_bootstrap_vs_perm_final(p=10, model_beta="Polynomial", N=60, T_0=50, alpha=c(0.2,0.3), N_sim=300,sd_Y=0.1)
save.image(file=paste0(working_dir, '/saved_results/boot_perm_smallN.RData'))
simulation2 <- simulation_bootstrap_vs_perm_final(p=10, model_beta="Polynomial", N=60, T_0=50, alpha=c(0.2,0.3), N_sim=300,sd_Y=1)
save.image(file=paste0(working_dir, '/saved_results/boot_perm_smallN.RData'))
simulation3 <- simulation_bootstrap_vs_perm_final(p=10, model_beta="Cos", N=60, T_0=50, alpha=c(0.2,0.3), sd_Y=0.1,  N_sim=300)
save.image(file=paste0(working_dir, '/saved_results/boot_perm_smallN.RData'))
simulation4 <- simulation_bootstrap_vs_perm_final(p=10, model_beta="Cos", N=60, T_0=50, alpha=c(0.2,0.3), sd_Y=1,  N_sim=300)
save.image(file=paste0(working_dir, '/saved_results/boot_perm_smallN.RData'))

## ARMA components: p = 50 

simulation5 <- simulation_bootstrap_vs_perm_final(p=10, model="Linear_FM", N=60, T_0=50, alpha=c(0.2,0.3), N_sim=300)
save.image(file=paste0(working_dir, '/saved_results/boot_perm_smallN.RData'))
simulation6 <- simulation_bootstrap_vs_perm_final(p=10, model_beta="logit", N=60, T_0=50, alpha=c(0.2,0.3), sd_Y=0.1, N_sim=300)
save.image(file=paste0(working_dir, '/saved_results/boot_perm_smallN.RData'))
print('first set done')
simulation7 <- simulation_bootstrap_vs_perm_final(p=10, model_beta="logit", N=60, T_0=50, alpha=c(0.2,0.3), sd_Y=1, N_sim=300)
save.image(file=paste0(working_dir, '/saved_results/boot_perm_smallN.RData'))
simulation8 <- simulation_bootstrap_vs_perm_final(p=10, model_beta="Cos", N=60, T_0=50, alpha=c(0.2,0.3), sd_Y=0.1, N_sim=300)
save.image(file=paste0(working_dir, '/saved_results/boot_perm_smallN.RData'))
simulation9 <- simulation_bootstrap_vs_perm_final(p=10, model_beta="Cos", N=60, T_0=50, alpha=c(0.2,0.3), sd_Y=1,  N_sim=300)
save.image(file=paste0(working_dir, '/saved_results/boot_perm_smallN.RData'))
simulation10 <- simulation_bootstrap_vs_perm_final(p=10, model="Factor_model", N=60, T_0=50, alpha=c(0.2,0.3), N_sim=300)
save.image(file=paste0(working_dir, '/saved_results/boot_perm_smallN.RData'))




## Low correlation
simulation11 <- simulation_bootstrap_vs_perm_final(p=10, model_beta="logit", N=60, T_0=50, alpha=c(0.2,0.3), model='AR-ARCH', N_sim=300)
save.image(file=paste0(working_dir, '/saved_results/boot_perm_smallN.RData'))
simulation12 <- simulation_bootstrap_vs_perm_final(p=10, model_beta="Cos", N=60, T_0=50, alpha=c(0.2,0.3), model='AR-ARCH', N_sim=300)
save.image(file=paste0(working_dir, '/saved_results/boot_perm_smallN.RData'))
simulation13 <- simulation_bootstrap_vs_perm_final(p=10, model_beta="Factor_model", N=60, T_0=50, alpha=c(0.2,0.3),  N_sim=300, non_stationary = T)
save.image(file=paste0(working_dir, '/saved_results/boot_perm_smallN.RData'))


print('second set done')
