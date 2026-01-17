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

## Set the correct working directory 
working_dir <- '.'
source(paste0(working_dir,'/libraries/library.R'))
source(paste0(working_dir,'/libraries/library_perm_and_boot.R'))
#######################################################################3
### Library for bootstrap 1


## Helper for bootstrap
## Arguments:
## Z: first element is y, other elements are either the predictions of the models trained on an HOLD OUT set(in case method is exponential of 
## LS forecaster) otherwise matrix with y as first variable and the columns being the usual variables of interest
## T_0: pre-treatment period
## Return the test statistic on a given bootstrapped sample
## Helper when only one learner is used 

helper_4boot_simple <- function(Z, T_0){
  
  N <- dim(Z)[1]
  
  index_remove <- c(T_0:N)
  y <- Z[,1]
  y_hat <- Z[,-1]
  losses <- (y_hat[index_remove] - y[index_remove])**2
  loss_t <- sum(losses)/sqrt(length(index_remove))
  return(loss_t)
}
## Helper for more than one learner 
helper_4boot_complex <- function(Z,T_0){
  
  N <- dim(Z)[1]
  
  index_remove <- c(T_0:N)
  y <- Z[,1]
  X <- Z[,-1]
  X <- as.matrix(X)
  colnames(X) <- c(1:dim(X)[2])
  p <- dim(X)[2]
  y1 <- y[-index_remove]
  X1 <- X[-index_remove,]
  eta = sqrt(2*log(p)/(N- 1))
  
  
  # Avoid cross validation not to go crazy
  weights <- Exp_algorithm(y1, X1, eta=eta)
  Y_expN <- apply(X[index_remove,], 1, function(x) weights%*%x)
  
  losses <- (Y_expN - y[index_remove])**2
  loss_t <- sum(losses)/sqrt(length(index_remove))
  return(loss_t)
}


## Helper Simulation:
## Pass y, X, p(dimension), N(length of the period)
## T_train: period for training of the learner(sample splitting rule)
## Model + model_beta: DGP on error(see library.R)
## lvl_sign: size of the test
## RNN, lasso, randomforest, strong experts, weak experts: booleans for experts to include
## M, lag, is_X_AR, sparsify, my_seed: depracated
## sd-Y: standard deviation of the error
## synthetic control only: boolean(only include the synthetic control)
## size_boot: size of the blocks for the bootstrap

## Return: test statistic and critical quantile computed via bootstrap for the synthetic learner
## and test statistics and critical quantile for synthetic control and DiD, with critical quantiles computed via permutations as in Chernozhukov et al. (2017)

helper_sim <- function(y, X, p, N, T_0,  M = Inf, T_train=ceiling(T_0/2),
                       model="ARMA", AR=0.5, MA=0.3, lvl_sign=0.05,RNN=F,lasso=F, randforest=F,fake_experts=10, strong_experts=T, 
                       weak_experts=F, fat_tailed_x=F,
                       model_beta ="Linear",
                       lag=F, is_X_AR=T, sd_Y=1,
                       sparsify=F,size_boot=10, 
                       my_seed = rnorm(1)){
  
  
  
  y1 <- y[-c(T_0:N)]
  X1 <- X[-c(T_0:N),]
  y_train <- y[c(1:T_train)]
  X_train <- X[c(1:T_train),]
  
  
  
  
    ## Generate multiple experts 
    
    new_experts <- suppressWarnings(generate_experts(X, y, N, T_0=T_train, strong_experts = strong_experts, weak_experts = weak_experts, 
                                                     lasso=lasso, randforest=randforest, 
                                                     fake_experts = 0, sc_control=F, RNN=RNN, 
                                                     factorized = F))
   
    ## Experts only with fake experts
    new_expertsB <- generate_experts(X, y, N, T_0=T_0, strong_experts = F, weak_experts = F, 
                                     lasso=F, randforest=F, fake_experts = fake_experts)
    
    Experts_with_fake <- cbind(new_experts, new_expertsB[,-c(1:p)])
    Experts_train_2 <- Experts_with_fake[-c(1:T_train, T_0:N),]
    y_train_2 <- y[-c(1:T_train, T_0:N)]
    
    E = dim(Experts_train_2)[2]
    eta = sqrt(2*log(E)/(N- T_train - 1))
    
    
    weights <- Exp_algorithm(y_train_2, Experts_train_2,  eta=eta)
    Y_expN <- apply(Experts_with_fake, 1, function(x) weights%*%x)
    quantile <- my_boot_quantile_complex(T_train, T_0, Experts_with_fake, y, size_boot, ncpus = 1)
  
  
  ## Predict 
  losses <- (Y_expN[c(T_0:N)] - y[c(T_0:N)])**2
  loss_t <- sum(losses)/sqrt(length(T_0:N))
  
  ### Synthetic Control(with permutations)
  ## Compute y on the full sample 
  
  mean_hat <- mean(y)
  weights <- SC_weights(y=y - mean_hat, X=apply(X, 2, function(x) x  -mean(x)), factorized = F)
  y_hat <- apply(apply(X, 2, function(x) x  -mean(x)), 1, function(x) weights%*%x) + mean_hat
  u_hat <- y - y_hat
  ## Permute the error 
  quantile_perm_sc <- compute_quantile_permutation(u_hat, T_0, N, lvl_sign)
  statistic_perm_sc <- sum((u_hat[T_0:N])**2)/sqrt(length(T_0:N))
  
  
  ## Diff in Diff Method 
  
  
  y_hat <- mean(y - apply(X,1 , mean)) + apply(X, 1,mean)
  u_hat <- y - y_hat
  ## Permute the error 
  quantile_perm_did <- compute_quantile_permutation(u_hat, T_0, N, lvl_sign)
  statistic_perm_did <- sum((u_hat[T_0:N])**2)/sqrt(length(T_0:N))
  
  
  
  return(c(loss_t, quantile, statistic_perm_sc, quantile_perm_sc, statistic_perm_did,quantile_perm_did))
  
}
### p: dimension of the covariate matrix
### N: total time periiod
### T_0 pretreatment period
### b: coefficients
### lvl_sign: size of the test
### alpha: true effect of the policy
### lasso: insert lasso in the learners
### randforest: instert randforest in the learner
### fake_experts: insert noise in the learners
###  strong_experts: insert strong experts in the learner
### model + model_beta: DGP(see library.R)
### method: either "exponential_forecaster" or "synthetic_control"
### weak_experts : insert mean and median in learners
### sc_control: insert sc_control in the learner
## size_boot: size of the block for bootstrap
## synthetic control only: only use the synthetic control as base learner
## other_N: T_+. Other N is a vector containing size of the sample. For each element in other_N we run a simulation 
## M, is_X_AR, lag: depracated  
## return_simulated_data: if False run simulations, hence return the simulated data 
### Simulate data and call helper_sim (see above)
## data = NA : simulate data, else use the data passed 
## Please pass an integer to T_train



Simulation_bootstrap_VS_perm <- function(p, N,T_0,  M = Inf, T_train=ceiling(T_0/2),
                                         b=c(1/((2:(p))^2), 1 - sum(1/((2:(p))^2))), alpha=10, 
                                         model="ARMA", AR=0.5, MA=0.3, lvl_sign=0.05,RNN=F,lasso=F, randforest=F,
                                         fake_experts=10, strong_experts=T, 
                                         weak_experts=F, fat_tailed_x=F,
                                         model_beta ="Linear",
                                         lag=F, is_X_AR=T, sd_Y=1, sparsify=F,size_boot=10, 
                                         synthetic_control_only=T, other_N = NA, lasso_only = F, 
                                         return_simulated_data = F, 
                                         data = NA){
  
  if(is.na(data)){
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
    data <- sim_process_factor_model(N=N,p=p, b=b, rho=0.6, DGP=DGP)
    X <- data[[2]]
    y <- data[[1]]
  }
  } else {
    X <- data[[1]]
    y <- data[[2]]
    y <- as.numeric(as.character(y))[1:as.numeric(N)]
    X <- X[1:N,]
  }
  y[T_0:N] = y[T_0:N] + alpha
  if(return_simulated_data) return(list(X, y))
  if(is.na(other_N) == F){ 
    res <- matrix(NA, nrow = length(other_N), ncol=6) 
    k <- 1
    for(n in other_N){    
      res[k,] <- helper_sim(y[1:n], X[1:n,], p, N = n, T_0 = T_0,  M = Inf, T_train=T_train,
                            lvl_sign=lvl_sign,RNN=RNN,lasso=lasso, randforest=randforest,fake_experts=fake_experts, strong_experts=strong_experts, 
                            weak_experts=weak_experts, fat_tailed_x=fat_tailed_x,
                            sparsify=sparsify,size_boot=size_boot, synthetic_control_only=synthetic_control_only, 
                            lasso_only = lasso_only)
      k <- k + 1} 
    
    return(res)
  }else {
    
    helper_sim(y, X, p, N = N, T_0,  M = Inf, T_train=T_train,
               lvl_sign=lvl_sign,RNN=RNN,lasso=lasso, randforest=randforest,fake_experts=fake_experts, strong_experts=strong_experts, 
               weak_experts=weak_experts, fat_tailed_x=fat_tailed_x,
               sparsify=sparsify,size_boot=size_boot)
  }
}


## for each value of alpha and each entry in other_N it runs N_sim simulations
## lvl_sign: size of the test
## for the other arguments see previous functions. 

## return counts of rejections for each method(DiD, SC, with quantiles computed using permutations as in Chernozhukov et al. (2017) and Synthetic Learner with critical quantile computed via bootstrap)

simulation_bootstrap_vs_perm_final <- function(AR=0.5,p=10, MA=0.3, N_sim = 100, N=100,
                                               lvl_sign=0.05, T_0=90, 
                                               T_train=ceiling(T_0/2), 
                                               alpha=c(0,0.1, 0.2,0.3,0.5,0.7,1,1.5), model="ARMA", model_beta="Linear",
                                               sd_Y=1, fat_tailed_x =F,
                                               lag=F, is_X_AR=T, verbose=F,  size_boot=10, 
                                               synthetic_control_only=F, other_N = NA, fake_experts=10, 
                                               lasso_only = F, 
                                               data = NA){
  
  
  k <- 1
  rows_number <-ifelse(is.na(sum(other_N)) == T,1, length(other_N))
  sample_sizes <- if(is.na(sum(other_N)) == T){N} else {other_N}
  
  final_result <- matrix(0, ncol=3, nrow=length(alpha)*rows_number)
  
  for (a in alpha){
    
    
    ## Simulate for bootstrap
    result <- matrix(NA, nrow=N_sim*rows_number, ncol=6)
    pass_data <- NA
    for(i in 1:N_sim){
      if(is.na(data) == F){
        pass_data <- data[[i]]
      }
      rr  <- tryCatch(do.call("Simulation_bootstrap_VS_perm", list(lvl_sign = lvl_sign, p=p, N=N,  T_0 = T_0, T_train=T_train,
                                                          alpha=a, AR=AR, MA=MA, model=model, model_beta=model_beta, 
                                                          fat_tailed_x=fat_tailed_x, sd_Y=sd_Y,
                                                          lag=lag, is_X_AR=is_X_AR, 
                                                          size_boot=size_boot, 
                                                          synthetic_control_only = synthetic_control_only, 
                                                          other_N=other_N, fake_experts = fake_experts, 
                                                          lasso_only = lasso_only, 
                                                          data = pass_data)), 
                      error = function(e) return(rep(NA, dim(result)[1])))
      if(rows_number == 1){
        result[i,]  <- rr} else {
          for(u in 1:rows_number)
            result[i + N_sim*(u-1),]  <- rr[u,]
        } 
    }
    if(rows_number == 1){
      counts_bootstrapped_quantile <- apply(result, 1, function(x) ifelse(x[1] > x[2], 1, 0))
      res1 <- sum(counts_bootstrapped_quantile)/N_sim
      counts_perm_quantile_sc <- apply(result, 1, function(x) ifelse(x[3] > x[4], 1, 0))
      res2 <- sum(counts_perm_quantile_sc)/N_sim
      
      counts_perm_quantile_did <- apply(result, 1, function(x) ifelse(x[5] > x[6], 1, 0))
      res3 <- sum(counts_perm_quantile_did)/N_sim
      
    
      final_result[k,] <- c(res1, res2, res3)
      if(verbose){print(c(res1, res2,res3))}
      k <- k + 1} else { 
        oo = 1
        res1 <- res2 <- res3 <- rep(NA, rows_number)
        for(u in 1:rows_number){
          
          counts_bootstrapped_quantile <- apply(result[(1 + N_sim*(u-1)):(N_sim*u), ], 1, function(x) ifelse(x[1] > x[2], 1, 0))
          res1[oo] <- sum(counts_bootstrapped_quantile)/N_sim
          counts_perm_quantile_sc <- apply(result[(1 + N_sim*(u-1)):(N_sim*u), ], 1, function(x) ifelse(x[3] > x[4], 1, 0))
          res2[oo] <- sum(counts_perm_quantile_sc)/N_sim
          
          counts_perm_quantile_did <- apply(result[(1 + N_sim*(u-1)):(N_sim*u), ], 1, function(x) ifelse(x[5] > x[6], 1, 0))
          res3[oo] <- sum(counts_perm_quantile_did)/N_sim
        
         
          oo <- oo + 1
        }
        final_result[k + c(length(alpha)*(c(1:rows_number)-1)),] <- cbind(res1, res2, res3) ## increasing N returned here
        if(verbose){print(c(res1, res2,res3))}
        k <- k + 1
      }
    
  }
  colnames(final_result) <- c('Synthetic_Learner', 'SC_Permutation', 'DID_Permutation')
  return(final_result)}
