#######################################################################3
#### Common library
#######################################################################

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

losses_one_learner <- function(y, X, p, N, T_0, T_train, fake_experts=50){
  
  y1 <- y[-c(T_0:N)]
  X1 <- X[-c(T_0:N),]
  
  y_train <- y[c(1:T_train)]
  X_train <- X[c(1:T_train),]
  ## Generate multiple experts 
  new_experts <- suppressWarnings(generate_experts(X, y, N, T_0=T_train, strong_experts = T, fake_experts = 0, 
                                                   sc_control = F, factorized = F))

  if(dim(new_experts)[2] ==3){
    new_experts <- cbind(new_experts, rep(0, dim(new_experts)[1]))
  }
  losses <- apply(new_experts, 2, function(x)  sum((x[c(T_0:N)] - y[c(T_0:N)])**2)/sqrt(length(T_0:N)) )
  quantiles <- my_boot_quantile_simple(T_train, T_0, Y_pred = new_experts, y, size_boot = 10, lvl_sign = 0.05, ncpus = 1, one_learner = F)
  rejections <- apply(cbind(losses, quantiles), 1, function(x) ifelse(x[1] > x[2], 1, 0))
  return(rejections)
}




Simulation_one_learner <- function(p, N,T_0,   T_train=ceiling(T_0/2),
                                         b=c(1/((2:(p))^2), 1 - sum(1/((2:(p))^2))), alpha=10, 
                                         model="ARMA", AR=0.5, MA=0.3, 
                                         fake_experts=10, 
                                         fat_tailed_x=F,
                                         model_beta ="Linear",
                                         lag=F, is_X_AR=T, sd_Y=1, 
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
  losses_one_learner(y = y[1:N], X = X[1:N,], p, N = N, T_0 = T_0, T_train = T_train,  fake_experts=fake_experts)

}


## for each value of alpha and each entry in other_N it runs N_sim simulations
## lvl_sign: size of the test
## for the other arguments see previous functions. 

## return counts of rejections for each method(DiD, SC, with quantiles computed using permutations as in Chernozhukov et al. (2017) and Synthetic Learner with critical quantile computed via bootstrap)

Simulation_one_learner_final <- function(AR=0.5,p=10, MA=0.3, N_sim = 100, N=100,
                                               lvl_sign=0.05, T_0=90, 
                                               T_train=ceiling(T_0/2), 
                                               alpha=c(0,0.1, 0.2,0.3,0.5,0.7,1,1.5), model="ARMA", model_beta="Linear",
                                               sd_Y=1, fat_tailed_x =F,
                                               lag=F, is_X_AR=T, verbose=F,  
                                               fake_experts=10, 
                                               data = NA){
  
  
  k <- 1
  rows_number <- 1
  sample_sizes <- N
  
  final_result <- matrix(0, ncol= 4, nrow=length(alpha)*rows_number)
  k = 1
  for (a in alpha){
    
    
    ## Simulate for bootstrap
  
    pass_data <- NA
    for(i in 1:N_sim){
      if(is.na(data) == F){
        pass_data <- data[[i]]
      }
      rr  <- tryCatch(do.call("Simulation_one_learner", list(p, N,T_0,   T_train=T_train, alpha=a, 
                                                    model=model, AR=AR, MA=MA, 
                                                    fake_experts=fake_experts, 
                                                    fat_tailed_x=fat_tailed_x,
                                                    model_beta = model_beta,
                                                    lag=lag, is_X_AR=is_X_AR, sd_Y=sd_Y, 
                                                    return_simulated_data = F, 
                                                    data = pass_data)), 
                      error = function(e) rep(NA, ncol(result)))
            
    if(i == 1) {
      result = rr } else {
        result = rbind(result, rr)
      }
      
      
    } 
    result = apply(result, 2, function(x) mean(x, na.rm = T))
    final_result[k,] <-result
    k = k + 1
  }
  colnames(final_result) <- c('XGBoost_Bootstrap', 'SVM_Bootstrap', 'Arima_Bootstrap')
  return(final_result)
}
