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

simulate_data_helper  <- function(p, N,T_0,  M = Inf, 
                                         b=c(1/((2:(p))^2), 1 - sum(1/((2:(p))^2))), alpha=10, 
                                         model="ARMA", AR=0.5, MA=0.3, fat_tailed_x=F,
                                         model_beta ="Linear",
                                         lag=F, is_X_AR=T, sd_Y=1, sparsify=F, non_stationary = F){
  

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
  return(list(X, y))
}


compute_standard_deviation <- function(p, N, sd_Y, model){
  ## Default
  model_gp = 'ARMA'
  model_beta = "Linear"
  if(model %in% c("Factor_model", "Linear_FM")){
    # generating process follows factor model/linear_FM (but the relationship with the coefficients is linear)
    model_gp = as.character(model)
  } else{
    ## Non linear models 
    model_beta = as.character(model)  
  }
  simulation <- list()
  for(i in 1:300){
    simulation[[i]] <- simulate_data_helper(p, N,T_0 = N - 1,  M = Inf, 
                                            b=c(1/((2:(p))^2), 1 - sum(1/((2:(p))^2))), alpha=0, 
                                            model=model_gp, AR=0.5, MA=0.3, fat_tailed_x=F,
                                            model_beta =as.character(model_beta),
                                            lag=F, is_X_AR=T, sd_Y=sd_Y)
    
    
  }
  variances = sapply(simulation, function(x) var(x[[2]]))
  return(sqrt(mean(variances))) 
}
     
  