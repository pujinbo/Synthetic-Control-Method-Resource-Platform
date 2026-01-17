working_dir <- '.'
source(paste0(working_dir,'/libraries/library_generate_data.R'))

models <- c('logit', 'Linear_FM', 'Factor_model')
sd_Y <- c(1, 0.1)
N <- 300
p <- 10
data <- list()

all_designs <- expand.grid(N, models, sd_Y, p)

library(foreach)
library(doParallel)
numcores <- 10
registerDoParallel(numcores)

simulated_data <- foreach(j = 1:dim(all_designs)[1], .combine = append)%dopar%{
  ## Default
  model_gp = 'ARMA'
  model_beta = "Linear"
  model <- all_designs[j,2]
  if(model %in% c("Factor_model", "Linear_FM")){
    # generating process follows factor model/linear_FM (but the relationship with the coefficients is linear)
    model_gp = as.character(model)
  } else{
    ## Non linear models 
    model_beta = as.character(model)  
  }
  N <- all_designs[j,1]
  sd_Y <- all_designs[j,3]
  p <- all_designs[j,4]
  simulation <- list()
  for(i in 1:300){
  simulation[[i]] <- simulate_data_helper(p, N,T_0 = N - 1,  M = Inf, 
                                                         b=c(1/((2:(p))^2), 1 - sum(1/((2:(p))^2))), alpha=0, 
                                                         model=model_gp, AR=0.5, MA=0.3, fat_tailed_x=F,
                                                         model_beta =as.character(model_beta),
                                                         lag=F, is_X_AR=T, sd_Y=sd_Y)
    
  }
   
  list(list(simulation, all_designs[j,]))
  
}

save(list = 'simulated_data', file = './generate_data/simulated_data_1.RData')




models <- c("Polynomial", "Cos")
sd_Y <- c(1, 0.1)
N <- 300
p <- 10
data <- list()

all_designs <- expand.grid(N, models, sd_Y, p)


simulated_data <- foreach(j = 1:dim(all_designs)[1], .combine = append)%dopar%{
  ## Default
  model_gp = 'ARMA'
  model_beta = "Linear"
  model <- all_designs[j,2]
  if(model %in% c("Factor_model", "Linear_FM")){
    # generating process follows factor model/linear_FM (but the relationship with the coefficients is linear)
    model_gp = as.character(model)
  } else{
    ## Non linear models 
    model_beta = as.character(model)  
  }
  N <- all_designs[j,1]
  sd_Y <- all_designs[j,3]
  p <- all_designs[j,4]
  simulation <- list()
  for(i in 1:300){
    simulation[[i]] <- simulate_data_helper(p, N,T_0 = N - 1,  M = Inf, 
                                            b=c(1/((2:(p))^2), 1 - sum(1/((2:(p))^2))), alpha=0, 
                                            model=model_gp, AR=0.5, MA=0.3, fat_tailed_x=F,
                                            model_beta =as.character(model_beta),
                                            lag=F, is_X_AR=T, sd_Y=sd_Y)
    
  }
  
  list(list(simulation, all_designs[j,]))
  
}

save(list = 'simulated_data', file = './generate_data/simulated_data_cos_and_pol.RData')



## Generate AR-ARCH 




models <-  c('logit', "Polynomial", "Cos")
sd_Y <- c(1)
N <- 300
p <- 10
data <- list()

all_designs <- expand.grid(N, models, sd_Y, p)


simulated_data <- foreach(j = 1:dim(all_designs)[1], .combine = append)%dopar%{
  ## Default
  model_gp = "AR-ARCH"
  model_beta = "Linear"
  model <- all_designs[j,2]
  if(model %in% c("Factor_model", "Linear_FM")){
    # generating process follows factor model/linear_FM (but the relationship with the coefficients is linear)
    model_gp = as.character(model)
  } else{
    ## Non linear models 
    model_beta = as.character(model)  
  }
  N <- all_designs[j,1]
  sd_Y <- all_designs[j,3]
  p <- all_designs[j,4]
  simulation <- list()
  for(i in 1:300){
    simulation[[i]] <- simulate_data_helper(p, N,T_0 = N - 1,  M = Inf, 
                                            b=c(1/((2:(p))^2), 1 - sum(1/((2:(p))^2))), alpha=0, 
                                            model=model_gp, AR=0.5, MA=0.3, fat_tailed_x=F,
                                            model_beta =as.character(model_beta),
                                            lag=F, is_X_AR=T, sd_Y=sd_Y)
    
  }
  
  list(list(simulation, all_designs[j,]))
  
}

save(list = 'simulated_data', file = './generate_data/simulated_data_AR_ARCH.RData')








models <- c('Factor_model')
sd_Y <- c(1)
N <- 300
p <- 10
data <- list()

all_designs <- expand.grid(N, models, sd_Y, p)


simulated_data <- foreach(j = 1:dim(all_designs)[1], .combine = append)%dopar%{
  ## Default
  model_gp = 'ARMA'
  model_beta = "Linear"
  model <- all_designs[j,2]
  if(model %in% c("Factor_model", "Linear_FM")){
    # generating process follows factor model/linear_FM (but the relationship with the coefficients is linear)
    model_gp = as.character(model)
  } else{
    ## Non linear models 
    model_beta = as.character(model)  
  }
  N <- all_designs[j,1]
  sd_Y <- all_designs[j,3]
  p <- all_designs[j,4]
  simulation <- list()
  for(i in 1:300){
    simulation[[i]] <- simulate_data_helper(p, N,T_0 = N - 1,  M = Inf, 
                                            b=c(1/((2:(p))^2), 1 - sum(1/((2:(p))^2))), alpha=0, 
                                            model=model_gp, AR=0.5, MA=0.3, fat_tailed_x=F,
                                            model_beta =as.character(model_beta),
                                            lag=F, is_X_AR=T, sd_Y=sd_Y, non_stationary = T)
    
  }
  
  list(list(simulation, all_designs[j,]))
  
}

save(list = 'simulated_data', file = './generate_data/simulated_data_non_stat_factor.RData')

