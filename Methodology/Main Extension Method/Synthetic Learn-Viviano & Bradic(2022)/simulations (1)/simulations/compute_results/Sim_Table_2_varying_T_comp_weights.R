working_dir <- '.'
load(paste0(working_dir, '/generate_data/simulated_data_1.RData'))
source(paste0(working_dir,'/libraries/library_sim_vary_T_compute_weights.R'))


library(foreach)
finals <- foreach(j = 1:length(simulated_data), .combine = append)%do%{

  first_row <- simulated_data[[j]]
  designs <- first_row[[2]]
  N <- c(60, 70, 80, seq(from = 100, to = 300, by = 40))
  tt <- c(5,10, 20)
  all_designs <- expand.grid(N, tt)
  model <- as.character(designs[1,2])
  sd_Y <- all_designs[1,3]
  p <-  as.numeric(designs[1,4])
  sd_Y <- as.numeric(designs[1,3])
  ## Simulations for p == 10
  if(sd_Y == 1 | (sd_Y == 0.1 & model == 'logit')){ 
  if(p == 10){
  
  simulation <- list()
  acc = 1
  

  for(k in 1:dim(all_designs)[1]){
  
  dd <- all_designs[k,]
  N <- as.numeric(as.character(dd[1]))
  tt <- as.numeric(as.character(dd[2]))
 
  simulation[[acc]] <- list(final_weights(p=p, model_beta=model, 
                                                   N = N, T_0 = N - tt, 
                                                   N_sim=300,sd_Y=sd_Y, 
                                                   fake_experts = 50, other_N = NA, 
                                                   verbose = F, 
                                                   data = first_row[[1]]), c(j, k))
  acc = acc + 1
  }

  
  list(simulation)
  }  
  }
}

save.image(file=paste0(working_dir, '/saved_results/varying_T_weights.RData'))
