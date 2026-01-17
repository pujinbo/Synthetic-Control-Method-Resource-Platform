working_dir <- '.'
load(paste0(working_dir, '/generate_data/simulated_data_cos_and_pol.RData'))
source(paste0(working_dir,'/libraries/library_sim_vary_T.R'))


library(foreach)
finals <- foreach(j = 1:length(simulated_data), .combine = append)%do%{

  first_row <- simulated_data[[j]]
  designs <- first_row[[2]]
  N <- c(60, 80, 100)
  tt <- c(5,10, 20)
  all_designs <- expand.grid(N, tt)
  model <- as.character(designs[1,2])
  sd_Y <- all_designs[1,3]
  p <-  as.numeric(designs[1,4])
  sd_Y <- as.numeric(designs[1,3])
  if(p == 10){ 
  simulation <- list()
  acc = 1
  

  for(k in 1:dim(all_designs)[1]){
  
  dd <- all_designs[k,]
  N <- as.numeric(as.character(dd[1]))
  tt <- as.numeric(as.character(dd[2]))
 
  simulation[[acc]] <- list(simulation_bootstrap_vs_perm_final(p=p, 
                                                   N = N, T_0 = N - tt, 
                                                   alpha=c(0,0.1, 0.2,0.3, 0.35, 0.4, 0.5,0.7,1,1.5), 
                                                   N_sim=300,sd_Y=sd_Y, 
                                                   fake_experts = 50, other_N = NA, 
                                                   verbose = F, 
                                                   data = first_row[[1]]), c(j, k))
  acc = acc + 1
  }

  
  list(simulation)
  }
  
}

save.image(file=paste0(working_dir, '/saved_results/varying_T_p10_cos_and_pol.RData'))
