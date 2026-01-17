## Set True if you want to re-run all simulations(approximate time is around 4 to 5 days on a 8 core computer)
run_also_sim = F
working_dir <- '.'
## Redraw the data: set T 
generate_data = F


if(run_also_sim){
if(generate_data) source('./generate_data/generate_data_sim_varying_T.R')
  
file_list <- list.files(path="./compute_results")
for(j in file_list){
  source(paste0(working_dir, '/compute_results/', j))
  rm(list=ls())
}  
}

file_list_plots <- list.files(path="./analysis")
for(j in file_list_plots){
  source(paste0(working_dir, '/analysis/', j))
  working_dir <- '.'
}
