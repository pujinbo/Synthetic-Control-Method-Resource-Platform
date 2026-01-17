library(ggplot2)
library(ggpubr)

plots = list()
my_selected_entry = function(which_N_to_select, which_T_to_select, my_seq = NA){
  
  if(which_T_to_select == 5) acc1 = 0
  if(which_T_to_select == 10) acc1 = 1
  if(which_T_to_select == 20) acc1 = 2
  if(is.na(my_seq)) my_seq = c(60, 70, 80, seq(from = 100, to = 300, by = 40))
  first_col = which(my_seq %in% which_N_to_select )
  elements = acc1 * length(my_seq) + first_col
  return(elements)
}


my_selected_entry2 = function(which_N_to_select, which_T_to_select, my_seq = NA){
  
  if(which_T_to_select == 5) acc1 = 0
  if(which_T_to_select == 10) acc1 = 1
  if(which_T_to_select == 20) acc1 = 2
  if(is.na(my_seq)) my_seq = c(60, 80, 100)
  first_col = which(my_seq %in% which_N_to_select )
  elements = acc1 * length(my_seq) + first_col
  return(elements)
}

create_title <- function(which_N_to_select, which_T_to_select, model, sd_Y, non_stat = F){
  if(model == 'logit' & sd_Y == 0.1) DGP = 'DGP2(a)'
  if(model == 'logit' & sd_Y == 1) DGP = 'DGP2(b)'
  if(model == 'Linear_FM') DGP = 'DGP1'
  if(model == 'Factor_model') DGP = 'DGP3'
  if(non_stat) DGP = 'DGP6'
  if(model == 'Polynomial' & sd_Y == 0.1) DGP = 'DGP4(a)'
  if(model == 'Polynomial' & sd_Y == 1) DGP = 'DGP4(b)'
  
  if(model == "Cos" & sd_Y == 0.1) DGP = 'DGP5(a)'
  if(model == "Cos" & sd_Y == 1) DGP = 'DGP5(b)'
  title = paste0(DGP, ", T = ", which_N_to_select, ', T_0 =', which_N_to_select - which_T_to_select)
  return(title)
}

create_plot_comparisons_SC <- function(selected_element, list_alphas, which_N_to_select, which_T_to_select, model, sd_Y, non_stat = F){
  
  results = selected_element[[1]]
  results = c(results)  
  types = c(rep('Synthetic_Learner', length(list_alphas)), rep('Synthetic Control (Perm)', length(list_alphas)), rep('DID (Perm)', length(list_alphas)))
  TE = rep(list_alphas)
  final_data = cbind(results, types, TE)
  final_data = as.data.frame(final_data)
  names(final_data) <- c('Rejections', 'Methods', 'alpha')
  final_data[, 1] <- as.numeric(as.character(final_data[,1]))
  final_data[, 3] <- as.numeric(as.character(final_data[,3]))
  title <- paste0('Percent of rejections ', create_title(which_N_to_select, which_T_to_select, model, sd_Y, non_stat = non_stat) )
  p2<-ggplot(final_data, aes(x=alpha, y=Rejections, group=Methods)) +
    geom_line(aes(color=Methods))+
    geom_point(aes(color=Methods)) +
    labs(title=title,x="alpha/sd_Y", y = "")
  return(p2)
  
}

create_final_plot = function(plots_list){
  kk = length(plots_list)
  
  if(kk == 1) {
    my_plot = plots_list[[1]]
    return(my_plot)}
  
  if(kk == 2){
    my_plot = ggarrange(plots_list[[1]], plots_list[[2]], common.legend = T)
    return(my_plot)
  }
  
  if(kk == 3){
    my_plot = ggarrange(plots_list[[1]], plots_list[[2]], plots_list[[3]], common.legend = T, nrow = 1)
    return(my_plot)
  }
  
  
  if(kk == 4){
    my_plot = ggarrange(plots_list[[1]], plots_list[[2]], plots_list[[3]], plots_list[[4]], common.legend = T)
    return(my_plot)
  }
  
  if(kk == 5){
    my_plot = ggarrange(plots_list[[1]], plots_list[[2]], plots_list[[3]], plots_list[[4]], plots_list[[5]], common.legend = T)
    return(my_plot)
  }
  
  if(kk == 6){
    my_plot = ggarrange(plots_list[[1]], plots_list[[2]], plots_list[[3]], plots_list[[4]], plots_list[[5]], plots_list[[6]], common.legend = T)
    return(my_plot)
  }
  
  if(kk == 7){
    my_plot = ggarrange(plots_list[[1]], plots_list[[2]], plots_list[[3]], plots_list[[4]], plots_list[[5]], plots_list[[6]], 
                        plots_list[[7]], common.legend = T)
    return(my_plot)
  }
  
  if(kk == 8){
    my_plot = ggarrange(plots_list[[1]], plots_list[[2]], plots_list[[3]], plots_list[[4]], plots_list[[5]], plots_list[[6]], 
                        plots_list[[7]], plots_list[[8]], common.legend = T)
    return(my_plot)
  }
  
}



create_final_plot_comparisons <- function(finals, which_N_to_select, which_T_to_select = 10){ 
N <- c(60, 70, 80, seq(from = 100, to = 300, by = 40))
tt <- c(5,10, 20)
list_alphas = c(0,0.1, 0.2,0.3, 0.35, 0.4, 0.5,0.7,1,1.5)
all_designs <- expand.grid(N, tt)
inner_loop_length = length(finals[[1]])
final_plot = list() 
acc = 1
for(j in 1:length(finals)){
  
  first_row <- simulated_data[[j]]
  designs <- first_row[[2]]
  variance_Y = sapply(first_row[[1]], function(x) var(x[[2]]))
  variance_Y = mean(variance_Y)
  model = as.character(designs[1,2])
  sd_Y =  designs[1,3]
  residual_variance = as.character(designs[1,3])
  ## Pick the N selected 
  select_entries = my_selected_entry(which_N_to_select, which_T_to_select)
  selected_elements = finals[[j]][select_entries]
  plots_list = list() 
  for(k in 1:length(selected_elements)){
    N_to_pass = which_N_to_select[k]
    final_plot[[acc]] <- create_plot_comparisons_SC(selected_elements[[k]], list_alphas/sqrt(variance_Y), N_to_pass, which_T_to_select, model, sd_Y)
    acc = acc + 1
  }
  
}

final_plot_all = create_final_plot(list(final_plot[[2]], final_plot[[4]], final_plot[[1]], final_plot[[3]]))
return(final_plot_all)
}



create_final_plot_comparisons_non_stat <- function(finals, which_N_to_select, which_T_to_select = 10){ 
  N <- c(60, 80, 100)
  tt <- c(5,10, 20)
  list_alphas = c(0,0.1, 0.2,0.3, 0.35, 0.4, 0.5,0.7,1,1.5)
  all_designs <- expand.grid(N, tt)
  inner_loop_length = length(finals[[1]])
  final_plot = list() 
  acc = 1
  for(j in 1:length(finals)){
    
    first_row <- simulated_data[[j]]
    designs <- first_row[[2]]
    variance_Y = sapply(first_row[[1]], function(x) var(x[[2]]))
    variance_Y = mean(variance_Y)
    model = as.character(designs[1,2])
    sd_Y =  designs[1,3]
    residual_variance = as.character(designs[1,3])
    ## Pick the N selected 
    select_entries = my_selected_entry2(which_N_to_select, which_T_to_select)
    selected_elements = finals[[j]][select_entries]
    plots_list = list() 
    for(k in 1:length(selected_elements)){
      N_to_pass = which_N_to_select[k]
      final_plot[[acc]] <- create_plot_comparisons_SC(selected_elements[[k]], list_alphas/sqrt(variance_Y), N_to_pass, which_T_to_select, model, sd_Y, non_stat = T)
      acc = acc + 1
    }
    
  }
  return(final_plot)
  
  
}


create_final_plot_plot_cos <- function(finals, which_N_to_select, which_T_to_select = 10){ 
  N <- c(60, 80, 100)
  tt <- c(5,10, 20)
  list_alphas = c(0,0.1, 0.2,0.3, 0.35, 0.4, 0.5,0.7,1,1.5)
  all_designs <- expand.grid(N, tt)
  inner_loop_length = length(finals[[1]])
  final_plot = list() 
  acc = 1
  for(j in 1:length(finals)){
    
    first_row <- simulated_data[[j]]
    designs <- first_row[[2]]
    variance_Y = sapply(first_row[[1]], function(x) var(x[[2]]))
    variance_Y = mean(variance_Y)
    model = as.character(designs[1,2])
    sd_Y =  designs[1,3]
    residual_variance = as.character(designs[1,3])
    ## Pick the N selected 
    select_entries = my_selected_entry2(which_N_to_select, which_T_to_select)
    selected_elements = finals[[j]][select_entries]
    plots_list = list() 
    for(k in 1:length(selected_elements)){
      N_to_pass = which_N_to_select[k]
      final_plot[[acc]] <- create_plot_comparisons_SC(selected_elements[[k]], list_alphas/sqrt(variance_Y), N_to_pass, which_T_to_select, model, sd_Y)
      acc = acc + 1
    }
    
  }
  
  final_plot_all = create_final_plot(list(final_plot[[3]], final_plot[[1]], final_plot[[4]], final_plot[[2]]))
  return(final_plot_all)
}

create_table_oracle = function(finals_weights, finals_one_learner, finals_SL, 
                               which_N_to_select, which_T_to_select, which_a){ 

  alpha=c(0,0.1, 0.2,0.3, 0.35, 0.4, 0.5,0.7,1,1.5)

  tables = list() 
  for(j in 1:length(finals_weights)){
  
  
  first_row <- simulated_data[[j]]
  designs <- first_row[[2]]
  #variance_Y = sapply(first_row[[1]], function(x) var(x[[2]]))
  #variance_Y = mean(variance_Y)
  model = as.character(designs[1,2])
  sd_Y =  designs[1,3]
  residual_variance = as.character(designs[1,3])
  ## Pick the N selected 
  select_entries = my_selected_entry(which_N_to_select, which_T_to_select)
  selected_elements_weights = finals_weights[[j]][select_entries]
  selected_elements_weights = selected_elements_weights[[1]][[1]]
  selected_elements_weights = selected_elements_weights[1:3]
  
  pick_finals_one_learner = finals_one_learner[[j]]
  pick_finals_one_learner = pick_finals_one_learner[select_entries]
  pick_finals_one_learner = pick_finals_one_learner[[1]][[1]]
  pick_finals_one_learner = pick_finals_one_learner[which_a,1:3]
  
  pick_finals_SL = finals_SL[[j]]
  pick_finals_SL = pick_finals_SL[select_entries]
  pick_finals_SL = pick_finals_SL[[1]][[1]]
  pick_finals_SL = pick_finals_SL[which_a, 1]
  selected_elements = cbind(rbind(pick_finals_SL, t(pick_finals_one_learner)), c(NA, selected_elements_weights))
  colnames(selected_elements) = c(alpha[which_a], 'Weights')
  tables[[j]] = selected_elements
  }
  return(tables)
} 



compute_size <- function(finals, which_N_to_select, which_T_to_select = 10){
  
  N <- c(60, 70, 80, seq(from = 100, to = 300, by = 40))
  tt <- c(5,10, 20)
  list_alphas = c(0,0.1, 0.2,0.3, 0.35, 0.4, 0.5,0.7,1,1.5)
  all_designs <- expand.grid(N, tt)
  inner_loop_length = length(finals[[1]])
  final_plot = list() 
  acc = 1
  for(j in c(2, 4, 1, 3)){
    
    first_row <- simulated_data[[j]]
    designs <- first_row[[2]]
    #variance_Y = sapply(first_row[[1]], function(x) var(x[[2]]))
    #variance_Y = mean(variance_Y)
    model = as.character(designs[1,2])
    sd_Y =  designs[1,3]
    residual_variance = as.character(designs[1,3])
    ## Pick the N selected 
    select_entries = my_selected_entry(which_N_to_select, which_T_to_select)
    selected_elements = finals[[j]][select_entries]
    selected_elements = sapply(selected_elements, function(x) x[[1]][[1]])
    if(j == 2) {
      my_table = selected_elements } else {
        my_table = rbind(my_table, selected_elements)
      }
  }
    rownames(my_table) <- c('DGP1', 'DGP2(a)', 'DGP2(b)', 'DGP3')
    colnames(my_table) = N[which_N_to_select]
    return(my_table)
}




compute_size_cos_pol <- function(finals, which_N_to_select, which_T_to_select = 10){
  
  N <- c(60, 80, 100)
  tt <- c(5,10, 20)
  list_alphas = c(0,0.1, 0.2,0.3, 0.35, 0.4, 0.5,0.7,1,1.5)
  all_designs <- expand.grid(N, tt)
  inner_loop_length = length(finals[[1]])
  final_plot = list() 
  acc = 1
  for(j in c(3, 1, 4, 2)){
    
    first_row <- simulated_data[[j]]
    designs <- first_row[[2]]
    #variance_Y = sapply(first_row[[1]], function(x) var(x[[2]]))
    #variance_Y = mean(variance_Y)
    model = as.character(designs[1,2])
    sd_Y =  designs[1,3]
    residual_variance = as.character(designs[1,3])
    ## Pick the N selected 
    select_entries = my_selected_entry(which_N_to_select, which_T_to_select, my_seq = c(60, 80, 100))
    selected_elements = finals[[j]][select_entries]
    selected_elements = sapply(selected_elements, function(x) x[[1]][[1]])
    if(j == 3) {
      my_table = selected_elements } else {
        my_table = rbind(my_table, selected_elements)
      }
  }
  rownames(my_table) <- c('DGP4(a)', 'DGP4(b)', 'DGP5(a)', 'DGP5(b)')
  colnames(my_table) = N[which_N_to_select]
  return(my_table)
}



compute_size_AR_ARCH <- function(finals, which_N_to_select, which_T_to_select = 10){
  
  N <- c(60, 80, 100)
  tt <- c(5,10, 20)
  list_alphas = c(0,0.1, 0.2,0.3, 0.35, 0.4, 0.5,0.7,1,1.5)
  all_designs <- expand.grid(N, tt)
  inner_loop_length = length(finals[[1]])
  final_plot = list() 
  acc = 1
  for(j in 1:length(finals)){
    
    first_row <- simulated_data[[j]]
    designs <- first_row[[2]]
    #variance_Y = sapply(first_row[[1]], function(x) var(x[[2]]))
    #variance_Y = mean(variance_Y)
    model = as.character(designs[1,2])
    sd_Y =  designs[1,3]
    residual_variance = as.character(designs[1,3])
    ## Pick the N selected 
    select_entries = my_selected_entry(which_N_to_select, which_T_to_select, my_seq = c(60, 80, 100))
    selected_elements = finals[[j]][select_entries]
    selected_elements = sapply(selected_elements, function(x) x[[1]][[1]])
    if(j == 1) {
      my_table = selected_elements } else {
        my_table = rbind(my_table, selected_elements)
      }
  }
  rownames(my_table) <- c('DGP2(c)', 'DGP4(c)', 'DGP5(c)')
  colnames(my_table) = N[which_N_to_select]
  return(my_table)
}



compute_size_non_stat <- function(finals, which_N_to_select, which_T_to_select = 10){
  
  N <- c(60, 80, 100)
  tt <- c(5,10, 20)
  list_alphas = c(0,0.1, 0.2,0.3, 0.35, 0.4, 0.5,0.7,1,1.5)
  all_designs <- expand.grid(N, tt)
  inner_loop_length = length(finals[[1]])
  final_plot = list() 
  acc = 1
  for(j in 1:length(finals)){
    
    first_row <- simulated_data[[j]]
    designs <- first_row[[2]]
    model = as.character(designs[1,2])
    sd_Y =  designs[1,3]
    residual_variance = as.character(designs[1,3])
    ## Pick the N selected 
    select_entries = my_selected_entry(which_N_to_select, which_T_to_select, my_seq = c(60, 80, 100))
    selected_elements = finals[[j]][select_entries]
    selected_elements = sapply(selected_elements, function(x) x[[1]][[1]])
    if(j == 1) {
      my_table = selected_elements } else {
        my_table = rbind(my_table, selected_elements)
      }
  }

  return(my_table)
}

