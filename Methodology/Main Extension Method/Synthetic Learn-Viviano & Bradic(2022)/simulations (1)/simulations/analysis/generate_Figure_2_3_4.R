working_dir = '.'
load(paste0(working_dir, '/generate_data/simulated_data_1.RData'))
source(paste0(working_dir,'/libraries/library_sim_vary_T.R'))
source(paste0(working_dir,'/libraries/library_analysis.R'))

load('saved_results/varying_T_p10.RData')
plot_60 = create_final_plot_comparisons(finals, which_N_to_select = 60, which_T_to_select = 10)
plot_100 = create_final_plot_comparisons(finals, which_N_to_select = 100, which_T_to_select = 10)


jpeg('./figures_and_tables/figures_main_text/figure2_PARTA.jpeg')
print(plot_60)
dev.off() 


jpeg('./figures_and_tables/figures_main_text/figure4.jpeg')
print(plot_100)
dev.off() 

load('saved_results/varying_T_p10_non_stat.RData')
plot_60 = create_final_plot_comparisons_non_stat(finals, which_N_to_select = 60, which_T_to_select = 10)
plot_100 = create_final_plot_comparisons_non_stat(finals, which_N_to_select = 100, which_T_to_select = 10)


library(ggpubr)
fig3 = ggarrange(plot_60[[1]], plot_100[[1]], common.legend = T)
jpeg('./figures_and_tables/figures_main_text/figure3.jpeg')
print(fig3)
dev.off()

load('./saved_results/varying_T_p10_cos_and_pol.RData')
plot3_cos_pol = create_final_plot_plot_cos(finals, which_N_to_select = 60, which_T_to_select = 10)
jpeg('./figures_and_tables/figures_main_text/figure2_PARTB.jpeg')
print(plot3_cos_pol)
dev.off() 

plot4_cos_pol = create_final_plot_plot_cos(finals, which_N_to_select = 100, which_T_to_select = 10)
jpeg('./figures_and_tables/figures_supplement/figure2.jpeg')
print(plot4_cos_pol)
dev.off() 

load('saved_results/varying_T_p10.RData')
plot_60_5 = create_final_plot_comparisons(finals, which_N_to_select = 60, which_T_to_select = 5)
plot_100_5 = create_final_plot_comparisons(finals, which_N_to_select = 100, which_T_to_select = 5)
jpeg('./figures_and_tables/figures_supplement/figure3_PARTA.jpeg')
print(plot_60_5)
dev.off() 
jpeg('./figures_and_tables/figures_supplement/figure3_PARTB.jpeg')
print(plot_100_5)
dev.off() 
plot_60_20 = create_final_plot_comparisons(finals, which_N_to_select = 60, which_T_to_select = 20)
plot_100_20 = create_final_plot_comparisons(finals, which_N_to_select = 100, which_T_to_select = 20)
jpeg('./figures_and_tables/figures_supplement/figure4_PARTA.jpeg')
print(plot_60_20)
dev.off() 
jpeg('./figures_and_tables/figures_supplement/figure4_PARTB.jpeg')
print(plot_100_20)
dev.off() 


