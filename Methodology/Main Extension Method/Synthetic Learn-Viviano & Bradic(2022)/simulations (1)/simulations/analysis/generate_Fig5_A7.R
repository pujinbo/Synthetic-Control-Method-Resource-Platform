######## Library 

library(ggplot2)
library(RColorBrewer)
library(grid)
library(datasets)
library(ggplot2)
fill <- "#4271AE"
lines <- "#1F3552"


##########################################################################
#### Plots on Power
##########################################################################

## Set the correct working directory
working_directory <- '.'
load(paste0(working_directory, '/saved_results/boot_perm2_endotime.RData'))
source(paste0(working_directory,'/libraries/library_analysis.R'))
source(paste0(working_directory, '/libraries/library_generate_data.R'))


## DGP1 
sd_DGP1 = compute_standard_deviation(p = 50, N = 400, sd_Y = 1, model = "Linear_FM")
sim9_1 <- simulation2[1:8,]
sim9_1 <- as.vector(sim9_1)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP1
alpha <- rep(alpha, 3)
methods <- c(rep('Sc_Learner', 8), rep('SC', 8), rep('DiD',8))
sim9_1 <- cbind(sim9_1, alpha, methods)
sim9_1 <- as.data.frame(sim9_1)
sim9_1[,1] <- as.numeric(as.character(sim9_1[,1]))
sim9_1[,2] <- as.numeric(as.character(sim9_1[,2]))
p4<-ggplot(sim9_1, aes(x=alpha, y=sim9_1, group=methods)) +
  geom_line(aes(color=methods))+
  geom_point(aes(color=methods)) +
  labs(title="Percentage of rejections - DGP1, T = 400",x="alpha/sd", y = "")




## DGP 2
sd_DGP2 = compute_standard_deviation(50, 400, 0.1, "logit")
sim10_1 <- simulation3[1:8,]
sim10_1 <- as.vector(sim10_1)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP2
alpha <- rep(alpha, 3)
methods <- c(rep('Sc_Learner', 8), rep('SC', 8), rep('DiD',8))
sim10_1 <- cbind(sim10_1, alpha, methods)
sim10_1 <- as.data.frame(sim10_1)
sim10_1[,1] <- as.numeric(as.character(sim10_1[,1]))
sim10_1[,2] <- as.numeric(as.character(sim10_1[,2]))
p7<-ggplot(sim10_1, aes(x=alpha, y=sim10_1, group=methods)) +
  geom_line(aes(color=methods))+
  geom_point(aes(color=methods)) +
  labs(title="Percentage of rejections - DGP2(a), T = 400",x="alpha/sd", y = "")




sd_DGP2_b = compute_standard_deviation(50, 400, 1, "logit")

sim11_1 <- simulation4[1:8,]
sim11_1 <- as.vector(sim11_1)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP2_b
alpha <- rep(alpha, 3)
methods <- c(rep('Sc_Learner', 8), rep('SC', 8), rep('DiD',8))
sim11_1 <- cbind(sim11_1, alpha, methods)
sim11_1 <- as.data.frame(sim11_1)
sim11_1[,1] <- as.numeric(as.character(sim11_1[,1]))
sim11_1[,2] <- as.numeric(as.character(sim11_1[,2]))
p10<-ggplot(sim11_1, aes(x=alpha, y=sim11_1, group=methods)) +
  geom_line(aes(color=methods))+
  geom_point(aes(color=methods)) +
  labs(title="Percentage of rejections - DGP2(b), T = 400",x="alpha/sd", y = "")




## DGP 4


sd_DGP4 = compute_standard_deviation(p = 10, N = 400, sd_Y = 0.1, model  = "Polynomial")
sim12_1 <- simulation5[1:8,]
sim12_1 <- as.vector(sim12_1)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP4
alpha <- rep(alpha, 3)
methods <- c(rep('Sc_Learner', 8), rep('SC', 8), rep('DiD',8))
sim12_1 <- cbind(sim12_1, alpha, methods)
sim12_1 <- as.data.frame(sim12_1)
sim12_1[,1] <- as.numeric(as.character(sim12_1[,1]))
sim12_1[,2] <- as.numeric(as.character(sim12_1[,2]))
p16<-ggplot(sim12_1, aes(x=alpha, y=sim12_1, group=methods)) +
  geom_line(aes(color=methods))+
  geom_point(aes(color=methods)) +
  labs(title="Percentage of rejections - DGP4(a), T = 400",x="alpha/sd", y = "")


sd_DGP4_b = compute_standard_deviation(10, 400, 1, "Polynomial")
sim13_1 <- simulation6[1:8,]
sim13_1 <- as.vector(sim13_1)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP4_b
alpha <- rep(alpha, 3)
methods <- c(rep('Sc_Learner', 8), rep('SC', 8), rep('DiD',8))
sim13_1 <- cbind(sim13_1, alpha, methods)
sim13_1 <- as.data.frame(sim13_1)
sim13_1[,1] <- as.numeric(as.character(sim13_1[,1]))
sim13_1[,2] <- as.numeric(as.character(sim13_1[,2]))
p19<-ggplot(sim13_1, aes(x=alpha, y=sim13_1, group=methods)) +
  geom_line(aes(color=methods))+
  geom_point(aes(color=methods)) +
  labs(title="Percentage of rejections - DGP4(b), T = 400",x="alpha/sd", y = "")



sd_DGP3 = compute_standard_deviation(50, 400, 1, 'Factor_model')
sim14_1 <- simulation7[1:8,]
sim14_1 <- as.vector(sim14_1)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP3
alpha <- rep(alpha, 3)
methods <- c(rep('Sc_Learner', 8), rep('SC', 8), rep('DiD',8))
sim14_1 <- cbind(sim14_1, alpha, methods)
sim14_1 <- as.data.frame(sim14_1)
sim14_1[,1] <- as.numeric(as.character(sim14_1[,1]))
sim14_1[,2] <- as.numeric(as.character(sim14_1[,2]))
p22<-ggplot(sim14_1, aes(x=alpha, y=sim14_1, group=methods)) +
  geom_line(aes(color=methods))+
  geom_point(aes(color=methods)) +
  labs(title="Percentage of rejections - DGP3, T = 400",x="alpha/sd", y = "")


library(ggpubr)
final_figure = ggarrange(p4, p22, common.legend = T)
jpeg('./figures_and_tables/figures_main_text/Figure5.jpeg')
print(final_figure)
dev.off()


final_figure_appendix <- ggarrange(p7, p10, p16, p19, common.legend = T)
jpeg('./figures_and_tables/figures_supplement/Figure7.jpeg')
print(final_figure_appendix)
dev.off()