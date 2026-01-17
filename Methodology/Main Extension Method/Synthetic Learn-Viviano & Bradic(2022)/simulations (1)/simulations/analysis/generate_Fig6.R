######## Library 

library(ggplot2)
library(RColorBrewer)
library(grid)
library(datasets)
library(ggplot2)
fill <- "#4271AE"
lines <- "#1F3552"

working_directory <- '.'
load(paste0(working_directory, '/saved_results/Sim_expVSLS.RData'))
source(paste0(working_directory, '/libraries/library_generate_data.R'))

## DGP 1
sd_DGP1 = compute_standard_deviation(50, 300, 1, "Linear_FM")
sim1 <- as.vector(simulation1)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP1
alpha <- rep(alpha, 2)
methods <- c(rep('Exp SL 100 NIL', 8), rep('LS SL 100 NIL', 8))
sim1 <- cbind(sim1, alpha, methods)
sim2 <- as.vector(simulation1B)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP1
alpha <- rep(alpha, 2)
methods <- c(rep('Exp SL 10 NIL', 8), rep('LS SL 10 NIL', 8))
sim2 <- cbind(sim2, alpha, methods)
sim <- rbind(sim1, sim2)
sim <- as.data.frame(sim)
sim[,1]<- as.numeric(as.character(sim[,1]))
sim[,2]<- as.numeric(as.character(sim[,2])) 

p1<-ggplot(sim, aes(x=alpha, y=sim1, group=methods)) +
  geom_line(aes(color=methods))+
  geom_point(aes(color=methods)) +
  labs(title="Percentage of rejections - DGP1",x="alpha/sd", y = "Percentage of rejections")


sd_DGP2 = compute_standard_deviation(50, 300, 0.1, "logit")
sim1 <- as.vector(simulation2)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP2
alpha <- rep(alpha, 2)
methods <- c(rep('Exp SL 100 NIL', 8), rep('LS SL 100 NIL', 8))
sim1 <- cbind(sim1, alpha, methods)
sim2 <- as.vector(simulation2B)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP2
alpha <- rep(alpha, 2)
methods <- c(rep('Exp SL 10 NIL', 8), rep('LS SL 10 NIL', 8))
sim2 <- cbind(sim2, alpha, methods)
sim <- rbind(sim1, sim2)
sim <- as.data.frame(sim)
sim[,1]<- as.numeric(as.character(sim[,1]))
sim[,2]<- as.numeric(as.character(sim[,2]))

p2 <-ggplot(sim, aes(x=alpha, y=sim1, group=methods)) +
  geom_line(aes(color=methods))+
  geom_point(aes(color=methods)) +
  labs(title="Percentage of rejections - DGP2(a)",x="alpha/sd", y = "Percentage of rejections")


sd_DGP3 = compute_standard_deviation(10, 300, 0.1, "Polynomial")
sim1 <- as.vector(simulation3)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP3
alpha <- rep(alpha, 2)
methods <- c(rep('Exp SL 100 NIL', 8), rep('LS SL 100 NIL', 8))
sim1 <- cbind(sim1, alpha, methods)
sim2 <- as.vector(simulation3B)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP3
alpha <- rep(alpha, 2)
methods <- c(rep('Exp SL 10 NIL', 8), rep('LS SL 10 NIL', 8))
sim2 <- cbind(sim2, alpha, methods)
sim <- rbind(sim1, sim2)
sim <- as.data.frame(sim)
sim[,1]<- as.numeric(as.character(sim[,1]))
sim[,2]<- as.numeric(as.character(sim[,2]))

p3<-ggplot(sim, aes(x=alpha, y=sim1, group=methods)) +
  geom_line(aes(color=methods))+
  geom_point(aes(color=methods)) +
  labs(title="Percentage of rejections - DGP4(a)",x="alpha/sd", y = "Percentage of rejections")


sd_DGP4 = compute_standard_deviation(10, 300, 1, "Polynomial")
sim1 <- as.vector(simulation4)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP4
alpha <- rep(alpha, 2)
methods <- c(rep('Exp SL 100 NIL', 8), rep('LS SL 100 NIL', 8))
sim1 <- cbind(sim1, alpha, methods)
sim2 <- as.vector(simulation4B)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP4
alpha <- rep(alpha, 2)
methods <- c(rep('Exp SL 10 NIL', 8), rep('LS SL 10 NIL', 8))
sim2 <- cbind(sim2, alpha, methods)
sim <- rbind(sim1, sim2)
sim <- as.data.frame(sim)
sim[,1]<- as.numeric(as.character(sim[,1]))
sim[,2]<- as.numeric(as.character(sim[,2]))

p4<-ggplot(sim, aes(x=alpha, y=sim1, group=methods)) +
  geom_line(aes(color=methods))+
  geom_point(aes(color=methods)) +
  labs(title="Percentage of rejections - DGP4(b)",x="alpha/sd", y = "Percentage of rejections")


final_fig = ggarrange(p1, p2, p3, p4, common.legend = T)
jpeg('./figures_and_tables/figures_main_text/Figure6.jpeg')
print(final_fig)
dev.off() 