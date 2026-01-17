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

working_directory <- '.'
load(paste0(working_directory, '/saved_results/oracle.RData'))



sd_DGP1 = compute_standard_deviation(50, 300, 1, "Linear_FM")
sim9 <- simulation5[[1]][,2:5]
sim9 <- as.vector(sim9)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP1
alpha <- rep(alpha, 4)
methods <- c(rep('SC', 8),rep('SC-Learner1', 8), rep('SC-Learner2', 8), rep('DiD',8))
sim9 <- cbind(sim9, alpha, methods)
sim9 <- as.data.frame(sim9)
sim9[,1]<- as.numeric(as.character(sim9[,1]))
sim9[,2]<- as.numeric(as.character(sim9[,2]))
p1<-ggplot(sim9, aes(x=alpha, y=sim9, group=methods)) +
  geom_line(aes(color=methods))+
  geom_point(aes(color=methods)) +
  labs(title="Percentage of rejections - DGP1",x="alpha/sd", y = "")


sd_DGP1 = compute_standard_deviation(50, 300, 1, "Factor_model")
sim14 <- simulation10[[1]][,2:5]
sim14 <- as.vector(sim14)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP1
alpha <- rep(alpha, 4)
methods <- c(rep('SC', 8),rep('SC-Learner1', 8), rep('SC-Learner2', 8), rep('DiD',8))
sim14 <- cbind(sim14, alpha, methods)
sim14 <- as.data.frame(sim14)
sim14[,1]<- as.numeric(as.character(sim14[,1]))
sim14[,2]<- as.numeric(as.character(sim14[,2]))
p2<-ggplot(sim14, aes(x=alpha, y=sim14, group=methods)) +
  geom_line(aes(color=methods))+
  geom_point(aes(color=methods)) +
  labs(title="Percentage of rejections - DGP3",x="alpha/sd", y = "")



sd_DGP1 = compute_standard_deviation(50, 300, 0.1, "logit")
sim10 <- simulation6[[1]][,2:5]
sim10 <- as.vector(sim10)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP1
alpha <- rep(alpha, 4)
methods <- c(rep('SC', 8),rep('SC-Learner1', 8), rep('SC-Learner2', 8), rep('DiD',8))
sim10 <- cbind(sim10, alpha, methods)
sim10 <- as.data.frame(sim10)
sim10[,1]<- as.numeric(as.character(sim10[,1]))
sim10[,2]<- as.numeric(as.character(sim10[,2]))
p3<-ggplot(sim10, aes(x=alpha, y=sim10, group=methods)) +
  geom_line(aes(color=methods))+
  geom_point(aes(color=methods)) +
  labs(title="Percentage of rejections - DGP2(a)",x="alpha/sd", y = "")


sd_DGP1 = compute_standard_deviation(50, 300, 1, "logit")
sim11 <- simulation7[[1]][,2:5]
sim11 <- as.vector(sim11)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP1
alpha <- rep(alpha, 4)
methods <- c(rep('SC', 8),rep('SC-Learner1', 8), rep('SC-Learner2', 8), rep('DiD',8))
sim11 <- cbind(sim11, alpha, methods)
sim11 <- as.data.frame(sim11)
sim11[,1]<- as.numeric(as.character(sim11[,1]))
sim11[,2]<- as.numeric(as.character(sim11[,2]))
p4<-ggplot(sim11, aes(x=alpha, y=sim11, group=methods)) +
  geom_line(aes(color=methods))+
  geom_point(aes(color=methods)) +
  labs(title="Percentage of rejections - DGP2(b)",x="alpha/sd", y = "")



sd_DGP1 = compute_standard_deviation(10, 300, 0.1, "Polynomial")
sim6 <- simulation3[[1]][,2:5]
sim6 <- as.vector(sim6)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP1
alpha <- rep(alpha, 4)
methods <- c(rep('SC', 8),rep('SC-Learner1', 8), rep('SC-Learner2', 8), rep('DiD',8))
sim6 <- cbind(sim6, alpha, methods)
sim6 <- as.data.frame(sim6)
sim6[,1]<- as.numeric(as.character(sim6[,1]))
sim6[,2]<- as.numeric(as.character(sim6[,2]))
p5<-ggplot(sim6, aes(x=alpha, y=sim6, group=methods)) +
  geom_line(aes(color=methods))+
  geom_point(aes(color=methods)) +
  labs(title="Percentage of rejections - DGP4(a)",x="alpha/sd", y = "")


sd_DGP1 = compute_standard_deviation(10, 300, 1, "Polynomial")
sim7 <- simulation4[[1]][,2:5]
sim7 <- as.vector(sim7)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP1
alpha <- rep(alpha, 4)
methods <- c(rep('SC', 8),rep('SC-Learner1', 8), rep('SC-Learner2', 8), rep('DiD',8))
sim7 <- cbind(sim7, alpha, methods)
sim7 <- as.data.frame(sim7)
sim7[,1]<- as.numeric(as.character(sim7[,1]))
sim7[,2]<- as.numeric(as.character(sim7[,2]))
p6<-ggplot(sim7, aes(x=alpha, y=sim7, group=methods)) +
  geom_line(aes(color=methods))+
  geom_point(aes(color=methods)) +
  labs(title="Percentage of rejections - DGP4(b)",x="alpha/sd", y = "")


sd_DGP1 = compute_standard_deviation(50, 300, 0.1, "Cos")
sim4 <- simulation1[[1]][,2:5]
sim4 <- as.vector(sim4)
alpha = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 1, 1.5)/sd_DGP1
alpha <- rep(alpha, 4)
methods <- c(rep('SC', 8),rep('SC-Learner1', 8), rep('SC-Learner2', 8), rep('DiD',8))
sim4 <- cbind(sim4, alpha, methods)
sim4 <- as.data.frame(sim4)
sim4[,1]<- as.numeric(as.character(sim4[,1]))
sim4[,2]<- as.numeric(as.character(sim4[,2]))
p7<-ggplot(sim4, aes(x=alpha, y=sim4, group=methods)) +
  geom_line(aes(color=methods))+
  geom_point(aes(color=methods)) +
  labs(title="Percentage of rejections - DGP5",x="alpha/sd", y = "")





library(ggpubr)
fig1 = ggarrange(p1, p3, p4, p2, common.legend = T)
fig2 = ggarrange(p5, p6, p7, common.legend = T)

jpeg('./figures_and_tables/figures_supplement/Figure5.jpeg')
print(fig1)
dev.off()

jpeg('./figures_and_tables/figures_supplement/Figure6.jpeg')
print(fig2)
dev.off()
