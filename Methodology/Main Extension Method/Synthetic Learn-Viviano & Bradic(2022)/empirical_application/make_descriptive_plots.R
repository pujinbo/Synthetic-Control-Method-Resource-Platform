library(gsynth)
library(ggplot2)
source('./do_file.R')
source('./my_panel_view_function.R')


###############################################
### Figure 1 (quarterly)
###############################################


XXX <- read.table('./MEDCOST_STATES_CHILDLESS_ADULTS.txt')
colnames(XXX) <- sapply(colnames(XXX), function(x) substring(x,2))
data_ts <- cbind(XXX, Tenn = med_ts)

data_ts_new <- matrix(nrow = 100, ncol = dim(data_ts)[2])
med_ts_new <- rep(NA, 100)
k <- 1
for(i in 1:100){
  data_ts_new[i,] <- apply(as.matrix(data_ts[k:(k+2), ]), 2, function(x) mean(unlist(x)))
  med_ts_new[i] <- mean(med_ts[k:(k+2)]) 
  k <- k + 3
}
data_ts_southern <- data_ts_new[,which(colnames(data_ts)%in% c(51,5,22,37,45,28,1,13,12,48,54, 'Tenn'))]
## Visualize with panel view only for Southern States 
names_country_southern <- c(51,5,22,37,45,28,1,13,12,48, 54, 47) ## Last is tennessee
names_country <- c(unique(new_data$X.STATE)[-c(43,52,53,54)], unique(new_data$X.STATE)[43])
for(i in 1:(length(names_country_southern))){
  if(i==1){
    xx <- data_ts_southern[,i]
    ll <- loess(xx ~ c(1:length(med_ts_new)), span=0.15)
    xx <- predict(ll)
    dd <- cbind(xx, rep(names_country[i], length(xx)), rep(0, length(xx)))} else if (i == length(names_country_southern)){
      xx <- data_ts_southern[,i]
      ll <- loess(xx ~ c(1:length(med_ts_new)), span=0.15)
      xx <- predict(ll)
      dd <- rbind(dd,cbind(xx, rep(names_country[i], length(xx)), c(rep(0, 51), rep(1, length(xx) - 51))))
    }else{
      xx <- data_ts_southern[,i]
      ll <- loess(xx ~ c(1:length(med_ts_new)), span=0.15)
      xx <- predict(ll)
      dd <- rbind(dd,cbind(xx, rep(names_country[i], length(xx)), rep(0,length(xx))))
    }
} 
dd[which.max(dd[,1]),1] <- dd[which.max(dd[,1]) - 1,1]
dates <- seq(as.Date("1993/1/1"), as.Date("2017/12/31"), by = "month")
dates <- dates[seq(from = 1, to = length(dates), by = 3)]
dd <- cbind(dd, rep(as.character(dates), length(names_country_southern)))
dd <- as.data.frame(dd)
dd[,1] <- as.numeric(as.character(dd[,1]))
dd[,3] <- as.numeric(as.character(dd[,3]))
names(dd) <- c('MEDCOST', 'State', 'Policy','Time')
dd$Time <- sapply(dd$Time, function(x)(substr(x, 1, 7)))

library(ggplot2)
library(ggpubr)
library(panelView)
pp <- my_panelView(MEDCOST ~ Policy, data = dd, index = c("State","Time"), 
                   type = "raw", main = "", legendOff = F, axis.adjust = T, axis.lab = 'unit', 
                   my_gap = 10)
jpeg('./plots/descript1.jpeg')
pp
dev.off()
#############################################################################################################################
################### Plots with Summary Statistics
#############################################################################################################################
## Summary statistics 
## Here we construct summary statistics 
## recall med_ts is reverted 
med_ts2017_2013 <- rev(med_ts)[1:60]
med_ts2006_2012 <- rev(med_ts)[61:145]
med_ts1993_2005 <- rev(med_ts)[146:300]
## Do the same for southerstates 
data_ts <- XXX
data_ts_southern <- data_ts[,which(colnames(data_ts)%in% c(51,5,22,37,45,28,1,13,12,48,54))]
## Now the serie goes from 1 being the farest one to the closest one
data_ts2017_2013 <- data_ts_southern[241:300,]
data_ts2006_2012 <- data_ts_southern[156:240,]
data_ts1993_2005 <- data_ts_southern[1:155,]
## Repeat with all states
data_tsUS2017_2013 <- data_ts[241:300,]
data_tsUS2006_2012 <- data_ts[156:240,]
data_tsUS1993_2005 <- data_ts[1:155,]




boxes1 <- c(med_ts2017_2013, med_ts2006_2012, med_ts1993_2005)
boxes2<- c(as.vector(as.matrix(data_ts2017_2013)), as.vector(as.matrix(data_ts2006_2012)) , as.vector(as.matrix(data_ts1993_2005)))
boxes3 <- c(as.vector(as.matrix(data_tsUS2017_2013)), as.vector(as.matrix(data_tsUS2006_2012)) , as.vector(as.matrix(data_tsUS1993_2005)))
period <- c(rep('2017-2013', length(med_ts2017_2013)), rep('2006-2012', length(med_ts2006_2012)), rep('1993-2005', length(med_ts1993_2005)))
boxes1 <- cbind(boxes1, period, rep('Tennessee', length(boxes1)))
period <- c(rep('2017-2013', length(data_ts2017_2013)), rep('2006-2012', length(data_ts2006_2012)), rep('1993-2005', length(data_ts1993_2005)))
boxes2 <- cbind(boxes2, period, rep('Southern States', length(boxes2)))
period <- c(rep('2017-2013', length(data_tsUS2017_2013)), rep('2006-2012', length(data_tsUS2006_2012)), rep('1993-2005', length(data_tsUS1993_2005)))
boxes3 <- cbind(boxes3, period, rep('US', length(boxes3)))
final_boxes <- rbind(boxes1, boxes2,boxes3)
final_boxes <- as.data.frame(final_boxes)
names(final_boxes)<- c('MEDCOST', "Period", 'Region')
final_boxes[,1] <- as.numeric(as.character(final_boxes[,1]))
p10 <- ggplot(final_boxes, aes(x = Period, y = MEDCOST, fill = Region)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "MEDCOST",
                     breaks = seq(0, 0.5, 0.2),
                     limits=c(0, 0.5)) +
  scale_x_discrete(name = "Period") +
  ggtitle("Health Care Access for Childless Adults") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15), legend.position = 'top', 
        legend.text = element_text(size=20))  + scale_fill_manual(values=c("chocolate1", 'lightgreen','plum3'))
jpeg('./plots/descript2.jpeg')
p10
dev.off()




############ Same Box Plot with Health Insurance Coverage


XXXXX <- read.table('./HEALTHPLAN_STATES_Childless_ADULTS.txt')
colnames(XXXXX) <- sapply(colnames(XXXXX), function(x) substring(x,2))

data_ts <- cbind(XXXXX, Tenn = y_ts)
data_ts_southern <- data_ts[,which(colnames(data_ts)%in% c(51,5,22,37,45,28,1,13,12,48,54, 'Tenn'))]
## Visualize with panel view only for Southern States 
names_country_southern <- c(51,5,22,37,45,28,1,13,12,48, 54,47) ## Last is tennessee
names_country <- c(unique(new_data$X.STATE)[-c(43,52,53,54)], unique(new_data$X.STATE)[43])
for(i in 1:(length(names_country_southern))){
  if(i==1){
    xx <- data_ts_southern[,i]
    ll <- loess(xx ~ c(1:length(med_ts)), span=0.05)
    xx <- predict(ll)
    dd <- cbind(xx, rep(names_country[i], length(xx)), rep(0, length(xx)))} else if (i == length(names_country_southern)){
      xx <- data_ts_southern[,i]
      ll <- loess(xx ~ c(1:length(med_ts)), span=0.05)
      xx <- predict(ll)
      dd <- rbind(dd,cbind(xx, rep(names_country[i], length(xx)), c(rep(0, 155), rep(1, length(xx) - 155))))
    }else{
      xx <- data_ts_southern[,i]
      ll <- loess(xx ~ c(1:length(med_ts)), span=0.05)
      xx <- predict(ll)
      dd <- rbind(dd,cbind(xx, rep(names_country[i], length(xx)), rep(0,length(xx))))
    }
} 
dates <- seq(as.Date("1993/1/1"), as.Date("2017/12/31"), by = "month")
dd <- cbind(dd, rep(as.character(dates), length(names_country_southern)))
dd <- as.data.frame(dd)
dd[,1] <- as.numeric(as.character(dd[,1]))
dd[,3] <- as.numeric(as.character(dd[,3]))
names(dd) <- c('HEALTHPLAN', 'State', 'Policy','Time')
dd$Time <- sapply(dd$Time, function(x)(substr(x, 1, 7)))



## Summary statistics 
## Here we construct summary statistics 
## recall med_ts is reverted 
y_ts2017_2013 <- rev(y_ts)[1:60]
y_ts2006_2012 <- rev(y_ts)[61:145]
y_ts1993_2005 <- rev(y_ts)[146:300]
## Do the same for southerstates 
hlth_data_ts <- XXXXX
hlth_data_ts_southern <- hlth_data_ts[,which(colnames(hlth_data_ts)%in% c(51,5,22,37,45,28,1,13,12,48,54))]
## Now the serie goes from 1 being the farest one to the closest one
hlth_data_ts2017_2013 <- hlth_data_ts_southern[241:300,]
hlth_data_ts2006_2012 <- hlth_data_ts_southern[156:240,]
hlth_data_ts1993_2005 <- hlth_data_ts_southern[1:155,]
## Repeat with all states
hlth_data_tsUS2017_2013 <- hlth_data_ts[241:300,]
hlth_data_tsUS2006_2012 <- hlth_data_ts[156:240,]
hlth_data_tsUS1993_2005 <- hlth_data_ts[1:155,]


boxes1 <- c(y_ts2017_2013, y_ts2006_2012, y_ts1993_2005)
boxes2<- c(as.vector(as.matrix(hlth_data_ts2017_2013)), as.vector(as.matrix(hlth_data_ts2006_2012)) , as.vector(as.matrix(hlth_data_ts1993_2005)))
boxes3 <- c(as.vector(as.matrix(hlth_data_tsUS2017_2013)), as.vector(as.matrix(hlth_data_tsUS2006_2012)) , as.vector(as.matrix(hlth_data_tsUS1993_2005)))
period <- c(rep('2013-2017', length(y_ts2017_2013)), rep('2006-2012', length(y_ts2006_2012)), rep('1993-2005', length(y_ts1993_2005)))
boxes1 <- cbind(boxes1, period, rep('Tennessee', length(boxes1)))
period <- c(rep('2013-2017', length(hlth_data_ts2017_2013)), rep('2006-2012', length(hlth_data_ts2006_2012)), rep('1993-2005', length(hlth_data_ts1993_2005)))
boxes2 <- cbind(boxes2, period, rep('Southern States', length(boxes2)))
period <- c(rep('2013-2017', length(hlth_data_tsUS2017_2013)), rep('2006-2012', length(hlth_data_tsUS2006_2012)), rep('1993-2005', length(hlth_data_tsUS1993_2005)))
boxes3 <- cbind(boxes3, period, rep('US', length(boxes3)))
final_boxes <- rbind(boxes1, boxes2,boxes3)
final_boxes <- as.data.frame(final_boxes)
names(final_boxes)<- c('HealthPlan', "Period", 'Region')
final_boxes[,1] <- as.numeric(as.character(final_boxes[,1]))
p9 <- ggplot(final_boxes, aes(x = Period, y = HealthPlan, fill = Region)) +
  geom_boxplot(alpha=0.7) +
  scale_y_continuous(name = "HealthPlan",
                     breaks = seq(0, 1, 0.2),
                     limits=c(0.6, 1)) +
  scale_x_discrete(name = "Period") +
  ggtitle("Insurance Coverage for Childless Adults") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15), legend.position = 'top', 
        legend.text = element_text(size=20))  + scale_fill_manual(values=c("chocolate1", 'lightgreen','plum3'))
jpeg('./plots/descript3.jpeg')
p9
dev.off()

ggarrange(p10, p9, common.legend = T)

