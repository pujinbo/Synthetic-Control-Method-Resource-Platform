##############################################################################################
########### Construction of dataset per state
##############################################################################################

## Data downloaded from bfrss 
#new_data <- read.table('/home/davide/Desktop/research/data/bfrss/data1_SC.txt')
new_data <- read.table('./data1_SC.txt')

### Construct Data Sets
## Consider only adults from 18 to 64 years old
new_data <- new_data[new_data$X.AGEG5YR %in% c(1:9),]

## Construct our y to be childless adult 
## We drop missing values for childres
new_data <- new_data[is.na(new_data$CHILDREN)==F, ]
## Drop also CHILDREN being 99 
new_data <- new_data[new_data$CHILDREN!=99, ]
new_data2 <- new_data[new_data$CHILDREN == 88,]

## Construct our y to be childless adult 
## We drop missing values for childres
new_data <- new_data[is.na(new_data$CHILDREN)==F, ]
## Drop also CHILDREN being 99 
new_data <- new_data[new_data$CHILDREN!=99, ]
new_data2 <- new_data[new_data$CHILDREN == 88,]

##############################################
################## Construct Output Variable 
##############################################
Tennessee <- new_data[new_data$X.STATE == 47,]
Tennessee_Childless <- Tennessee[Tennessee$CHILDREN==88,]

## For each month and each year sum individuals with health care 

## Notice: Y is constructed in reverse time ordering, we need to revert the sequence at the end:
number_observations  <- y <- matrix(NA, nrow=12, ncol=length(unique(Tennessee_Childless$IYEAR)[-2]))
k <- 1
for(j in unique(Tennessee_Childless$IYEAR)[-2]){
  for (i in c(12:1)){
    ## We record people having health insurance  
    y[i,k] <- sum(Tennessee_Childless[Tennessee_Childless$IMONTH == i & Tennessee_Childless$IYEAR == j,]$HLTHPLAN == 1)/length(Tennessee_Childless[Tennessee_Childless$IMONTH == i & Tennessee_Childless$IYEAR == j,]$HLTHPLAN)
    number_observations[i,k] <- length(Tennessee_Childless[Tennessee_Childless$IMONTH == i & Tennessee_Childless$IYEAR == j,]$HLTHPLAN)
  }
  k <- k +1
}

## Do the same by looking at MEDCOST variable

number_observations_medcost  <- medcost <- matrix(NA, nrow=12, ncol=length(unique(Tennessee_Childless$IYEAR)[-2]))
k <- 1
Tennessee_Childless_medcost <- Tennessee_Childless[is.na(Tennessee_Childless$MEDCOST) ==F,]
Tennessee_Childless_medcost <- Tennessee_Childless_medcost[Tennessee_Childless_medcost$MEDCOST != 9,]
Tennessee_Childless_medcost <- Tennessee_Childless_medcost[Tennessee_Childless_medcost$MEDCOST != 7,]

for(j in unique(Tennessee_Childless$IYEAR)[-2]){
  for (i in c(12:1)){
    
    medcost[i,k] <- sum(Tennessee_Childless_medcost[Tennessee_Childless_medcost$IMONTH == i & Tennessee_Childless_medcost$IYEAR == j,]$MEDCOST == 1)/length(Tennessee_Childless_medcost[Tennessee_Childless_medcost$IMONTH == i & Tennessee_Childless_medcost$IYEAR == j,]$MEDCOST)
    number_observations_medcost[i,k] <- length(Tennessee_Childless_medcost[Tennessee_Childless_medcost$IMONTH == i & Tennessee_Childless_medcost$IYEAR == j,]$MEDCOST)
  }
  k <- k +1
}

## There are some missing values but we can impute. First we can drop 2016 and 2017 
library(imputeTS)
y_ts <- as.vector(y)
med_ts <- as.vector(medcost)
## Revert back the order of the time serie
y_ts <- rev(y_ts)
med_ts <- rev(med_ts)
## Intrapolation 
y_ts <- na.interpolation(y_ts) ## y - share of health insurance coverage
med_ts <- na.interpolation(med_ts) ## med_ts - share of individuals not able to afford health care expenses

###########################################################
###################### Repeat the same for all other states : Here if save the files
###########################################################
## Uncomment to TRUE if want to re-run the data processing 
run_for_states = FALSE
if(run_for_states){
### We create the same measure for all other states with childless adults 
XXX <- matrix(NA, nrow=300, ncol=length(unique(new_data2$X.STATE)[-43]))
v = 1
for (u in unique(new_data2$X.STATE)[-43]){
  number_observations  <- y <- matrix(NA, nrow=12, ncol=length(unique(Tennessee_Childless$IYEAR)[-2]))
  k <- 1
  for(j in unique(Tennessee_Childless$IYEAR)[-2]){
    for (i in c(12:1)){
      ## We record people having health insurance  
      y[i,k] <- sum(new_data2[new_data2$IMONTH == i & new_data2$IYEAR == j & new_data2$X.STATE == u,]$MEDCOST == 1)/length(new_data2[new_data2$IMONTH == i 
                                                                                                                                     & new_data2$IYEAR == j 
                                                                                                                                     & new_data2$X.STATE == u,]$MEDCOST)
      number_observations[i,k] <- length(new_data2[new_data2$IMONTH == i & new_data2$IYEAR == j & new_data2$X.STATE == u,]$MEDCOST)
    }
    k <- k +1
  }
  ## Revert the time ordering from past to future
  y <- rev(as.vector(y))
  XXX[,v] <- y
  v = v+1
}


## Drop last three columns which are Guam, Puerto Rico and Virgin Islands
XXX <- XXX[,-c(51,52,53)]
## Impute missing values in XX
XXX <- apply(XXX, 2, na.interpolation)
colnames(XXX)<- unique(new_data$X.STATE)[-c(43,52,53,54)]

write.table(XXX, './MEDCOST_STATES_CHILDLESS_ADULTS.txt')


## Repeat the sam only with adults 


XX <- matrix(NA, nrow=300, ncol=length(unique(new_data$X.STATE)[-43]))
v = 1
for (u in unique(new_data$X.STATE)[-43]){
  number_observations  <- y <- matrix(NA, nrow=12, ncol=length(unique(Tennessee_Childless$IYEAR)[-2]))
  k <- 1
  for(j in unique(Tennessee_Childless$IYEAR)[-2]){
    for (i in c(12:1)){
      ## We record people having health insurance  
      y[i,k] <- sum(new_data[new_data$IMONTH == i & new_data$IYEAR == j & new_data$X.STATE == u,]$MEDCOST == 1)/length(new_data[new_data$IMONTH == i 
                                                                                                                                & new_data$IYEAR == j 
                                                                                                                                & new_data$X.STATE == u,]$MEDCOST)
      number_observations[i,k] <- length(new_data[new_data$IMONTH == i & new_data$IYEAR == j & new_data$X.STATE == u,]$MEDCOST)
    }
    k <- k +1
  }
  y <- rev(as.vector(y))
  XX[,v] <- y
  v = v+1
}


## Drop last three columns which are Guam, Puerto Rico and Virgin Islands
XX <- XX[,-c(51,52,53)]
## Impute missing values in XX
XX <- apply(XX, 2, na.interpolation)
colnames(XX)<- unique(new_data$X.STATE)[-c(43,52,53,54)]




write.table(XX, './MEDCOST_STATES_ADULTS.txt')



### We create the same measure for all other states with childless adults 
XXXX <- matrix(NA, nrow=300, ncol=length(unique(new_data$X.STATE)[-43]))
v = 1
for (u in unique(new_data$X.STATE)[-43]){
  number_observations  <- y <- matrix(NA, nrow=12, ncol=length(unique(Tennessee_Childless$IYEAR)[-2]))
  k <- 1
  for(j in unique(Tennessee_Childless$IYEAR)[-2]){
    for (i in c(12:1)){
      ## We record people having health insurance  
      y[i,k] <- sum(new_data[new_data$IMONTH == i & new_data$IYEAR == j & new_data$X.STATE == u,]$HLTHPLAN == 1)/length(new_data[new_data$IMONTH == i 
                                                                                                                                 & new_data$IYEAR == j 
                                                                                                                                 & new_data$X.STATE == u,]$HLTHPLAN)
      number_observations[i,k] <- length(new_data[new_data$IMONTH == i & new_data$IYEAR == j & new_data$X.STATE == u,]$HLTHPLAN)
    }
    k <- k +1
  }
  y <- rev(as.vector(y))
  XXXX[,v] <- y
  v = v+1
}


## Drop last three columns which are Guam, Puerto Rico and Virgin Islands
XXXX <- XXXX[,-c(51,52,53)]
## Impute missing values in XX
XXXX <- apply(XXXX, 2, na.interpolation)
colnames(XXXX)<- unique(new_data$X.STATE)[-c(43,52,53,54)]




write.table(XXXX, './HEALTHPLAN_STATES_ADULTS.txt')




### We create the same measure for all other states with childless adults 
XXXXX <- matrix(NA, nrow=300, ncol=length(unique(new_data2$X.STATE)[-43]))
v = 1
for (u in unique(new_data2$X.STATE)[-43]){
  number_observations  <- y <- matrix(NA, nrow=12, ncol=length(unique(Tennessee_Childless$IYEAR)[-2]))
  k <- 1
  for(j in unique(Tennessee_Childless$IYEAR)[-2]){
    for (i in c(12:1)){
      ## We record people having health insurance  
      y[i,k] <- sum(new_data2[new_data2$IMONTH == i & new_data2$IYEAR == j & new_data2$X.STATE == u,]$HLTHPLAN == 1)/length(new_data2[new_data2$IMONTH == i 
                                                                                                                                      & new_data2$IYEAR == j 
                                                                                                                                      & new_data2$X.STATE == u,]$HLTHPLAN)
      number_observations[i,k] <- length(new_data2[new_data2$IMONTH == i & new_data2$IYEAR == j & new_data2$X.STATE == u,]$HLTHPLAN)
    }
    k <- k +1
  }
  y <- rev(as.vector(y))
  XXXXX[,v] <- y
  v = v+1
}


## Drop last three columns which are Guam, Puerto Rico and Virgin Islands
XXXXX <- XXXXX[,-c(51,52,53)]
## Impute missing values in XX
XXXXX <- apply(XXXXX, 2, na.interpolation)
colnames(XXXXX)<- unique(new_data2$X.STATE)[-c(43,52,53,54)]




write.table(XXXXX, './HEALTHPLAN_STATES_Childless_ADULTS.txt')

}





######################################################################################################
##### Repeat the same for employment variable
######################################################################################################


employment_do = F
if(employment_do){
new_data <- read.table('./data1_SC.txt')


### Construct Data Sets
## Consider only adults from 18 to 64 years old
new_data <- new_data[new_data$X.AGEG5YR %in% c(1:9),]

## Construct our y to be childless adult 
## We drop missing values for childres
new_data <- new_data[is.na(new_data$CHILDREN)==F, ]
## Drop also CHILDREN being 99 
new_data <- new_data[new_data$CHILDREN!=99, ]
new_data2 <- new_data[new_data$CHILDREN == 88,]

## Construct our y to be childless adult 
## We drop missing values for childres
new_data <- new_data[is.na(new_data$CHILDREN)==F, ]
## Drop also CHILDREN being 99 
new_data <- new_data[new_data$CHILDREN!=99, ]
new_data2 <- new_data[new_data$CHILDREN == 88,]

##############################################
################## Construct Output Variable 
##############################################
Tennessee <- new_data[new_data$X.STATE == 47,]


## For each month and each year sum individuals with health care 

## Notice: Y is constructed in reverse time ordering, we need to revert the sequence at the end:
number_observations  <- employ <- matrix(NA, nrow=12, ncol=length(unique(Tennessee$IYEAR)[-2]))
k <- 1
for(j in unique(Tennessee$IYEAR)[-2]){
  for (i in c(12:1)){
    ## We record people having health insurance  
    employ[i,k] <- sum(Tennessee[Tennessee$IMONTH == i & Tennessee$IYEAR == j,]$EMPLOY %in% c(1,2))/length(Tennessee[Tennessee$IMONTH == i & Tennessee$IYEAR == j,]$EMPLOY)
    number_observations[i,k] <- length(Tennessee[Tennessee$IMONTH == i & Tennessee$IYEAR == j,]$EMPLOY)
  }
  k <- k +1
}


library(imputeTS)
employ_ts <- rev(as.vector(employ))
employ_ts[employ_ts == 0] <- NA
employ_ts <- na.interpolation(employ_ts) 

###########################################################
###################### Repeat the same for all other states : Here if save the files
###########################################################
## Uncomment to TRUE if want to re-run the data processing 
run_for_states = T
if(run_for_states){
  ### We create the same measure for all other states with childless adults 
  XXXe <- matrix(NA, nrow=300, ncol=length(unique(new_data2$X.STATE)[-43]))
  v = 1
  for (u in unique(new_data2$X.STATE)[-43]){
    number_observations  <- emp <- matrix(NA, nrow=12, ncol=length(unique(Tennessee$IYEAR)[-2]))
    k <- 1
    for(j in unique(Tennessee$IYEAR)[-2]){
      for (i in c(12:1)){
        ## We record people having health insurance  
        emp[i,k] <- sum(new_data2[new_data2$IMONTH == i & new_data2$IYEAR == j & new_data2$X.STATE == u,]$EMPLOY %in% c(1,2))/length(new_data2[new_data2$IMONTH == i 
                                                                                                                                       & new_data2$IYEAR == j 
                                                                                                                                       & new_data2$X.STATE == u,]$EMPLOY)
        number_observations[i,k] <- length(new_data2[new_data2$IMONTH == i & new_data2$IYEAR == j & new_data2$X.STATE == u,]$EMPLOY)
      }
      k <- k +1
    }
    ## Revert the time ordering from past to future
    emp <- rev(as.vector(emp))
    XXXe[,v] <- emp
    v = v+1
  }
  
  ## Drop last three columns which are Guam, Puerto Rico and Virgin Islands
  XXXe <- XXXe[,-c(51,52,53)]
  ## Impute missing values in XX
  XXXe <- apply(XXXe, 2, na.interpolation)
  colnames(XXXe)<- unique(new_data$X.STATE)[-c(43,52,53,54)]
  XXXe <- cbind(XXXe, employ_ts)
  write.table(XXXe, './employment_BFRSS.txt')
}
  
}