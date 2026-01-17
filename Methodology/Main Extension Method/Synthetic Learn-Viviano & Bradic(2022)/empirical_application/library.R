### Library for empirical analysis 

library(e1071)
library(xgboost)
library(glmnet)
library(boot)
library(devtools)
library('gsynth')
library(ggplot2)
library(tidyr)
library(panelView)
library(zoo)
library('Synth')
library(quadprog)
library(randomForest)
library(rnn)
## Run Exponential algorithm 
Exp_algorithm <- function(output, Experts, eta){
  n <- length(output)
  p <- dim(Experts)[2]
  weights <- rep(1/p, p)
  losses <- (Experts - output)**2
  sum_losses <- apply(losses, 2, sum)
  weights <- exp(-eta*sum_losses)
  weights <- weights/sum(weights)
  if(is.na(weights[1])){
    for (i in 1:n){
      losses = (Experts[i,] - output[i])**2
      weights = weights*exp(-eta*losses)
      weights = weights/sum(weights)
      if(is.na(weights[1])){weights <- rep(1/p, p)}}}
  
  if(is.na(weights[1])){
    weights <- NA
  } else if(sum(abs(weights)) > 0){
    weights <- weights/sum(weights) 
  } else {weights = rep(1/(p), p )}
  
  return(weights)
}

## Compute weights for synthetic control
SC_weights <- function(X, y, constraint = 1){
  p <- dim(X)[2]
  Rinv <- solve(chol(t(X) %*% X));
  C <- cbind(rep(1,p), diag(p))
  b <- c(constraint,rep(0,p))
  d <- t(y) %*% X  
  weights <- solve.QP(Dmat = Rinv, factorized = TRUE, dvec = d, Amat = C, bvec = b, meq = 1)$solution
  return(weights)}

## Generate experts for exponential algorithms

## T_0: starting period
## T_1: end of the period for training
## N: total length of the period
generate_experts <- function(X, y,N,T_0,xgboost=F, T1,
                             randforest = F, lasso=F, sc_control=F, RNN=F, SVM=T, double_lasso=F, lag_y = NA, 
                             constraint=1, arima=F, did =F, external_covariates, factor_model = F){
  
  
  if(is.na(lag_y) == F){lag_y1 <- lag_y[T_0:T1]}
  X1 <- X[T_0:T1, ]
  y1 <- y[T_0:T1]
  p <- dim(X)[2]
  ## first expert is the mean
  St_experts <- NA
  if(xgboost){
  ## Compute xgboost
  xgmodel <- xgboost(data=as.matrix(X1), label=y1, nround=70, verbose = 0)
  xgb_expert <- predict(xgmodel, newdata = X,params=list(eta=0.15, max_depth=3,gamma=0.1))
  Experts <- xgb_expert
  } 
  if(factor_model){
    factors <- eigen(X[T_0:T1,]%*%t(X[T_0:T1,]))$vectors[, 1]
    ## Estimate the factors on the pre-treatment period 
    factors_est <- cv.glmnet(x= as.matrix(X1),y=as.vector(factors), 
                             lambda = seq(from=exp(-10),to= exp(2), length=79), 
                             nfolds = 5)
    factors <- predict(factors_est, newx=as.matrix(X))
    
    ## Estimate loadings via LS 
    beta_hat_factors <- solve(t(cbind(1, factors[T_0:T1,]))%*%cbind(1, factors[T_0:T1,]),t(cbind(1, factors[T_0:T1,]))%*%y1)
    predictions_factor <- cbind(1, factors)%*%beta_hat_factors
    Experts <- predictions_factor
  }
  if (SVM){
    
    #   svm_model <- tune.svm(y = y1, x=X1,kernel=c('polynomial'),cross=3, epsilon=seq(from= 0.01, to = 0.5, by=0.05), 
    #                          cost=seq(from= 0.01, to = 0.5, by=0.05), gamma=seq(from = 0.1, to = 3, length=10))
    #    svm_model <- svm(y = y1, x=X1, gamma=svm_model$best.parameters[1], epsilon = svm_model$best.parameters[3], cost = svm_model$best.parameters[2], kernel='polynomial' )
    svm_model <- svm(y = y1, x=X1, kernel='polynomial' )
    svm_pred <- predict(svm_model, newdata=X)
    Experts <- cbind(Experts, svm_pred)
  } 
  
  
  if(arima){
    arima_model <- arima(y1, order = c(1,0,0), xreg=X1)
    arima <- predict(arima_model, newxreg=X)$pred
    arima_model <- arima(y1, order = c(0,0,1), xreg=X1)
    arima2 <- predict(arima_model, newxreg=X)$pred
    #arima_model <- arima(y1, order = c(1,0,0))
    #arima <- predict(arima_model)$pred
    #arima_model <- arima(y1, order = c(0,0,1))
    #arima2 <- predict(arima_model)$pred
    
    Experts <- cbind(Experts, arima, arima2)
  }
  if(randforest){
    colnames(external_covariates) <- paste0('E', c(1:dim(external_covariates)[2]))
    datat <- as.data.frame(cbind(X1, external_covariates[T_0:T1,]))
     datap <- cbind(X, external_covariates)
    names(datap) <- names(datat)
    
    Random_forest <- randomForest(y1~.,data = as.data.frame(datat), maxnodes = 20) 
    RF_expert <- predict(Random_forest, newdata=as.data.frame(datap))
    Experts <- cbind(Experts,RF_expert)
  }
  if (RNN){
    yy_train <- array(y1, c(1,length(y1), 1))
    XX_train <- array(X1, c(1, dim(X1)[1], dim(X1)[2]))
    rnn_model <- suppressMessages(trainr(Y=yy_train, X=XX_train, learningrate = 0.01, learningrate_decay = 0.4, hidden_dim = 15, numepochs = 300, use_bias = T))
    Rec_NN <- as.vector(predictr(rnn_model,array(X, c(1, dim(X)[1], dim(X)[2]))))
    Experts <- cbind(Experts, Rec_NN)
  }
  ## compute synthetic controls
  if(lasso){
    
    if(is.na(lag_y)){
    
      
    Lassocv <- cv.glmnet(x= as.matrix(X1),y=as.vector(y1), 
                         lambda = seq(from=exp(-10),to= exp(-1), length=79), 
                         nfolds = 5)
    Lasso <- glmnet(x=as.matrix(X1), y=y1, lambda=Lassocv$lambda.min)
    Lasso_expert <- predict(Lasso, newx=as.matrix(X))
    
    
        Experts <- cbind(Lasso_expert, Experts)
        St_experts <- Experts} else {
          ## use also lag_y variable 
          XX1 <- cbind(X1, lag_y1)
          XX_t <- cbind(X[1:length(lag_y),], lag_y)
          Lassocv <- cv.glmnet(x= as.matrix(XX1),y=as.vector(y1), 
                               lambda.min.ratio = 0.5, 
                               nfolds = 5)
          Lasso <- glmnet(x=as.matrix(XX1), y=y1, lambda=0.001)
               y_hat <- rep(NA, dim(X)[1])
          y_hat[1:length(lag_y)] <- predict(Lasso, newx = as.matrix(XX_t))
          y_hat_next =   y_hat[length(y1)]
          TT <- length(lag_y)
          for(i in 1:length(c(TT:(N-1)))){
          lag_y <- c(lag_y, y_hat_next)
          XX_t <- cbind(X[1:(length(lag_y)),], lag_y)
          pred <- predict(Lasso, newx=as.matrix(XX_t))
          y_hat_next =   pred[length(pred)]
          }
        Lasso_expert <- predict(Lasso, newx = as.matrix(XX_t))
          Experts <- cbind(Experts, Lasso = Lasso_expert)
          St_experts <- Experts
        }
    
  }
  
  if(sc_control){
    ## Loop over all possible constraints passed
    for(i in constraint){ 
    ## SC control is the last expert
    sc_w <- SC_weights(X=apply(X1, 2 , function(x) x - mean(x)),  y=(y1 - mean(y1)), constraint = i)
    sc_pred <- apply(X, 1, function(x) sc_w%*%x) + mean(y1) - sc_w%*%apply(X1, 2, mean)
    Experts <- cbind(Experts, sc_pred)}
  }
  
  if(double_lasso){
    Lassocv <- cv.glmnet(x= as.matrix(X1),y=as.vector(y1), 
                         seq(from=exp(-10),to= exp(-3), length=79), 
                         nfolds = 5)
    Lasso <- glmnet(x=as.matrix(X1), y=y1, lambda=Lassocv$lambda.min)
    cc <- which(coef(Lasso) != 0 )
    cc <- cc[-1] + 1
    ## remove intercept
    newdata <- as.data.frame(cbind(y,X))
    n <- names(newdata)[cc]
    f<- as.formula(paste("y ~", paste(n[!n %in% "y"], collapse = " + ")))
    reg <- lm(f , data =  newdata[1:T_0, ])
    double_lasso <- predict(reg, newdata = newdata)
    Experts <- cbind(Experts, double_lasso)
  }
  
  if (did){
    did <- mean(y1) - mean(apply(X1, 2, mean)) + apply(X, 1, mean)
    did <- as.vector(did)
    Experts <- cbind(Experts, did)
  }
  
  return(Experts)}

helper_method <- function(string, X, y,  eta){
  if(string=="exponential_forecaster"){
    weights <- Exp_algorithm(output=y, Experts =X, eta=eta)
  } else if(string=="synthetic_control"){weights <- SC_weights(y=y,X=X)}
  return(weights)}


## Function for bootstrap 

function4boot_TE <- function(Z, method="exponential_forecaster", T_0, t_stat = '1'){
  
  N <- dim(Z)[1]
  Experts <- Z[,-1] 
  y <- Z[,1]
  y1 <- y[-c(T_0:N)]
  Experts1 <- Experts[-c(T_0:N),]
  k <- 1
  
  weights <- helper_method(method,  y=y1, X=Experts1,eta=1)
  Y_expN <- apply(Experts[T_0:N,], 1, function(x) weights%*%x)
  
  ## Test on average null 
  if(t_stat == '1'){
  losses <- sum((Y_expN - y[T_0:N])**2)/sqrt(length(T_0:N)) } else {
    losses <- (sum((Y_expN - y[T_0:N]))/sqrt(length(T_0:N)))**2
  }
  return(losses)
  
}


function4boot_DiD <- function(Z, T_0, t_stat = '1'){
  
  N <- dim(Z)[1]
  X <- Z[,-1] 
  y <- Z[,1]
  y1 <- y[-c(T_0:N)]
  X1 <- X[-c(T_0:N),]
  k <- 1
  
  Y_expN <- mean(y1) - mean(apply(X1, 2, mean)) + apply(X, 1, mean)
  
  ## Test on average null 
  if(t_stat == '1'){
  losses <- sum((Y_expN[T_0:N] - y[T_0:N])**2)/sqrt(length(T_0:N)) } else{ 
    losses <- (sum((Y_expN[T_0:N] - y[T_0:N]))/sqrt(length(T_0:N)))**2
    }
  return(losses)
  
}



