tryCatch(library(mvtnorm), error = function(e) NA)
library(boot)
library("quadprog")
library(DTMCPack)
library(foreach)
library(glmnet)
library(xgboost)
library(randomForest)
library(e1071)
library(permute)
library(rnn)


## Exponential weighting algorithm
## output: y vector
## Experts: matrix of experts
## eta: tuning parameter 
## return: weights via exponential weighting scheme

Exp_algorithm <- function(output, Experts, eta){
  n <- length(output)
  p <- dim(Experts)[2]
  weights <- rep(1/p, p)
  for (i in 1:n){
    losses = (Experts[i,] - output[i])**2
    weights = weights*exp(-eta*losses)
    weights = weights/sum(weights)}
  
## Check that weights are not-ill conditioned 
    if(is.na(weights[1])){
    weights <- rep(1/p, p)
  } else if(sum(abs(weights)) > 0){
    weights <- weights/sum(weights) 
  } else {weights = rep(1/(p), p )}
  
  return(weights)
}

## Return: Weights for the Synthetic Control method with constraint at one
## Input X, y output: weights
## factorized: use the factorized form (can be used only for T > p)

SC_weights <- function(X, y, factorized = T){
  p <- dim(X)[2]
  R <- t(X) %*% X
  ## Factorized case (see help(solve.QP))
  if(factorized) R <- solve(chol(t(X) %*% X))
  C <- cbind(rep(1,p), diag(p))
  b <- c(1,rep(0,p))
  d <- t(y) %*% X  
  weights <- tryCatch(solve.QP(Dmat = R, factorized = factorized, dvec = d, Amat = C, bvec = b, meq = 1)$solution, error=function(x){NA})
  return(weights)}

## Helper method: call the function depending on the string passed
helper_method <- function(string, X, y, eta){
  if(string=="exponential_forecaster"){
    weights <- Exp_algorithm(output=y, Experts =X,  eta=eta)
  } else if(string=="synthetic_control"){weights <- SC_weights(y=y,X=X)}
  return(weights)}


### Function to generate experts
## X matrix of covariates
## y otuput
## N total length of the period (all periods summed)
## T_0: pre-treatment period
## Strong experts: whether to include arima, SVM Xgboost
## fake experts: number of non informative experts
## weak experts: deprecated
## randforest: whether to include a random forest trained with package randomForest
## lasso: whether include lasso with tuning via CV
## sc_control: include synthetic control
## RNN: include recurrent neural network
## T_1: post treatment period of interst(T - T_0)
## If T_1 = NA, compute experts only on the pre-treatment period
# factorized: estimate SC with factorization for QP optimization 
generate_experts <- function(X, y,N,T_0,strong_experts=F, fake_experts = 10, weak_experts=F, 
                             randforest = F, lasso=F, sc_control=F, RNN=F, T1=NA, 
                             factorized = T, linear_xgboost = F, non_stationary = F){
  if(non_stationary) X = cbind(X, c(1:(dim(X)[1])))
  if(is.na(T1)){
    X1 <- X[1:(T_0), ]
    y1 <- y[1:(T_0)]} else {
      X1 <- X[T1:dim(X)[1], ]
      y1 <- y[T1:dim(X)[1]]
    }
  p <- dim(X)[2]
  Experts <- X
  St_experts <- NA
  if(strong_experts){
    
    ## Compute xgboost
    if(linear_xgboost == F){ 
    xgmodel <- xgboost(data=X1, label=y1, nround=70, verbose = 0)
    xgb_expert <- predict(xgmodel, newdata = X)
    } else {
      
      lambdas_alphas = expand.grid(seq(from = 0, to = 1, length = 10), seq(from = 0, to = 1, length = 10))
      error = rep(NA, 100)
      for(acc in 1:100) {
      j = lambdas_alphas[acc,]
      aa <- xgb.cv(data=X1, label=y1, nround=70, verbose = 0, nfold = 3, params = list(boost = 'gblinear', 
                                                                                 lambda = 0, alpha = j[2]))
      error[acc] = aa$evaluation_log[70, 4]
      }
      error = unlist(error)
      xgb_expert <- predict(xgmodel, newdata = X)
    }
    svm_model <- svm(y = y1, x=X1)
    svm_pred <- predict(svm_model, newdata=X)
    ## Arima might have optimization problem
    ## remove the expert if it cannot be optimized
    arima_model <- tryCatch(arima(y1, order = c(0,1,1), xreg=X1), error=function(e){NA})
    arima <- tryCatch(predict(arima_model, newxreg=X)$pred, error=function(e){NA})
    
    ## remove the columns of X from the experts
    if(is.na(arima) == F){
      St_experts <- cbind(xgb_expert,svm_pred, arima)
      Experts <- St_experts} else {
        St_experts <- cbind(xgb_expert,svm_pred)
        Experts <- St_experts
      }
  }
  if(randforest){
    Random_forest <- randomForest(y1~.,data = as.data.frame(X1)) 
    RF_expert <- predict(Random_forest, newdata=X)
    if(is.na(St_experts)){Experts <- RF_expert} else {Experts <- cbind(RF_expert, Experts)}
  }
  if (RNN){
    yy_train <- array(y1, c(1,length(y1), 1))
    XX_train <- array(X1, c(1, dim(X1)[1], dim(X1)[2]))
    rnn_model <- suppressMessages(trainr(Y=yy_train, X=XX_train, learningrate = 0.01))
    Rec_NN <- as.vector(predictr(rnn_model,array(X, c(1, dim(X)[1], dim(X)[2]))))
    Experts <- cbind(Experts, Rec_NN)
  }

  if (fake_experts > 0){
    A <- matrix(runif(fake_experts^2)*2-1, ncol=fake_experts) 
    Sigma <- t(A) %*% A
    false_experts <-  0.1*X[,1] + rmvnorm(N,rep(0,fake_experts),sigma=Sigma)
    Experts <- cbind(Experts, false_experts)
  } 
  if(weak_experts){
    weak1 <- apply(X, 1, mean)
    weak2 <- apply(X, 1, median)
    if(is.na(St_experts)){Experts <- cbind(weak1, weak2)} else{Experts <- cbind(St_experts,weak1, weak2)}
  }
  
 
  if(lasso){
    
    Lassocv <- cv.glmnet(x= as.matrix(X1),y=y1, 
                         lambda= seq(from=exp(-4),to= exp(4)), 
                         nfolds = 5)
    Lasso <- glmnet(x=as.matrix(X1), y=y1, lambda=Lassocv$lambda.min)
    Lasso_expert <- predict(Lasso, newx=as.matrix(X))
    
    if(is.na(St_experts)){
      Experts <- Lasso_expert
      St_experts <- Experts} else{
        Experts <- cbind(Lasso_expert, Experts)
        St_experts <- Experts}  
  }
  
  if(sc_control){
    
    ## SC control is the last expert
    mean_hat <- mean(y1)
    sc_w <- SC_weights(X=X1 - apply(X1, 2, mean), y=y1 - mean(y1), factorized = factorized)
    sc_pred <- apply(X - apply(X, 2, mean), 1, function(x) sc_w%*%x) + mean(y1) 
    Experts <- cbind(Experts, sc_pred)}  
  
  return(Experts)}


## Helper to simulate the process 

## model: markov-switching ARMA
##        ARMA(p,q) - specify your p and q
##        AR-ARCH 
## b: value of linear coefficients
## model_beta : Cos, Polynomial, logit or linear


sim_process <- function(model, N, p=NA, q=NA, X, b, model_beta, sd_Y=1){
  if (model == "markov-switching ARMA"){
    
    P <- matrix(c(0, 0.7, 0.5, 0.2, 0, 0, 0.8, 0.3, 0.5), nrow=3, ncol=3)
    matrix_coef <- matrix(c(1.5, 0.6, 1, 0.9,-1.2,1, 0, 0.7, 0), 3, 3)
    Sim_markovChain <- DTMC(tmat = P, io = c(0.25, 0.50, 0.25), 10*N, trace=F)
    coef_MC <- t(Sim_markovChain)%*%t(matrix_coef)
    ## remove burn in period
    coef_N <- coef_MC[(9*N):((10*N)-1),]
    y <- c(rnorm(1, 0,1), rep(0, N-2))
    epsilon_minus1 <- 0 
    for (i in 2:N){
      epsilon <- rnorm(1)
      y[i] <- c(y[i-1], epsilon_minus1,epsilon)%*%coef_N[i,]
      epsilon_minus1 <- epsilon
    }
  } else if (model=="ARMA"){
    y <- arima.sim(model=list(Ar=p, ma=q), N, innov=rnorm(N, sd=sd_Y))
  } else if (model=="ARMA_fat_tailed"){
    noise <- rt(N, df=3)
    y <- arima.sim(model=list(Ar=p, ma=q), N, innov = noise)
  } else if (model=="AR-ARCH"){
    alpha = 0.7  
    z <- rnorm(N)
    epsilon <-c(rnorm(1), rep(0, N-1))
    for(i in 2:N){
      h <- 0.001 + alpha*epsilon[i-1]**2
      epsilon[i] <- sqrt(h)*rnorm(1)
    }
    y <- arima.sim(model=list(Ar=0.8, ma=0), N, innov= epsilon)
  }
  if(model_beta =="Linear"){
    y = y + X%*%b} else if(model_beta =="Polynomial"){y = y + apply(poly(X, degree=2),1,sum) } else if(model_beta =="Cos"){
      y = cos(X%*%b + y)}else if(model=="logit"){
        y <- exp(X%*%b+y)/(1 + exp(X%*%b+y))
      }
  return(y)
}


## Simulate the process as a factor model(DGP1(2) or DGP5(1))
sim_process_factor_model <- function(N,p, b, rho, DGP="1", return_errors=F,  non_stationary= F){
  factor <- rnorm(N)
  if(non_stationary) factor = factor + cos(c(1:N))
  mu_j <- lambda_j <- c(1:p)/p
  theta <- rnorm(N)
  X <- matrix(0, nrow=N, ncol=p)
  for (i in 1:p){

     xi_j <- rnorm(N, mean = 0, sd = 1 - rho**2)
    epsilon_j <- arima.sim(n = N, model=list(ar = rho), innov = xi_j)
    X[,i] <- lambda_j[i]*factor + mu_j[i] + epsilon_j + theta
  }

  v_t <- rnorm(N, mean = 0, sd = 1- rho**2)
  u_t <- arima.sim(n = N, model=list(ar = rho), innov = xi_j)
  if (DGP=="1"){
    Y <- 0.5 + 0.5*factor + theta + u_t} else {
      Y <- X%*%b + u_t
    } 
  if(return_errors){
  return(list(y=Y, X=X, error=u_t))} else{
    return(list(y=Y, X=X, error=u_t))
    }
}
