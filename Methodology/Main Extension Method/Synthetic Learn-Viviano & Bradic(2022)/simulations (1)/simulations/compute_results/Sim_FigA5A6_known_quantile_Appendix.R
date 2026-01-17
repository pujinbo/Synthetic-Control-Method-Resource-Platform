#######################################################################3
#### Common library
#######################################################################
working_dir <- '.'
source(paste0(working_dir,'/libraries/library.R'))

####################################################
########### Library for oracle 1
####################################################


## p dimension of X
## N total number of periods
## T_0 total pre treatment period
## M keep Inf, maximum loss
## T_train: period for training the experts during pre-treatment
## b : value of the linear coefficient beta
## alpha: effect of a policy
## RNN: include RNN in experts
## lasso: include lasso in experts
## randforest: include randforest in experts
## fake_experts: number of non informative experts
## strong_experts: include SVM, ARMA(1,1), XGboost
## crazy experts: deprecated
## model: either ARMA, AR-ARCH, markov-switching ARMA (see Common_library_sim)
## AR: value of the AR coefficient
## MA: value of the MA coefficient
## lvl_sign: level of significance of the test
## discounting: discount the loss function(deprecated
## weak experts: deprecated
## fat_tailed_x: use a t-student distribution for X
## model_beta : Cos, Polynomial, logit or linear
## lag: include lagged Y in the covariate matrix
## is_X_AR: let X be an AR process
## sd_Y: variance of noise of y
## seed: deprecated
## sigma : 1 low correlation among X, !=1 high correlation among X

### Return estimated loss and true loss and the estimated weights

Simulation_oracle1 <- function(p, N,T_0, T_train=T_0/2,
                               b=c(1/((2:(p))^2), 1 - sum(1/((2:(p))^2))), alpha=10, 
                               RNN=F,lasso=F, randforest=F,fake_experts=10, strong_experts=F,
                               model="ARMA", AR=0.5, MA=0.3, 
                               weak_experts=F, fat_tailed_x=F,
                               model_beta ="Linear",
                               lag=F, is_X_AR=T, sd_Y=1,  sigma=1){
  
  ## sigma != 1 -> high correlation
  if(sigma==1){
  sigma=matrix(NA,p,p)
  for(k in 1:p){for(j in 1:p){sigma[k,j]=0.5^abs(j-k)}}
  } else{
  Posdef <- function (n, ev = runif(n, 0, 10)) 
  {
    Z <- matrix(ncol=n, rnorm(n^2))
    decomp <- qr(Z)
    Q <- qr.Q(decomp) 
    R <- qr.R(decomp)
    d <- diag(R)
    ph <- d / abs(d)
    O <- Q %*% diag(ph)
    Z <- t(O) %*% diag(ev) %*% O
    return(Z)
  }
  
  sigma <- Posdef(n=p, ev=(c(1:p))**4)
  }
  if(model!="Factor_model" & model !="Linear_FM"){
    ## Generate random noise
    X=rmvnorm(N,rep(0,p),sigma=diag(1, nrow=p))
    X = X%*%chol(sigma)
    ## generate X as AR(1)
    innov <- rnorm(N, sd=1 - 0.8^2)
    AR <- sapply(rep(N, p), function(x) arima.sim(model=list(Ar=0.8), x, innov=innov))
    if(is_X_AR){X <- AR + X}
    ## Add fat tailed t student noise
    if(fat_tailed_x){X <- X + rt(N, 4)}
    y <- sim_process(model=model, N=N, p=AR, q=MA, X=X, b=b, model_beta=model_beta, sd_Y=sd_Y)
  } else if (model %in% c("Factor_model", "Linear_FM")){
    DGP <- ifelse(model=="Linear_FM",2,1)
    data <- sim_process_factor_model(N=N,p=p, b=b, rho=0.6, DGP=DGP)
    X <- data[[2]]
    y <- data[[1]]
  }
  y[T_0:N] = y[T_0:N] + alpha
  
  y1 <- y[-c(T_0:N)]
  X1 <- X[-c(T_0:N),]
  ## add lag effect 
  if(lag){XX <- cbind(X, c(0,y[-length(y)]))} else{XX <- X}
  
  ## Generate experts without fake experts
  new_experts <- suppressWarnings(generate_experts(XX, y, N, T_0=T_train, strong_experts = strong_experts, weak_experts = weak_experts, 
                                                   lasso=T, randforest=randforest, 
                                                   fake_experts = 0, sc_control=T, RNN=RNN))
  ## Generate only fake experts
  new_expertsB <- generate_experts(XX, y, N, T_0=T_0, strong_experts = F, weak_experts = F, 
                                   lasso=F, randforest=F, fake_experts = fake_experts)
  XX <- new_expertsB
  
  ## Code to adjust for fake_expers == 0
  ind <- dim(new_expertsB)[2] +1  - fake_experts
  ## Construct union of experts and fake experts
  Experts_withfake <- cbind(new_experts[,-c(dim(new_experts)[2]-1,dim(new_experts)[2]) ], new_expertsB[,c(ind:dim(new_expertsB)[2])])
  Experts_withfake <- Experts_withfake[-c(1:T_train),]
  ## Experts without fake experts: remove also the synthetic control
  Experts <- new_experts[-c(1:T_train),-c(dim(new_experts)[2]-1,dim(new_experts)[2])]
  Experts_withSC <- new_experts[-c(1:T_train),]
  ## Remov observations used for training experts
  y_train <- y[-c(1:T_train)]
  ## define eta
  E = dim(Experts)[2]
  eta = sqrt(2*log(E)/(N- T_train - 1))
  
  ## Method: exponential forecaster
  
  weights_expforecaster <- Exp_algorithm(y_train[-c((T_0-T_train):(N-T_train))], Experts[-c((T_0-T_train):(N-T_train)),], eta=eta)
  Y_expN <- apply(Experts[c((T_0-T_train):(N-T_train)),], 1, function(x) weights_expforecaster%*%x)
  
  ## Method: synthetic control
  
  mean_hat <- mean(y1)
  weights_SC <- SC_weights(y=y1 - mean_hat, X=apply(X[-c(T_0:N),], 2, function(x) x  -mean(x)))
  Y_SC <- apply(X, 1, function(x) weights_SC%*%x) + mean_hat - weights_SC%*%apply(X, 2, mean)
  
  
 

  ## Method: Exponential forecaster with fake experts
  
  
  weights_expforecaster_withfake <- Exp_algorithm(y_train[-c((T_0-T_train):(N-T_train))], 
                                                  Experts_withfake[-c((T_0-T_train):(N-T_train)),], 
                                               eta=eta)
  Y_exp_withfake <- apply(Experts_withfake[c((T_0-T_train):(N-T_train)),], 1, 
                          function(x) weights_expforecaster_withfake%*%x)
  
  ## Method: exponential forecaster with SC
  
  
  weights_expforecaster_withSC <- Exp_algorithm(y_train[-c((T_0-T_train):(N-T_train))], Experts_withSC[-c((T_0-T_train):(N-T_train)),], eta=eta)
  Y_exp_withSC <- apply(Experts_withSC[c((T_0-T_train):(N-T_train)),], 1, function(x) weights_expforecaster_withSC%*%x)
  
  
  ## Method: DiD
  
  general_mean <- mean(cbind(y,X))
  mean_y <-mean(y - general_mean)
  alpha1 <- lm(apply(cbind(y - general_mean - mean_y,X - general_mean), 1, mean) ~ -1 + c(rep(0, N - T_0), rep(1, T_0)))$coef
  DID_coef <- mean(y[T_0:N]) - mean(y[1:(T_0 - 1)]) - (mean(X[T_0:N]) - mean(X[1:(T_0 - 1)]))
  Y_did <- general_mean + mean_y + alpha1*c(rep(0, T_0),rep(1, N - T_0)) + DID_coef*c(rep(0, T_0),rep(1, N - T_0))
    
  ## Predict 
  
  ## Loss for standard exponential forecaster
  loss_exp <- (Y_expN - y[c(T_0:N)])**2 
  loss_expt <- sum(loss_exp)/(sqrt(length(T_0:N)))
  loss_exptrue <- sum((Y_expN - y[c(T_0:N)] + alpha)**2)/(sqrt(length(T_0:N)))
  
  ## Control for ill-conditioned simulations
  if(is.na(weights_SC) == F){
  ## Loss for Synthetic control
  loss_sc <- (Y_SC[c(T_0:N)] - y[c(T_0:N)])**2
  loss_sct <- sum(loss_sc)/(sqrt(length(T_0:N)))
  loss_sctrue <- sum((Y_SC[c(T_0:N)] - y[c(T_0:N)] + alpha)**2)/(sqrt(length(T_0:N)))} else{
    loss_sct <- NA
    loss_sctrue <- NA
  }
  

  ## Loss for exponential forecaster with fake experts
  loss_exp_withfake <- (Y_exp_withfake- y[c(T_0:N)])**2
  loss_exp_withfaket <- sum(loss_exp_withfake)/(sqrt(length(T_0:N)))
  loss_exp_withfaketrue <- sum((Y_exp_withfake - y[c(T_0:N)] + alpha)**2)/(sqrt(length(T_0:N)))
  
  
  
  ## Loss exp with SC
  loss_exp_withSC <- (Y_exp_withSC - y[c(T_0:N)])**2
  loss_exp_withSCt <- sum(loss_exp_withSC)/(sqrt(length(T_0:N)))
  loss_exp_withSCtrue <- sum((Y_exp_withSC - y[c(T_0:N)] + alpha)**2)/(sqrt(length(T_0:N)))
  

  
  
  ## Loss =DiD
  ## On Did we reverted the loss with respect to potential outcome without policy
  loss_did <- (Y_did[c(T_0:N)] - y[c(T_0:N)] + alpha)**2
  loss_didt <- sum(loss_did)/(sqrt(length(T_0:N)))
  loss_didtrue <- sum((Y_did[c(T_0:N)] - y[c(T_0:N)])**2)/(sqrt(length(T_0:N)))
  
  
  
  loss_t <- c(loss_expt, loss_sct,  loss_exp_withfaket, loss_exp_withSCt, loss_didt)
  loss_true <- c(loss_exptrue, loss_sctrue,  loss_exp_withfaketrue, loss_exp_withSCtrue, loss_didtrue)
  return(list(loss_t, loss_true))
}


### Return the percentage of rejection using a given model


## p dimension of X
## N total number of periods
## T_0 total pre treatment period
## M keep Inf, maximum loss
## T_train: period for training the experts during pre-treatment
## b : value of the linear coefficient beta
## alpha: effect of a policy
## RNN: include RNN in experts
## lasso: include lasso in experts
## randforest: include randforest in experts
## fake_experts: number of non informative experts
## strong_experts: include SVM, ARMA(1,1), XGboost
## crazy experts: deprecated
## model: either ARMA, AR-ARCH, markov-switching ARMA (see Common_library_sim)
## AR: value of the AR coefficient
## MA: value of the MA coefficient
## lvl_sign: size of the test
## discounting: discount the loss function(deprecated
## weak experts: deprecated
## fat_tailed_x: use a t-student distribution for X
## model_beta : Cos, Polynomial, logit or linear
## lag: include lagged Y in the covariate matrix
## is_X_AR: let X be an AR process
## sd_Y: variance of noise of y
## seed: deprecated
## sigma : 1 low correlation among X, !=1 high correlation among X

## Return percentage of rejecions for oracle
simulation_oracle1_final <- function(AR=0.5,p=10, MA=0.3, N_sim = 100, N=100,lvl_sign=0.05, T_0=90, 
                                     T_train=T_0/2, alpha=seq(from = -3, to=3, by=0.5), model="ARMA", model_beta="Linear",
                                     fat_tailed_x =F,sd_Y=1,
                                     RNN=F, lasso=F, randforest=F,fake_experts=10, strong_experts=T,
                                     weak_experts =F,lag=F, is_X_AR=T, verbose=T, sigma=1){
  
  
  k <- 1
  final_result <- matrix(0, ncol=15, nrow=length(alpha))
  for (a in alpha){
    
    ## Simulate for bootstrap
    result_did <- result_exp <- result_SC <-  result_exp_withfake <- result_exp_withSC <- matrix(NA, nrow=N_sim, ncol=2)
    for(i in 1:N_sim){
      res <- do.call("Simulation_oracle1", list(p=p, N=N, T_0 = T_0, T_train=T_train,
                                                alpha=a, AR=AR, MA=MA, model=model, model_beta=model_beta,
                                                fat_tailed_x=fat_tailed_x, sd_Y=sd_Y,
                                                lasso=lasso, randforest=randforest, RNN=RNN,
                                                fake_experts=fake_experts, weak_experts =weak_experts,
                                                strong_experts=strong_experts,
                                                lag=lag, is_X_AR=is_X_AR, sigma=sigma))
      result_exp[i,] <- c(res[[1]][1], res[[2]][1])
      result_SC[i,] <- c(res[[1]][2], res[[2]][2])
       result_exp_withfake[i,] <- c(res[[1]][3], res[[2]][3])
      result_exp_withSC[i,] <- c(res[[1]][4],  res[[2]][4])
      result_did[i,] <- c(res[[1]][5],  res[[2]][5])
    
    }
    if(k ==1){ 
      list_result_exp <- result_exp
      list_result_exp_withSC <- result_exp_withSC 
      list_result_SC <- result_SC
      list_result_exp_withfake <- result_exp_withfake
      list_result_did <- result_did
    } else{ 
        list_result_exp <- cbind(list_result_exp,result_exp)
        list_result_SC <- cbind(list_result_SC,result_SC)
        list_result_exp_withfake <- cbind(list_result_exp_withfake, result_exp_withfake)
        list_result_did <- cbind(list_result_did, result_did)
        list_result_exp_withSC <- cbind(list_result_exp_withSC, result_exp_withSC)}
    
    quantile_exp <- sort(result_exp[,2])[N_sim*(1-lvl_sign)]
    counts_optimalrej_exp <- apply(result_exp, 1, function(x) ifelse(x[1] > quantile_exp, 1, 0))
    res_exp <- sum(counts_optimalrej_exp)/N_sim
    loss_mean_exp <- mean(result_exp[,2])
    loss_var_exp <- var(result_exp[,2])
    
    result_SC <- na.omit(result_SC)
    quantile_SC <- sort(result_SC[,2])[N_sim*(1-lvl_sign)]
    counts_optimalrej_SC <- apply(result_SC, 1, function(x) ifelse(x[1] > quantile_SC, 1, 0))
    res_SC <- sum(counts_optimalrej_SC)/N_sim
    loss_mean_SC <- mean(result_SC[,2])
    loss_var_SC <- var(result_SC[,2])
    
    quantile_exp_withfake <- sort(result_exp_withfake[,2])[N_sim*(1-lvl_sign)]
    counts_optimalrej_exp_withfake <- apply(result_exp_withfake, 1, function(x) ifelse(x[1] > quantile_exp_withfake, 1, 0))
    res_exp_withfake <- sum(counts_optimalrej_exp_withfake)/N_sim
    loss_mean_exp_withfake <- mean(result_exp_withfake[,2])
    loss_var_exp_withfake <- var(result_exp_withfake[,2])
    
    quantile_exp_withSC <- sort(result_exp_withSC[,2])[N_sim*(1-lvl_sign)]
    counts_optimalrej_exp_withSC <- apply(result_exp_withSC, 1, function(x) ifelse(x[1] > quantile_exp_withSC, 1, 0))
    res_exp_withSC <- sum(counts_optimalrej_exp_withSC)/N_sim
    loss_mean_exp_withSC <- mean(result_exp_withSC[,2])
    loss_var_exp_withSC <- var(result_exp_withSC[,2])
    
    quantile_did <- sort(result_did[,2])[N_sim*(1-lvl_sign)]
    counts_optimalrej_did <- apply(result_did, 1, function(x) ifelse(x[1] > quantile_did, 1, 0))
    res_did <- sum(counts_optimalrej_did)/N_sim
    loss_mean_did <- mean(result_did[,2])
    loss_var_did <- var(result_did[,2])
 
    
    ## First 6 elements:
    ## results for exponential forecaster, SC, exponential forecaster with fake experts, exp with SC, did, lasso
    final_result[k,] <- c(res_exp,res_SC,res_exp_withfake, res_exp_withSC, res_did, loss_mean_exp, loss_mean_SC, loss_mean_exp_withfake, loss_mean_exp_withSC, 
                          loss_mean_did, 
                          loss_var_exp, loss_var_SC, loss_var_exp_withfake, loss_var_exp_withSC, loss_var_did) 
    k <- k + 1
    
  }
  ## The first element in the list is the object of interest                                     
  return(list(final_result,list_result_exp,list_result_SC, list_result_exp_withfake, list_result_exp_withSC))}





## p = 10 

## ARMA components

simulation1 <- simulation_oracle1_final(p=10, model_beta="Polynomial", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), N_sim=500,sd_Y=0.1, fake_experts = 50)
save.image(file=paste0(working_dir, '/saved_results/oracle.RData'))
simulation2 <- simulation_oracle1_final(p=10, model_beta="Polynomial", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), N_sim=500,sd_Y=1, fake_experts = 50)
save.image(file=paste0(working_dir, '/saved_results/oracle.RData'))
simulation3 <- simulation_oracle1_final(p=10, model_beta="Cos", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), sd_Y=0.1, fake_experts = 50, N_sim=500)
save.image(file=paste0(working_dir, '/saved_results/oracle.RData'))
simulation4 <- simulation_oracle1_final(p=10, model_beta="Cos", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), sd_Y=1, fake_experts = 50, N_sim=500)
save.image(file=paste0(working_dir, '/saved_results/oracle.RData'))

## ARMA components: p = 50 

simulation5 <- simulation_oracle1_final(p=50, model="Linear_FM", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), N_sim=500, fake_experts = 50)
save.image(file=paste0(working_dir, '/saved_results/oracle.RData'))
simulation6 <- simulation_oracle1_final(p=50, model_beta="logit", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), sd_Y=0.1, N_sim=500, fake_experts = 50)
save.image(file=paste0(working_dir, '/saved_results/oracle.RData'))
print('first set done')
simulation7 <- simulation_oracle1_final(p=50, model_beta="logit", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), sd_Y=1, N_sim=500, fake_experts = 50)
save.image(file=paste0(working_dir, '/saved_results/oracle.RData'))
simulation8 <- simulation_oracle1_final(p=50, model_beta="Cos", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), sd_Y=0.1, fake_experts = 50, N_sim=500)
save.image(file=paste0(working_dir, '/saved_results/oracle.RData'))
simulation9 <- simulation_oracle1_final(p=50, model_beta="Cos", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), sd_Y=1, fake_experts = 50, N_sim=500)
save.image(file=paste0(working_dir, '/saved_results/oracle.RData'))
simulation10 <- simulation_oracle1_final(p=50, model="Factor_model", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), N_sim=500, fake_experts = 50)
save.image(file=paste0(working_dir, '/saved_results/oracle.RData'))
print('first set done')


## AR-ARCH components 

## Low correlation
simulation11 <- simulation_oracle1_final(p=50, model_beta="logit", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), model='AR-ARCH', N_sim=500, fake_experts=50)
save.image(file=paste0(working_dir, '/saved_results/oracle.RData'))
simulation12 <- simulation_oracle1_final(p=50, model_beta="Cos", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), model='AR-ARCH', fake_experts=50, N_sim=500)
save.image(file=paste0(working_dir, '/saved_results/oracle.RData'))
print('first set done')


