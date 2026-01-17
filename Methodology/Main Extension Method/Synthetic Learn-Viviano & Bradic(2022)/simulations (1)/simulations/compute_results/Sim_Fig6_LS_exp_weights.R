#######################################################################3
#### Common library
#######################################################################

working_dir <- '.'
source(paste0(working_dir,'/libraries/library.R'))
Simulation_oracle1 <- function(p, N,T_0,T_train=T_0/2,
                               b=c(1/((2:(p))^2), 1 - sum(1/((2:(p))^2))), alpha=10, 
                               RNN=F,lasso=F, randforest=F,fake_experts=10, strong_experts=F, 
                               model="ARMA", AR=0.5, MA=0.3, 
                               weak_experts=F, fat_tailed_x=F,
                               model_beta ="Linear",
                               lag=F, is_X_AR=T, sd_Y=1, sigma=1){
  
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
  ## Experts without fake experts
  Experts <- new_experts[-c(1:T_train),-c(dim(new_experts)[2]-1,dim(new_experts)[2])]
  Experts_withSC <- new_experts[-c(1:T_train),]
  ## Remov observations used for training experts
  y_train <- y[-c(1:T_train)]
  ## define eta
  E = dim(Experts)[2]
  eta = sqrt(2*log(E)/(N- T_train - 1))
  
  
  ## Method: Exponential forecaster with fake experts
  
  
  weights_expforecaster_withfake <- Exp_algorithm(y_train[-c((T_0-T_train):(N-T_train))], 
                                                  Experts_withfake[-c((T_0-T_train):(N-T_train)),], 
                                                  eta=eta)
  Y_expN <- apply(Experts_withfake[c((T_0-T_train):(N-T_train)),], 1, 
                  function(x) weights_expforecaster_withfake%*%x)
  
  ## Method: LS with fake experts
  
  reg1 <- lm(y_train[-c((T_0-T_train):(N-T_train))]~-1+Experts_withfake[-c((T_0-T_train):(N-T_train)),])
  Y_LS <- Experts_withfake[c((T_0-T_train):(N-T_train)),]%*%coef(reg1)
  
  ## Predict 
  
  ## Loss for standard exponential forecaster
  loss_exp <- (Y_expN - y[c(T_0:N)])**2
  loss_expt <- sum(loss_exp)/(sqrt(length(T_0:N)))
  loss_exptrue <- sum((Y_expN - y[c(T_0:N)] + alpha)**2)/(sqrt(length(T_0:N)))
  
  
  ## Loss exp with SC
  loss_LS <- (Y_LS - y[c(T_0:N)])**2
  loss_LSt <- sum(loss_LS)/(sqrt(length(T_0:N)))
  loss_LStrue <- sum((Y_LS - y[c(T_0:N)] + alpha)**2)/(sqrt(length(T_0:N)))
  
  loss_t <- c(loss_expt, loss_LSt)
  loss_true <- c(loss_exptrue, loss_LStrue)
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

## Return percentage of rejecions for oracle
simulation_oracle_expVSLS <- function(AR=0.5,p=10, MA=0.3, N_sim = 100, N=100,lvl_sign=0.05, T_0=90, 
                                      T_train=T_0/2, alpha=seq(from = -3, to=3, by=0.5), model="ARMA", model_beta="Linear",
                                      fat_tailed_x =F,sd_Y=1,
                                      RNN=F, lasso=F, randforest=F,fake_experts=10, strong_experts=T,
                                      weak_experts =F,lag=F, is_X_AR=T, verbose=T, sigma=1){
  
  
  k <- 1
  final_result <- matrix(0, ncol=2, nrow=length(alpha))
  for (a in alpha){
    
    ## Simulate for bootstrap
    result_LS <- result_exp <- matrix(NA, nrow=N_sim, ncol=2)
    for(i in 1:N_sim){
      res <- do.call("Simulation_oracle1", list( p=p, N=N, T_0 = T_0, T_train=T_train,
                                                 alpha=a, AR=AR, MA=MA, model=model, model_beta=model_beta,
                                                 fat_tailed_x=fat_tailed_x, sd_Y=sd_Y,
                                                 lasso=lasso, randforest=randforest, RNN=RNN,
                                                 fake_experts=fake_experts, weak_experts =weak_experts,
                                                 strong_experts=strong_experts,
                                                 lag=lag, is_X_AR=is_X_AR, sigma=sigma))
      result_exp[i,] <- c(res[[1]][1], res[[2]][1])
      result_LS[i,] <- c(res[[1]][2], res[[2]][2])
      
    }
    
    
    quantile_exp <- sort(result_exp[,2])[N_sim*(1-lvl_sign)]
    counts_optimalrej_exp <- apply(result_exp, 1, function(x) ifelse(x[1] > quantile_exp, 1, 0))
    res_exp <- sum(counts_optimalrej_exp)/N_sim
    loss_mean_exp <- mean(result_exp[,2])
    loss_var_exp <- var(result_exp[,2])
    
    quantile_LS <- sort(result_LS[,2])[N_sim*(1-lvl_sign)]
    counts_optimalrej_LS <- apply(result_LS, 1, function(x) ifelse(x[1] > quantile_LS, 1, 0))
    res_LS <- sum(counts_optimalrej_LS)/N_sim
    loss_mean_LS <- mean(result_LS[,2])
    loss_var_LS <- var(result_LS[,2])
    
    ## First 6 elements:
    ## results for exponential forecaster, SC, exponential forecaster with fake experts, exp with SC, did, lasso
    final_result[k,] <- c(res_exp,res_LS) 
    k <- k + 1
    
  }
  ## The first element in the list is the object of interest                                     
  return(final_result)}


## Simulate with 100 fake experts
simulation1 <- simulation_oracle_expVSLS(p=50, model="Linear_FM", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), N_sim=500, fake_experts = 100)
### Simulate with 10 fake experts
simulation1B <- simulation_oracle_expVSLS(p=50, model="Linear_FM", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), N_sim=500, fake_experts = 10)

simulation2 <- simulation_oracle_expVSLS(p=50, model_beta="logit", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), sd_Y=0.1, N_sim=500, fake_experts = 100)
### Simulate with 10 fake experts
simulation2B <- simulation_oracle_expVSLS(p=50, model_beta="logit", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), sd_Y=0.1, N_sim=500, fake_experts = 10)

simulation3 <- simulation_oracle_expVSLS(p=50,  model_beta="Cos", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), sd_Y=0.1, fake_experts = 100, N_sim=500)
### Simulate with 10 fake experts
simulation3B <- simulation_oracle_expVSLS(p=50, model_beta="Cos", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), sd_Y=0.1, fake_experts = 10, N_sim=500)


simulation4 <- simulation_oracle_expVSLS(p=50,  model_beta="Cos", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), sd_Y=1, fake_experts = 100, N_sim=500)
### Simulate with 10 fake experts
simulation4B <- simulation_oracle_expVSLS(p=50, model_beta="Cos", N=300, T_0=280, alpha=c(0,0.1, 0.2,0.3,0.5,0.7, 1,1.5), sd_Y=1, fake_experts = 10, N_sim=500)



save(list = c('simulation1', 'simulation1B', 'simulation2', 'simulation2B', 'simulation3', 'simulation3B', 'simulation4', 
              'simulation4B'), file = paste0(working_dir, '/saved_results/Sim_expVSLS.RData'))


