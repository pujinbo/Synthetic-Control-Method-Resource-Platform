## Inputs
## Estimated residual 
## length of ptre-treatment period
## length of all periods (T)
## level of significance 
compute_quantile_permutation <- function(u_hat, T_0, N, lvl_sign = 0.05){ 
 ## Moving block permutation (built in R function)
 ## find all possible permutations (Inf is the second argument)
  permutation_set <- suppressMessages(shuffleSet(N, Inf, how(within = Within(type = "series"))))
  test_statistic <- apply(permutation_set, 1, function(x){ 
   uu <- u_hat[x]
   test <- sum((uu[T_0:N])**2)/sqrt(length(T_0:N))
   test
  })
  quantile_perm <- sort(test_statistic)[ceiling((1-lvl_sign)*length(test_statistic))]
  return(quantile_perm)
}


## Helper for bootstrap
## Arguments:
## Z: first element is y, other elements are either the predictions of the models trained on an HOLD OUT set(in case method is exponential of 
## LS forecaster) otherwise matrix with y as first variable and the columns being the usual variables of interest
## T_0: pre-treatment period
## Return the test statistic on a given bootstrapped sample


helper_4boot_simple <- function(Z, T_0){
  
  N <- dim(Z)[1]
  
  index_remove <- c(T_0:N)
  y <- Z[,1]
  if(dim(Z)[2] == 2){
  y_hat <- Z[,-1]
  losses <- (y_hat[index_remove] - y[index_remove])**2
  loss_t <- sum(losses)/sqrt(length(index_remove))
  return(loss_t)
  } else {
    y_hat <- Z[,-1]
    losses <- apply(Z[, -1], 2, function(z) sum((z[index_remove] - y[index_remove])**2))
    loss_t <- sapply(losses, function(x) x/sqrt(length(index_remove)))
    return(loss_t)
  }
}


## Helper for more than one learner 
helper_4boot_complex <- function(Z,T_0){
  
  N <- dim(Z)[1]
  
  index_remove <- c(T_0:N)
  y <- Z[,1]
  X <- Z[,-1]
  X <- as.matrix(X)
  colnames(X) <- c(1:dim(X)[2])
  p <- dim(X)[2]
  y1 <- y[-index_remove]
  X1 <- X[-index_remove,]
  eta = sqrt(2*log(p)/(N- 1))
  
  
  # Avoid cross validation not to go crazy
  weights <- Exp_algorithm(y1, X1, eta=eta)
  Y_expN <- apply(X[index_remove,], 1, function(x) weights%*%x)
  
  losses <- (Y_expN - y[index_remove])**2
  loss_t <- sum(losses)/sqrt(length(index_remove))
  return(loss_t)
}


#################################
## Block bootstrap : a single learner 
#################################

## T_train: size of the sample used for training the learner
## T_0: intervention period
## Y_pred: predicted Y 
## y: observed y
## size_boot: size of the block for block-bootstrap 
## one_learner: whether Y_pred only contains one learner 
## Here only one learner is used 
my_boot_quantile_simple <- function(T_train, T_0, Y_pred, y, size_boot, lvl_sign = 0.05, ncpus = 1, one_learner = T){
  if(one_learner){ 
  bootsr <- tsboot(cbind(y[-c(1:T_train)],Y_pred[-c(1:T_train)]), function(x) helper_4boot_simple(x, T_0=T_0 - T_train), R =100, sim="fixed", l=size_boot, 
                   ncpus = ncpus, parallel = 'no')
  quantile <- sort(bootsr$t[,1])[ceiling((1-lvl_sign)*length(bootsr$t[,1]))]
  } else {
    bootsr <- tsboot(cbind(y[-c(1:T_train)],Y_pred[-c(1:T_train), ]), function(x) helper_4boot_simple(x, T_0=T_0 - T_train), R =100, sim="fixed", l=size_boot, 
                     ncpus = ncpus, parallel = 'no')
    quantile = apply(bootsr$t, 2, function(x) sort(x)[ceiling((1-lvl_sign)*length(x))])
    
  }
  return(quantile)
} 

## Experts is a matrix of experts 
my_boot_quantile_complex  <- function(T_train, T_0, Experts, y, size_boot, lvl_sign = 0.05, ncpus = 1){
  bootsr <- tsboot(cbind(y[-c(1:T_train)],Experts[-c(1:T_train),]), function(x) 
    helper_4boot_complex(x, T_0=T_0 - T_train), R =100, sim="fixed", l=size_boot, 
                   ncpus = ncpus)
  quantile <- sort(bootsr$t[,1])[ceiling((1-lvl_sign)*length(bootsr$t[,1]))]
  return(quantile)
}


