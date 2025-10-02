##date 30/09/2025 
##Author: Jordi

rm(list=ls())
gc()
library(MASS)

#################

rm(list = ls())
gc()
library(MASS)

# DGP
n <- 500
P01<-0.35
P10<-0.15
P11<-0.5


#Define moments
define_moments <- function(alpha_1, alpha_2){
    m1<-0.35 + alpha_1
    m2<- 0.15 + alpha_2
    m3<- abs(0.5 - (1 + alpha_1)*(1 + alpha_2))

    return(c(m1, m2, m3))
}

#define alpha_grid

alpha_1_grid <- seq(-1, 0, by = 0.005)
alpha_2_grid <- seq(-1, 0, by = 0.005)


#Define t-statistic

#define the variance-covariance matrix

p_hat<- c(P01, P10, P11)
sigma_hat <- diag(p_hat) - p_hat%*% t(p_hat)

var_hat<- diag(sigma_hat)

#define t-statistic 

t_test <- function(alpha_1, alpha_2) {
    m <- define_moments(alpha_1, alpha_2)
    t1 <- sqrt(n) * m[1] / sqrt(var_hat[1])
    t2 <- sqrt(n) * m[2] / sqrt(var_hat[2])
    t3 <- sqrt(n) * m[3] / sqrt(var_hat[3])
    return(max(pmax(t1,0), pmax(t2,0), abs(t3)))
}

t_test(alpha_1 = -0.5, alpha_2 = -0.4)
##seems to be working

#we can do this by gms method:

B <- 1000 #ad hoc? 
J <- 3 #two moments
Z <- mvrnorm(n = B, mu = rep(0, J), Sigma=sigma_hat)

kappa <- sqrt(2*log(log(n)))

xi <- function(mom) {
    sd <- sqrt(diag(sigma_hat))
    xi_raw <- (sqrt(n) * mom) / (kappa * sd)
    xi_raw[1:2] <- pmin(xi_raw[1:2], 0) 
    return(xi_raw)  
}

get_critical <- function(alpha_1, alpha_2){
    moments <- define_moments(alpha_1, alpha_2)
    xi_value <- xi(moments)

    distrib <- apply(Z, 1, function(x) {
        ineq1 <- max(x[1] + xi_value[1], 0)
        ineq2 <- max(x[2] + xi_value[2], 0)
        eq    <- abs(x[3] + xi_value[3])
        max(ineq1, ineq2, eq)
    })

    return(quantile(distrib, 0.95))
}

grid <-expand.grid(alpha_1 = seq(-1,0,0.005), alpha_2 = seq(-1,0,0.005))

t_stat <- apply(grid, 1, \(x) t_test(alpha_1 = x[1], alpha_2 = x[2]))

critical = apply(grid, 1, \(x) get_critical(alpha_1 = x[1], alpha_2 = x[2]))

estimate <- grid[t_stat <= critical,]

alpha1_bounds_gms <- range(estimate$alpha_1)
alpha2_bounds_gms <- range(estimate$alpha_2)




########################Cox and Shi

define_moments <- function(alpha_1, alpha_2){
  m1 <- 0.35 + alpha_1           
  m2 <- 0.15 + alpha_2            
  m3 <- 0.5 - (1 + alpha_1)*(1 + alpha_2)   
  c(m1, m2, m3)
} ##repeat but for clarity's sake...

project_mu_numeric <- function(m, S){ #revise. -->use solve.QP (w constraints as specified in the problem. Clearly this was not working...)
  # Objective Q(mu1, mu2, mu3=0)
  Q_obj <- function(mu12){
    mu <- c(mu12[1], mu12[2], 0)  # enforce μ3 = 0
    as.numeric(t(m - mu) %*% S %*% (m - mu))
  }
  
  inits <- list(
    c(min(m[1],0), min(m[2],0)),     
    c(0, 0),                        
    c(m[1], 0),                    
    c(0, m[2])
  )
  solutions <- lapply(inits, function(init) {
    sol <- optim(
      par = init,
      fn = Q_obj,
      method = "L-BFGS-B",
      lower = c(-Inf, -Inf),
      upper = c(0, 0)
    )
    list(mu = c(sol$par, 0), Q = sol$value)
  })

  mu_hat <- solutions[[which.min(sapply(solutions, \(z) z$Q))]]$mu

  return(mu_hat) ##revise...
}

p_hat   <- c(P01, P10, P11)
Sigma   <- diag(p_hat) - p_hat %*% t(p_hat)
S_inv <- MASS::ginv(Sigma) ##??? Penrose Inverse

Tn_cs <- function(alpha_1, alpha_2){
  m  <- define_moments(alpha_1, alpha_2)
  mu <- project_mu_numeric(m, S_inv)                     
  as.numeric(n * t(m - mu) %*% S_inv %*% (m - mu))
}


active_rank <- function(mu, tol = 1e-10){
  r <- 1L  # equality contributes rank 1
  if (abs(mu[1]) < tol) r <- r + 1L #thn this adds more
  if (abs(mu[2]) < tol) r <- r + 1L 
  
  ##This is also wrong, because the equality gives two moment ineq, so it should be 2 + (1) ??????
  r
}

crit_cs <- function(r, alpha = 0.05){
  qchisq(p = 1 - alpha, df = r) #this makes sense...
}

eval_point <- function(alpha_1, alpha_2){
  m  <- define_moments(alpha_1, alpha_2)
  mu <- project_mu_numeric(m, S_inv)
  Tn <- as.numeric(n * t(m - mu) %*% S_inv %*% (m - mu))
  r  <- active_rank(mu)
  list(Tn = Tn, r = r) 
}


decide_keep <- function(alpha_1, alpha_2, alpha = 0.05){
  out <- eval_point(alpha_1, alpha_2)
  cv  <- crit_cs(out$r, alpha)
  out$Tn <= cv
}

grid <- expand.grid(alpha_1 = seq(-1, 0, 0.005),
                    alpha_2 = seq(-1, 0, 0.005))

keep <- apply(grid, 1, \(x) decide_keep(x[1], x[2]))
cs95 <- grid[keep, , drop = FALSE]

alpha1_bounds_cs <- range(cs95$alpha_1)
alpha2_bounds_cs <- range(cs95$alpha_2)


##results don't make much sense though...


###############################EOF 