##date 11/09/2025 visca Catalunya lliure. 
##Author: Jordi

rm(list=ls())
gc()
library(MASS)
#-----------------------------------------------------------------------------------
##1.-Define the variables first and the main functions i will use throughout. 
n<-1000
set.seed(1234)

x<-runif(n, min=-sqrt(3), max=sqrt(3))
epsilon<- runif(n, min=-0.5, max=0.5)
theta<-2
v<-rnorm(n)

ystar <- theta * x + epsilon
yl <- ystar + v * (v < 0)    
yu <- ystar + v * (v >= 0)   

theta_grid <- seq(1, 3, by = 0.01)

yc<-0.5*(yl+yu)
delta<-(yu-yl)/2

##This is to use in the subsampling to take random draws directly from here
data_full <- data.frame(
  x = x,
  yc = yc,
  delta = delta
)


#Moment function
m1_vec <- function(yc, delta, X, theta){
  return( yc * X - abs(X) * delta - theta * X^2 )
}

m2_vec <- function(yc, delta, X, theta){
  return( theta * X^2 - yc * X - abs(X) * delta )
}

#Calculate moment matrices from data
moment_matrix_1<-sapply(theta_grid,function(theta_sim) m1_vec(yc, delta, x, theta_sim))
moment_matrix_2<-sapply(theta_grid,function(theta_sim) m2_vec(yc, delta, x, theta_sim))

#Moments
mbar1 <-colMeans(moment_matrix_1)
mbar2 <-colMeans(moment_matrix_2)
s2_1 <- apply(moment_matrix_1, 2, var)
s2_2 <- apply(moment_matrix_2, 2, var)


#Estimate t-statistic
tn <- n*(pmax(mbar1/sqrt(s2_1), 0)^2 + pmax(mbar2/sqrt(s2_2), 0)^2)

#Note: super inefficient. As a shameless stataist, I am trying to depart from my vices but they always win in the short run.

#-----------------------------------------------------------------------------------

##Exercise 2: PA Method -->aproximate the distribution of the T to have the right critical values. 
B <- 1000 #ad hoc?
J <- 2 #two moments
Z <- mvrnorm(n = B, mu = rep(0, J), Sigma = diag(J))

s_matrix <- matrix(NA, nrow = B, ncol = length(theta_grid))

for (j in 1:length(theta_grid)) {
  moment_vectors <- cbind(moment_matrix_1[, j], moment_matrix_2[, j])
  sigma <- cov(moment_vectors)
  D_hat_inv_sqrt <- diag(diag(sigma)^(-0.5)) #####WRONG!!! correct asap
  omega <- D_hat_inv_sqrt %*% sigma %*% D_hat_inv_sqrt

  for (b in 1:B) {
    simulated_moments <- t(chol(omega)) %*% Z[b, ]  ##not sure about this, but the idea is to have omega*Z that is not independent.
    s_b <- sum(pmax(simulated_moments, 0)^2)
    s_matrix[b, j] <- s_b
  }
}
##Find critical value
pa_critical_value <- apply(s_matrix, 2, quantile, probs = 0.95, na.rm = TRUE)

##Define identified set
confidence_set_pa <- theta_grid[tn <= pa_critical_value]
bounds_set_pa <- c(min(confidence_set_pa), max(confidence_set_pa))

#-----------------------------------------------------------------------------------

##Exercise 3. Subsampling-->another way to determine the critical values.
#we define sample size and number of draws
b<- round(sqrt(n))
k<-1000


Tn_b_matrix <- matrix(NA, nrow = k, ncol = length(theta_grid))


for (i in 1:k) {
    subsample_indices <- sample(1:n, b, replace = FALSE)
    subsample_data <- data_full[subsample_indices, ]
        subsample_x <- subsample_data$x
        subsample_yc <- subsample_data$yc
        subsample_delta <- subsample_data$delta

    moment_matrix_1<-sapply(theta_grid,function(theta_sim) m1_vec(subsample_yc, subsample_delta, subsample_x, theta_sim))

    moment_matrix_2<-sapply(theta_grid,function(theta_sim) m2_vec(subsample_yc, subsample_delta, subsample_x, theta_sim))

    #estimate moments
    mbar1_b <-colMeans(moment_matrix_1)
    mbar2_b <-colMeans(moment_matrix_2)
    s2_1_b <- apply(moment_matrix_1, 2, var)
    s2_2_b <- apply(moment_matrix_2, 2, var)

    Tn_b_vec <- b * ((pmax(mbar1_b/sqrt(s2_1_b), 0)^2 + (pmax(mbar2_b/ sqrt(s2_2_b), 0 ))^2))
    
    Tn_b_matrix[i, ] <- Tn_b_vec
}

#find critical value
critical_value_subsampling <- apply(Tn_b_matrix, 2, quantile, probs = 0.95)

#define identified set
confidence_set_subsampling <- theta_grid[tn <= critical_value_subsampling]
bounds_set_subsampling <- c(min(confidence_set_subsampling), max(confidence_set_subsampling))


#-----------------------------------------------------------------------------------
##Exercise 4. MMS method

# Matrix to store the simulated test statistics for each theta agaaain, need to find a more efficient and smart way to code in R and detach from Stataism
skappa_matrix <- matrix(NA, nrow = B, ncol = length(theta_grid))

kappa <- sqrt(2 * log(log(n))) ##recommended but need to find a way as proposed in the paper mentioned; data driven way is always possible but don't understand shit from the table in the paper alluded.

for (j in 1:length(theta_grid)) {
  
  ##No need to repeat this, as computed above, but easier to follow...
  moment_vectors <- cbind(moment_matrix_1[, j], moment_matrix_2[, j])
  sigma <- cov(moment_vectors)
  D_hat_inv_sqrt <- diag(diag(sigma)^(-0.5))
  omega <- D_hat_inv_sqrt %*% sigma %*% D_hat_inv_sqrt

  xi_1<- sqrt(n)*(mbar1[j]/(sqrt(s2_1[j])*kappa))
  xi_2<- sqrt(n)*(mbar2[j]/(sqrt(s2_2[j])*kappa))

  varphi <- c(pmin(xi_1, 0), pmin(xi_2, 0)) ##many different varphi functions can be used, but I am lazy and decided to have only this one. Check Bontemps notes. 

  for (b in 1:B) {
    simulated_moments <- t(chol(omega)) %*% Z[b, ] + varphi
    s_b_kappa <- pmax(simulated_moments[1], 0)^2 +pmax(simulated_moments[2], 0)^2
    skappa_matrix[b, j] <- s_b_kappa
  }
}

# Find the critical value (95th percentile) for each theta
gms_critical_value <- apply(skappa_matrix, 2, quantile, probs = 0.95, na.rm = TRUE)

# Cconfidence set
confidence_set_gms <- theta_grid[tn <= gms_critical_value]
bounds_set_gms <- c(min(confidence_set_gms), max(confidence_set_gms))

#note: revise this...
#-----------------------------------------------------------------------------------

##Exercise 5. CHK method, pretty straight forward
alpha=0.05
p=2 ##number of ineq

cck=-qnorm(alpha/p)/sqrt(1-qnorm(alpha/p)^2/n)

tn_cck <- pmax(sqrt(n)*(mbar1/sqrt(s2_1)), sqrt(n)*(mbar2/sqrt(s2_2)))

confidence_set_cck <- theta_grid[tn_cck <= cck]
identified_set_cck <- c(min(confidence_set_cck), max(confidence_set_cck))

#-----------------------------------------------------------------------------------EOF 