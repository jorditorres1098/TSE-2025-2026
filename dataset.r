
###1.1.This is just to simulate the data and see if seeting a seed
rm(list=ls())
gc()
library(MASS)



##Define the variables first.
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
theta_grid_fine <- seq(1, 3, by = 0.005)


##exercise 2
yc<-0.5*(yl+yu)
delta<-(yu-yl)/2

critical_value_qui<-qchisq(0.95, df=2)

# Combine the relevant data in a dataframe
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

moment_matrix_1<-sapply(theta_grid,function(theta_sim) m1_vec(yc, delta, x, theta_sim))

moment_matrix_2<-sapply(theta_grid,function(theta_sim) m2_vec(yc, delta, x, theta_sim))

#estimate moments
mbar1 <-colMeans(moment_matrix_1)
mbar2 <-colMeans(moment_matrix_2)
s2_1 <- apply(moment_matrix_1, 2, var)
s2_2 <- apply(moment_matrix_2, 2, var)

tn <- n*((pmax(mbar1, 0)^2)/s2_1 + (pmax(mbar2, 0)^2)/s2_2)
# Find the confidence set using logical indexing
confidence_set_1 <- theta_grid[tn <= critical_value_qui]

# Define the final identified set
identified_set_1 <- c(min(confidence_set_1), max(confidence_set_1))


###Subsampling
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

    Tn_b_vec <- b * ( (pmax(mbar1_b, 0)^2) / s2_1_b + (pmax(mbar2_b, 0)^2) / s2_2_b )
    
    Tn_b_matrix[i, ] <- Tn_b_vec
}

##find critical value
critical_value_subsampling <- apply(Tn_b_matrix, 2, quantile, probs = 0.95)

confidence_set_subsampling <- theta_grid[tn <= critical_value_subsampling]
identified_set_subsampling <- c(min(confidence_set_subsampling), max(confidence_set_subsampling))