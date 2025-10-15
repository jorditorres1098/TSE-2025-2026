##Homework 2 of Policy analysis: RD
##Date: 15/10/2025


#0. Introduction, data manipulation. 
library(haven)
library(here)
library(dplyr)
library(rdlocrand)

relative_path <- "/Users/jorditorresvallverdu/Library/Mobile Documents/com~apple~CloudDocs/tse/year2/term1/bobba/data"

main <- read_dta(file.path(relative_path, "RD_sample.dta"))

df <- as.data.frame(haven::as_factor(main))
#View(df[1:100, ])


#pre data-cleaning
main <- main |> 
    mutate(time = as.numeric(time)) |>
    filter(time>=30) |>
    mutate(
    time= time-120,
    population= population-500) 

#Note: there are some extreme values for some running variables should we consider that in the cleaning process???


#1.1 Window selection.

#define vector of running variables
R_list <- list(
  population = main$population,
  time = main$time
)

Y_list <- list(
  wage = main$wage,
  score = main$score
)
#define covariates to test balance
X <- as.matrix(main[ , c( "gender", "univ", "exp_publica_c", "exp_privada_c")])





#use program to compute the windows, but this is inefficient, cause this is an input of the previous command too. 
window <- lapply(R_list, function(R){
    rdwinselect(
  R = R,
  X = X,
  cutoff = 0,       
  level = 0.15,
  statistic = "ksmirnov", 
  reps = 1000,
  seed = 123,
  wmasspoints = TRUE
) }
)
##Basically the issue is that we reject the hypothesis of balance very early on, so the bandwidth is what it is...


#1.2.
ranunif <- function(Y,R, type="ksmirnov"){
        rdrandinf(
    Y=Y,
    R=R,
    cutoff = 0,       
    level = 0.15,
    statistic = type, 
    reps = 1000,
    seed = 123,
    wmasspoints = TRUE
    )
}

grid <- expand.grid(R=names(R_list), Y=names(Y_list))

output <- apply(grid, 1, function(row){
    Rname <- row["R"]
    Yname <- row["Y"]

    cat("iteration" ,Rname, Yname)

    ranunif(Y=Y_list[[Yname]],
    R=R_list[[Rname]], 
    type="ksmirnov")

})







##this must be done more efficiently, I am repeating all the time the sme function. Maybe I can define a function









##1.1.1. Using program

