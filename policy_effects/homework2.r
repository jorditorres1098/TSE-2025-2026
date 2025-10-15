##Homework 2 of Policy analysis: RD
##Date: 15/10/2025


#0. Introduction, data manipulation. 
library(haven)
library(here)
library(dplyr)

relative_path <- "/Users/jorditorresvallverdu/Library/Mobile Documents/com~apple~CloudDocs/tse/year2/term1/bobba/data"

main <- read_dta(file.path(relative_path, "RD_sample.dta"))

df <- as.data.frame(haven::as_factor(main))
View(df[1:100, ])


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

#define covariates to test balance
X <- array()



##1.1.1. Using program

