##Homework 2 of Policy analysis: RD
##Date: 15/10/2025
##Author: Jordi Torres 


#0. Introduction, data manipulation. 
library(haven)
library(here)
library(dplyr)
library(rdlocrand)
library(ggplot2)
library(lmtest)
library(sandwich)
library(rdrobust)
library(rd2d)
library(rddensity)

rm(list = ls())
relative_path <- "/Users/jorditorresvallverdu/Library/Mobile Documents/com~apple~CloudDocs/tse/year2/term1/bobba/data"

data <- read_dta(file.path(relative_path, "RD_sample.dta"))


################Data cleaning
main <- data |> 
    mutate(time = as.numeric(time)) |>
    mutate(population= as.numeric(population)) |>
    filter(time>30) |>
    mutate(
    time= time-120,
    population= population-500, 
    D_pop= as.integer(population<=0) ,
    D_time= as.integer(time>=0)) #compute the two treatment variables 
   
rm(data)
#Note: there are some extreme values for some running variables should we consider that in the cleaning process???


#define vector of running variables
R_list <- list(
  population = main$population,
  time = main$time
)

#define vector of outcomes
Y_list <- list(
  wage = main$wage,
  score = main$score
)

#Define vector of treatment dummies
D_list <- list(
  population = main$D_pop,
  time       = main$D_time
)

#define covariates to test balance
X <- as.matrix(main[ , c( "gender", "univ", "exp_publica_c", "exp_privada_c")])

################Q.1
## Local RD-style descriptive plots (dots only, no smoothing)

library(dplyr)
library(ggplot2)

# Collapse at school level
plot_data <- main %>%
  group_by(id_school) %>%
  summarise(
    wage = mean(wage, na.rm = TRUE),
    score = mean(score, na.rm = TRUE),
    population = first(population),
    time = first(time),
    .groups = "drop"
  )

plot_data %>%
  filter(population >= -500 & population <= 500) %>%
  mutate(pop_bin = cut(population, breaks = seq(-500, 500, by = 50))) %>%
  group_by(pop_bin) %>%
  summarise(
    pop_mid = mean(population, na.rm = TRUE),
    wage = mean(wage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = pop_mid, y = wage)) +
  geom_point(color = "darkred", size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Local RD: Wage vs Population",
    x = "Population (centered at cutoff)",
    y = "Average Wage"
  ) +
  theme_minimal(base_size = 13)

plot_data %>%
  filter(population >= -500 & population <= 500) %>%
  mutate(pop_bin = cut(population, breaks = seq(-500, 500, by = 50))) %>%
  group_by(pop_bin) %>%
  summarise(
    pop_mid = mean(population, na.rm = TRUE),
    score = mean(score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = pop_mid, y = score)) +
  geom_point(color = "darkred", size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Local RD: Score vs Population",
    x = "Population (centered at cutoff)",
    y = "Average Score"
  ) +
  theme_minimal(base_size = 13)

plot_data %>%
  filter(time >= -90 & time <= 90) %>%
  mutate(time_bin = cut(time, breaks = seq(-90, 90, by = 5))) %>%
  group_by(time_bin) %>%
  summarise(
    time_mid = mean(time, na.rm = TRUE),
    wage = mean(wage, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = time_mid, y = wage)) +
  geom_point(color = "darkred", size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Local RD: Wage vs Time",
    x = "Time (centered at cutoff)",
    y = "Average Wage"
  ) +
  theme_minimal(base_size = 13)

plot_data %>%
  filter(time >= -90 & time <= 90) %>%
  mutate(time_bin = cut(time, breaks = seq(-90, 90, by = 5))) %>%
  group_by(time_bin) %>%
  summarise(
    time_mid = mean(time, na.rm = TRUE),
    score = mean(score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = time_mid, y = score)) +
  geom_point(color = "darkred", size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Local RD: Score vs Time",
    x = "Time (centered at cutoff)",
    y = "Average Score"
  ) +
  theme_minimal(base_size = 13)



#use program to compute the windows, but this is inefficient, cause this is an input of the previous command too. 

run_all <- function(Y, R, D, X, type) {
  # 1. Window selection
  win <- rdwinselect(R=R, X=X, cutoff=0, statistic=type, level=0.15, reps=1000, plot = TRUE)
    wl <- win$window[1]
    wr <- win$window[2]
    #I know this is redundant, but wanted to pass one onto the other but apparently does not work with wl...
  # 2. Randinf
  ri <- rdrandinf(Y=Y, R=R, level=0.15, cutoff=0, fuzzy=NULL, statistic=type, reps=1000, covariates = X)

  # 3. Sensitivity
  rs <- rdsensitivity(Y=Y, R=R, cutoff=0, wlist = c(2,3,4,5,6), fuzzy=NULL, statistic=type, reps=1000)

  list(window=win, inference=ri, sensitivity=rs)
}

#rdbounds not supported, does not seem to run that well. 


grid <- expand.grid(R=names(R_list), Y=names(Y_list))

output <- apply(grid, 1, function(row) {
  Rn <- row["R"]
  Yn <- row["Y"]
  cat("iteration:", Rn, Yn, "\n")
  run_all(Y_list[[Yn]], R_list[[Rn]], D_list[[Rn]], X, type="diffmeans")
})



################Exercise 2. Continuity based approaches.
#Assume: continuity around the cutoff-->test for continuity of the pdf at Xi

#2.1. Global parametric approach

#First we do a simple regression
globalrd1 <- lm(wage ~ population + D_pop + D_pop:population, data = main)

cl_vcov1 <- vcovCL(globalrd1, cluster = ~id_school)  
coeftest(globalrd1, vcov = cl_vcov1)


globalrd2 <- lm(score ~ population + D_pop + D_pop:population, data = main)

cl_vcov2 <- vcovCL(globalrd2, cluster = ~id_school)  
coeftest(globalrd2, vcov = cl_vcov2)


globalrd3 <- lm(wage ~ time + D_time + D_time:time, data = main)

cl_vcov3 <- vcovCL(globalrd3, cluster = ~id_school)  
coeftest(globalrd3, vcov = cl_vcov3)



globalrd4 <- lm(score ~ time + D_time + D_time:time, data = main)

cl_vcov4 <- vcovCL(globalrd3, cluster = ~id_school)  
coeftest(globalrd4, vcov = cl_vcov4)

#Keep graphical analysis for afterwards.--> basically I need to simply predict for each line. Also, I need to define different polynomials to see how robust is the effect. 


#2.2. Second we do a local approximation non-parametrically -using Cattaneo et al state of art approach. We will use different methods -kernel and polynomial approximations- to check for robustness. 


rd_local_nonpar <- function(Y, X, ker, p, bselect, cluster_var, nameY, nameX) {
  
  res <- rdrobust(y = Y, x = X, p = p, bwselect=bselect,
                  kernel = ker, vce = "nn", masspoints = "adjust", cluster=cluster_var)
  
  hL <- as.numeric(res$bws[1,1])   #this gets reseted at each apply
  hR <- as.numeric(res$bws[1,2])   
  
  ix <- X >= -hL & X <= hR #restrict support to bandwidth recommended-->this is what is done, no?--->>REVISE

  # Plot 
  rdplot(y = Y[ix], x = X[ix], c = 0, p = p, kernel = ker, h = c(hL, hR),masspoints = "adjust", ci=TRUE, shade=TRUE,
  x.label = nameX %||% "Running variable", y.label = nameY %||% "Outcome", title   = paste0("RD Plot: ", nameY, " vs ", nameX),
  x.lim   = c(-hL, hR))
  #just to make it nicer: (NOTE: NEED TO ADD CI!!) But to the regression discontinuity line-->DOES THE program accept it? does not seem so! kind of slopish...Add myself ; code it.

  
  return(data.frame(
    outcome = nameY,
    running = nameX,
    tau_hat = res$coef[1, 1],
    se      = res$se[1, 1],
    pval    = res$pv[1, 1]
  ))
}

results <- apply(grid, 1, function(row) {
  Rn <- row["R"] #useful trick that I apply throughout, maybe inefficiently though. 
  Yn <- row["Y"]
  cat("iteration:", Rn, Yn, "\n")
  rd_local_nonpar(Y = Y_list[[Yn]], X = R_list[[Rn]], ker = "tri", p = 1, bselect="mserd", cluster_var=main$id_school, nameY = Yn, nameX = Rn) ##change this to try different stuff and generate different tables. 
})

#NOTE:#When I write this I will make sure to try different specifications as a robustness check. 

rd_density_test_population <-rddensity(
R_list$population,
c = 0,
p = 2,
q = 0,
kernel = "triangular",
massPoints = TRUE,
bwselect = "diff",
all = FALSE,
regularize = TRUE,
binoNW = 10,
binoP = 0.5
)

rd_density_test_time <-rddensity(
R_list$time, c = 0, p = 2, q = 0,
kernel = "triangular",
massPoints = TRUE,
bwselect = "diff",
all = FALSE,
regularize = TRUE,
binoNW = 10,
binoP = 0.5
)



rdplotdensity( rd_density_test_population,
R_list$population,
plotN = 10, plotGrid = c("es", "qs"), alpha = 0.05, CIsimul = 2000, histFillCol = 3, histFillShade = 0.2, histLineCol = "white",
title = "Manipulation test time",
xlabel = "time",
ylabel = "density",
)

rdplotdensity(rd_density_test_time,
R_list$time,
plotN = 10, plotGrid = c("es", "qs"), alpha = 0.05,
CIsimul = 2000, histFillCol = 3, histFillShade = 0.2, histLineCol = "white",
title = "Manipulation test time",
xlabel = "population",
ylabel = "density",
)


##robustness
results_robust1 <- apply(grid, 1, function(row) {
  Rn <- row["R"] #useful trick that I apply throughout, maybe inefficiently though. 
  Yn <- row["Y"]
  cat("iteration:", Rn, Yn, "\n")
  rd_local_nonpar(Y = Y_list[[Yn]], X = R_list[[Rn]], ker = "tri", p = 2, bselect="mserd", cluster_var=main$id_school, nameY = Yn, nameX = Rn) ##change this to try different stuff and generate different tables. 
})

results_robust2 <- apply(grid, 1, function(row) {
  Rn <- row["R"] #useful trick that I apply throughout, maybe inefficiently though. 
  Yn <- row["Y"]
  cat("iteration:", Rn, Yn, "\n")
  rd_local_nonpar(Y = Y_list[[Yn]], X = R_list[[Rn]], ker = "tri", p = 3, bselect="mserd", cluster_var=main$id_school, nameY = Yn, nameX = Rn) ##change this to try different stuff and generate different tables. 
})

results_robust3 <- apply(grid, 1, function(row) {
  Rn <- row["R"] #useful trick that I apply throughout, maybe inefficiently though. 
  Yn <- row["Y"]
  cat("iteration:", Rn, Yn, "\n")
  rd_local_nonpar(Y = Y_list[[Yn]], X = R_list[[Rn]], ker = "epanechnikov", p = 1, bselect="mserd", cluster_var=main$id_school, nameY = Yn, nameX = Rn) ##change this to try different stuff and generate different tables. 
})

results_robust4 <- apply(grid, 1, function(row) {
  Rn <- row["R"] #useful trick that I apply throughout, maybe inefficiently though. 
  Yn <- row["Y"]
  cat("iteration:", Rn, Yn, "\n")
  rd_local_nonpar(Y = Y_list[[Yn]], X = R_list[[Rn]], ker = "epanechnikov", p = 2, bselect="mserd", cluster_var=main$id_school, nameY = Yn, nameX = Rn) ##change this to try different stuff and generate different tables. 
})


################Exercise 3. Define first rurality frontier

main <- main |>
      mutate(rurality_frontier= sqrt((population)^2+(time)^2)
      ) |>
      mutate(rurality_frontier=ifelse (main$D_pop==1 | main$D_time==1, rurality_frontier*(-1), rurality_frontier ) )
       



grid2 <- data.frame(Y=names(Y_list)) ##dreplicate previous command now with one running variable and two outcomes.


results2 <- apply(grid2, 1, function(row) {
  Yn <- row["Y"]
  cat("iteration:", Yn, "\n")
  rd_local_nonpar(Y = Y_list[[Yn]], X = main$rurality_frontier, ker = "tri", p = 1, bselect="mserd", cluster_var=main$id_school, nameY = Yn, nameX = "rurality") ##change this to try different stuff and generate different tables. 
})
#NOTE: here i should pass it also with RD2 package?

################Exercise 4. Biundary rd

# treatment rule (OR condition

main <- main |> mutate(t=as.numeric(main$population<=0 | main$time>=0))
X_mat <- cbind(main$population, main$time)


b1 <- cbind(seq(0, 200, length.out = 20), rep(0, 20)) #ad hoc, is there a data drive way to select points?

# Other arm: population = 0, time decreases
b2 <- cbind(rep(0, 20), seq(0, -40, length.out = 20))

# Combine (this is your "L" shape)
b <- rbind(b1, b2)

rd2_wage <-rd2d(Y=main$wage, X=X_mat, t=main$t, b=b, p = 1, kernel = "tri", masspoints = "adjust", level = 95, cbands = TRUE, repp = 1000, bwselect = "mserd")

rd2_score <-rd2d(Y=main$score, X=X_mat, t=main$t, b=b, p = 1, kernel = "tri", masspoints = "adjust", level = 95, cbands = TRUE, repp = 1000, bwselect = "mserd")

#graph results, inefficiently probably.
results_rd2_wage <- as.data.frame(rd2_wage$results)

results_rd2_wage <- results_rd2_wage |>
  dplyr::select(b1, b2, tau = Est.q, se = Se.q, ci_l = CI.lower, ci_u = CI.upper)


ggplot(results_rd2_wage, aes(x = 1:nrow(results_rd2_wage), y = tau)) +
  geom_point(color = "blue", size = 2) +
  geom_line(color = "blue", alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u), width = 0.2, color = "gray40") +
  labs(
    title = "Point Estimates along the Rurality Frontier (Wage)",
    x = "Boundary Evaluation Point (b₁–b₄₀)",
    y = "Estimated Treatment Effect (τ̂)"
  ) +
  theme_minimal(base_size = 13)



  results_rd2_score <- as.data.frame(rd2_score$results)

results_rd2_score <- results_rd2_score |>
  dplyr::select(b1, b2, tau = Est.q, se = Se.q, ci_l = CI.lower, ci_u = CI.upper)


ggplot(results_rd2_score, aes(x = 1:nrow(results_rd2_score), y = tau)) +
  geom_point(color = "blue", size = 2) +
  geom_line(color = "blue", alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u), width = 0.2, color = "gray40") +
  labs(
    title = "Point Estimates along the Rurality Frontier (Score)",
    x = "Boundary Evaluation Point (b₁–b₄₀)",
    y = "Estimated Treatment Effect (τ̂)"
  ) +
  theme_minimal(base_size = 13)

############################################## EOF