##Homework 2 of Policy analysis: RD
##Date: 15/10/2025


#0. Introduction, data manipulation. 
library(haven)
library(here)
library(dplyr)
library(rdlocrand)
library(ggplot2)
library(lmtest)
library(sandwich)
library(rdrobust)


relative_path <- "/Users/jorditorresvallverdu/Library/Mobile Documents/com~apple~CloudDocs/tse/year2/term1/bobba/data"

main <- read_dta(file.path(relative_path, "RD_sample.dta"))

df <- as.data.frame(haven::as_factor(main))
#View(df[1:100, ])


#pre data-cleaning
main <- main |> 
    mutate(time = as.numeric(time)) |>
    mutate(population= as.numeric(population)) |>
    filter(time>30) |>
    mutate(
    time= time-120,
    population= population-500, 
    D_pop= as.integer(population<=0) ,
    D_time= as.integer(time>=0)) #compute the two treatment variables 
   

#Note: there are some extreme values for some running variables should we consider that in the cleaning process???


#1.1 Window selection.

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

################
#Some descriptives, graph analysis 
plot_data <- main %>%
  group_by(id_school) %>%
  summarise(wage = mean(wage, na.rm = TRUE),
            population = first(population),
            time= first(time),
            score= mean(score, na.rm=TRUE),
            .groups = "drop")

collapsed <- main %>%
  filter(population >= -500 & population <= 500) %>%
  group_by(id_school) %>%
  summarise(
    wage = mean(wage, na.rm = TRUE),
    population = first(population),   # population should be school-specific
    .groups = "drop"
  ) %>%
  distinct(id_school, .keep_all = TRUE)   # enforce uniqueness


  ggplot(collapsed, aes(x = population, y = wage)) +
  geom_point(color = "darkred") +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "RD Plot: Wage vs Population (School-level, collapsed)")

  plot_data %>%
  filter(population >= -500 & population <= 500) %>%
  mutate(pop_bin = cut(population, breaks = seq(-500, 500, by = 50))) %>%
  group_by(pop_bin) %>%
  summarise(score = mean(score, na.rm = TRUE),
            pop_mid = mean(population, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = pop_mid, y = score)) +
  geom_point(color = "darkred") +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "RD Plot: Score vs Population (School-level, binned)")



plot_data %>%
  filter(time >= -89 & time <= 89) %>%
  mutate(time_bin = cut(time, breaks = seq(-89, 89, by = 5))) %>%
  group_by(time_bin) %>%
  summarise(wage = mean(wage, na.rm = TRUE),
            time_bin = mean(time, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = time_bin, y = wage)) +
  geom_point(color = "darkred") +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "RD Plot: Wage vs Time (School-level, binned)")


plot_data %>%
  filter(time >= -89 & time <= 89) %>%
  mutate(time_bin = cut(time, breaks = seq(-89, 89, by = 5))) %>%
  group_by(time_bin) %>%
  summarise(score = mean(score, na.rm = TRUE),
            time_bin = mean(time, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = time_bin, y = score)) +
  geom_point(color = "darkred") +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "RD Plot: score vs Time (School-level, binned)")



#use program to compute the windows, but this is inefficient, cause this is an input of the previous command too. 

run_all <- function(Y, R, D, X, type) {
  # 1. Window selection
  win <- rdwinselect(R=R, X=X, cutoff=0, statistic=type, level=0.15, reps=1000, plot = TRUE)
    wl <- win$window[1]
    wr <- win$window[2]

  # 2. Randinf
  ri <- rdrandinf(Y=Y, R=R, wl=wl, wr=wr, cutoff=0, fuzzy=D, statistic=type, reps=1000)

  # 3. Sensitivity
  rs <- rdsensitivity(Y=Y, R=R, cutoff=0, wlist = c(2,3,4,5,6), fuzzy=D, statistic=type, reps=1000)

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


##To review at some other time; ask for the code...



##Exercise 2. Continuity based approaches. 
#Assume: continuity around the cutoff-->test for continuity of the pdf at Xi

#2.1. Global parametric approach

#First we do a simple regression

globalrd1 <- lm(wage ~ population + D_pop + D_pop:population, data = main)

cl_vcov1 <- vcovCL(globalrd1, cluster = ~id_school)  
coeftest(globalrd, vcov = cl_vcov)


globalrd1_2 <- lm(score ~ population + D_pop + D_pop:population, data = main)

cl_vcov1_2 <- vcovCL(globalrd1_2, cluster = ~id_school)  
coeftest(globalrd, vcov = cl_vcov)


globalrd2 <- lm(wage ~ time + D_time + D_time:time, data = main)

cl_vcov2 <- vcovCL(globalrd2, cluster = ~id_school)  
coeftest(globalrd, vcov = cl_vcov)

globalrd2_2 <- lm(score ~ time + D_time + D_time:time, data = main)

cl_vcov2_2 <- vcovCL(globalrd2_2, cluster = ~id_school)  
coeftest(globalrd, vcov = cl_vcov)

#Keep graphical analysis for afterwards.--> basically I need to simply predict for each line. Also, I need to define different polynomials to see how robust is the effect. 


#Second we do a local approximation non-parametrically -using Cattaneo et al state of art approach. We will use different methods -kernel and polynomial approximations- to check for robustness. 


# 1) Estimate RD (this chooses the optimal h for you)

rd_local_nonpar <- function(Y, X, ker, p, bselect, cluster_var, nameY, nameX) {
  
  res <- rdrobust(y = Y, x = X, p = p, bwselect=bselect,
                  kernel = ker, vce = "nn", masspoints = "adjust", cluster=cluster_var)
  
  hL <- as.numeric(res$bws[1,1])   
  hR <- as.numeric(res$bws[1,2])   
  
  ix <- X >= -hL & X <= hR #restrict support to bandwidth recommended-->this is what is done, no?
  
  # Plot 
  rdplot(y = Y[ix], x = X[ix], c = 0, p = p, kernel = ker, h = c(hL, hR),masspoints = "adjust", 
  #just to make it nicer: (NOTE: NEED TO ADD CI!!)
  x.label = nameX %||% "Running variable", y.label = nameY %||% "Outcome", title   = paste0("RD Plot: ", nameY, " vs ", nameX),
  x.lim   = c(-hL, hR))
  
  # Return key results
  return(data.frame(
    outcome = nameY,
    running = nameX,
    tau_hat = res$Estimate[1],     # sestimate
    se      = res$se[1],           
    pval    = res$pv[1],                
    h_left  = hL,
    h_right = hR,
    N_left  = res$N[1],
    N_right = res$N[2]
  ))
}

results <- apply(grid, 1, function(row) {
  Rn <- row["R"] #useful trick that I apply throughout, maybe inefficiently though. 
  Yn <- row["Y"]
  cat("iteration:", Rn, Yn, "\n")
  rd_local_nonpar(Y = Y_list[[Yn]], X = R_list[[Rn]], ker = "tri", p = 1, bselect="mserd", cluster_var=main$id_school, nameY = Yn, nameX = Rn) ##change this to try different stuff and generate different tables. 
})
##When I write this I will make sure to try different specifications. 

#pending to use the rddensity command-->this is basically to see if continuity holds, no?




############################################## EOF