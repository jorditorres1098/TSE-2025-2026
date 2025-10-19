##Homework 2 of Policy analysis: RD
##Date: 15/10/2025


#0. Introduction, data manipulation. 
library(haven)
library(here)
library(dplyr)
library(rdlocrand)
library(ggplot2)


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

plot_data %>%
  filter(population >= -500 & population <= 500) %>%
  mutate(pop_bin = cut(population, breaks = seq(-500, 500, by = 50))) %>%
  group_by(pop_bin) %>%
  summarise(wage = mean(wage, na.rm = TRUE),
            pop_mid = mean(population, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = pop_mid, y = wage)) +
  geom_point(color = "darkred") +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "RD Plot: Wage vs Population (School-level, binned)")

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



##Exercise 2.










############################################## EOF