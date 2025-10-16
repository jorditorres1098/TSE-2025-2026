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
View(df[1:100, ])


#pre data-cleaning
main <- main |> 
    mutate(time = as.numeric(time)) |>
    mutate(population= as.numeric(population)) |>
    filter(time>30) |>
    mutate(
    time= time-120,
    population= population-500, 
    D_pop= population<=0 ,
    D_time= time<=0) 
   

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


#Define treatment




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
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
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
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "RD Plot: Score vs Population (School-level, binned)")

plot_data %>%
  filter(time >= -30 & time <= 30) %>%
  mutate(pop_bin = cut(population, breaks = seq(-500, 500, by = 50))) %>%
  group_by(pop_bin) %>%
  summarise(wage = mean(wage, na.rm = TRUE),
            pop_mid = mean(population, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = pop_mid, y = wage)) +
  geom_point(color = "darkred") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "RD Plot: Wage vs Population (School-level, binned)")




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
  wmasspoints = TRUE, 
  plot=TRUE
) }
)

#Here I need to use the bounds in the first derivation to compute the difference in means of the main variables of interest. Why it asks this difference?


##Basically the issue is that we reject the hypothesis of balance very early on, so the bandwidth is what it is...


#1.2.
ranunif <- function(Y,R, type){
        rdrandinf(
    Y=Y,
    R=R,
    cutoff = 0,       
    level = 0.15,
    statistic = type, 
    reps = 1000,
    seed = 123,
    wmasspoints = TRUE,
    plot=TRUE
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

#why I don't have good graphs here? work on this too, get the correct graphs. Good formated. 

##Fins aquí tinc una noció clara del que he de fer

rdbounds <- function(Y,R, type){
  rdrbounds(
  Y=Y,
  R=R,
  statistic = type,
  evalat = "cutoff",
  kernel = "uniform",
  reps = 1000,
  seed = 123
  )
}


output2 <- apply(grid, 1, function(row){
    Rname <- row["R"]
    Yname <- row["Y"]

    cat("iteration" ,Rname, Yname)

    rdbounds(Y=Y_list[[Yname]],
    R=R_list[[Rname]], 
    type="ksmirnov")
})

#don't know why this is not working...Maybe not applying well the command

############################################## EOF