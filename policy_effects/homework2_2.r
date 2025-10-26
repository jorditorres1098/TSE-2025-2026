##Homework 2 of Policy analysis: RD
##Date: 15/10/2025

rm(list = ls())
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
library(data.table)
library(mlogit)

library(data.table)

##First define the algorithm. 


dictator_algorithm <- function(data, utility_col) {

  # Ensure data.table
  data <- as.data.table(data)

  # Teacher information
  score_i <- data[, .(score = unique(score)), by = id_teach]
  teacher_order <- score_i[order(-score), id_teach] ## sort teachers by their score 

  # School capacity
  capacity_j <- data[, .(n_plazas = unique(n_plazas)), by = id_school]
  capacity_j <- setNames(capacity_j$n_plazas, capacity_j$id_school)

  characteristics_j <- data[, .(population=unique(population_norm), time=unique(time_norm), treat_extreme= unique(treat_extremerural), treat_rural= unique(treat_rural), treat_mod= unique(treat_modrural), wage= unique(wage)), by= id_school]

  # Rank of preferences
  data[, rank := frank(-get(utility_col), ties.method = "random"), by = id_teach]
  teacher_pref <- data[order(id_teach, rank),
                       .(pref_list = list(id_school),
                         utils = list(get(utility_col))),
                       by = id_teach]

  assign <- rep(NA_integer_, length(teacher_order))
  names(assign) <- teacher_order

  for (i in teacher_order) { 
    row <- teacher_pref[id_teach == i]
    pref_teach <- row$pref_list[[1]]
    utils_teach <- row$utils[[1]]

    for (k in seq_along(pref_teach)) {  
      j <- pref_teach[k]
      u <- utils_teach[k]

      if (!is.na(capacity_j[j]) && capacity_j[j] > 0) {
        assign[as.character(i)] <- j
        capacity_j[j] <- capacity_j[j] - 1
        break
      }
    }
  }

  # export
  matches <- data.table(
    id_teach = as.integer(names(assign)),
    id_school = as.integer(assign)


  )

  matches <- matches[!is.na(id_school)]

  matches[, outside_opt := as.integer(id_school %in% c(1, 2))]


  # Merge school characteristics
  matches <- merge(matches, characteristics_j, by = "id_school", all.x = TRUE)

  # Merge teacher scores
  matches <- merge(matches, score_i, by = "id_teach", all.x = TRUE)

  #Normalize cutoffs and apply same cleaning as before! (pending)

  return(matches)
}


##Exercise 7

df <- fread("/Users/jorditorresvallverdu/Documents/GitHub/TSE-2025-2026/policy_effects/model_restricted_clogit.csv")
match_clog <- dictator_algorithm(df, "uij")

rm(df)



main <- match_clog[outside_opt == 0] |>
  mutate(
    time = as.numeric(time),
    population = as.numeric(population)
  ) |>
  filter(time > 30) |>
  mutate(
    time = time - 120,
    population = population - 500,
    D_pop = as.integer(population <= 0),
    D_time = as.integer(time >= 0)
  )

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

grid <- expand.grid(R=names(R_list), Y=names(Y_list))



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
  #just to make it nicer: (NOTE: NEED TO ADD CI!!) But to the regression discontinuity line-->DOES THE program accept it? does not seem so! kind of slopish...

  
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


###Mix logit
df2 <- fread("/Users/jorditorresvallverdu/Documents/GitHub/TSE-2025-2026/policy_effects/model_restricted_mixlogit.csv")
match_clog_mix <- dictator_algorithm(df2, "mix_uij")

rm(df2)


main <- match_clog_mix[outside_opt == 0] |>
  mutate(
    time = as.numeric(time),
    population = as.numeric(population)
  ) |>
  filter(time > 30) |>
  mutate(
    time = time - 120,
    population = population - 500,
    D_pop = as.integer(population <= 0),
    D_time = as.integer(time >= 0)
  )

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

grid <- expand.grid(R=names(R_list), Y=names(Y_list))


results2 <- apply(grid, 1, function(row) {
  Rn <- row["R"] #useful trick that I apply throughout, maybe inefficiently though. 
  Yn <- row["Y"]
  cat("iteration:", Rn, Yn, "\n")
  rd_local_nonpar(Y = Y_list[[Yn]], X = R_list[[Rn]], ker = "tri", p = 1, bselect="mserd", cluster_var=main$id_school, nameY = Yn, nameX = Rn) ##change this to try different stuff and generate different tables. 
})



######clogit cf
###Clogit logit
df3 <- fread("/Users/jorditorresvallverdu/Documents/GitHub/TSE-2025-2026/policy_effects/model_restricted_clogit_cf.csv")
match_clog_cf <- dictator_algorithm(df3, "uij_cf")

rm(df3)


###mixlogit
df4 <- fread("/Users/jorditorresvallverdu/Documents/GitHub/TSE-2025-2026/policy_effects/model_restricted_mixlogit_cf.csv")
match_mix_cf <- dictator_algorithm(df4, "mix_uij_cf")

rm(df4)




####Computing differences

#define all rural
for (d in list(match_clog, match_clog_cf, match_clog_mix, match_clog_mix_cf)) {
  d[, rural := as.integer(treat_extreme == 1 | treat_rural == 1 | treat_mod == 1)]
}

##Averages in level of rurality

collapse_summary <- function(data, label) {
  data[, .(mean_score = mean(score, na.rm = TRUE),n = .N
  ), by = .(rurality = fifelse(treat_extreme == 1, "Extreme",
                        fifelse(treat_rural == 1, "Rural",
                        fifelse(treat_mod == 1, "Moderate", "Urban"))))
  ][, model := label]
}

sum_clog    <- collapse_summary(match_clog, "Clogit")
sum_clog_cf <- collapse_summary(match_clog_cf, "Clogit_CF")
sum_mix     <- collapse_summary(match_clog_mix, "Mixlogit")
sum_mix_cf  <- collapse_summary(match_mix_cf, "Mixlogit_CF")

summary_all <- rbind(sum_clog, sum_clog_cf, sum_mix, sum_mix_cf)


# Compute differences between CF and baseline

collapse_rural_urban <- function(data, label) {
  data[, .(mean_score = mean(score, na.rm = TRUE), n = .N), by = .(rural = as.integer(treat_extreme == 1 | treat_rural == 1 | treat_mod == 1))
  ][, model := label]
}

rural_clog    <- collapse_rural_urban(match_clog, "Clogit")
rural_clog_cf <- collapse_rural_urban(match_clog_cf, "Clogit_CF")
rural_mix     <- collapse_rural_urban(match_clog_mix, "Mixlogit")
rural_mix_cf  <- collapse_rural_urban(match_mix_cf, "Mixlogit_CF")

rural_summary <- rbind(rural_clog, rural_clog_cf, rural_mix, rural_mix_cf)


compute_effect <- function(base, cf, model_name) {
  merged <- merge(base, cf, by = "rural", suffixes = c("", "_cf"))
  merged[, .(
    model = model_name,
    rural = rural,
    mean_score = mean_score,
    mean_score_cf = mean_score_cf,
    diff_cf = mean_score_cf - mean_score
  )]
}

effect_clog <- compute_effect(rural_clog, rural_clog_cf, "Clogit")
effect_mix  <- compute_effect(rural_mix, rural_mix_cf, "Mixlogit")

effect_summary <- rbind(effect_clog, effect_mix)


#########EOF