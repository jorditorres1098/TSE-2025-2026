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

      if (!is.na(capacity_j[j]) && capacity_j[j] > 0 && u > 0) {
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

  matches <- matches[!is.na(id_school)] #drop missing?
  return(matches)
}



df <- fread("/Users/jorditorresvallverdu/Documents/GitHub/TSE-2025-2026/policy_effects/model_restricted_clogit.csv.gz")
match_clog <- dictator_algorithm(df, "uij")

df2 <- fread("/Users/jorditorresvallverdu/Documents/GitHub/TSE-2025-2026/policy_effects/model_restricted_mixlogit.csv")
match_clog_mix <- dictator_algorithm(df2, "mix_uij")





#########EOF