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
library(rd2d)
library(data.table)
library(mlogit)

library(data.table)

df <- fread("/Users/jorditorresvallverdu/Documents/GitHub/TSE-2025-2026/policy_effects/model_restricted_clogit.csv.gz")
#Needs to have uij, scorei, nplazasj, id_teach, id_school as input. 

#Teacher information
score_i <- df[, .(score = unique(score)), by = id_teach]
teacher_order <- score_i[order(-score), id_teach] ## sort teachers by their score 

#School capacity
capacity_j <- df[, .(n_plazas = unique(n_plazas)), by = id_school]##weird that there are schools with 10000 slots, this is probably a typo in the data.
capacity_j <- setNames(capacity_j$n_plazas, capacity_j$id_school)


#Rank of preferences
df[, rank := frank(-uij, ties.method = "random"), by = id_teach]
teacher_pref <- df[order(id_teach, rank), .(pref_list = list(id_school), utils   = list(uij)), by = id_teach]

assignments <- rep(NA_integer_, length(teacher_order))
names(assignments) <- teacher_order


for (i in teacher_order){ 
    ##Order by score from highesr to lowest

    row <- teacher_pref[id_teach == i]
    pref_teach <- row$pref_list[[1]]   # vector of school ids ordered by pref
    utils_teach <- row$utils[[1]]      # vector of same length with utility (we have to check that utility is higher than outside option!!!)

    for (k in seq_along(pref_teach)){  

        j <- pref_teach[k]
        u <- utils_teach[k] ##in case we want to impose also that u>positive? 

        if (capacity_j[j]>0 && u>0){
            assignments[as.character(i)] = j
            capacity_j[j]=capacity_j[j]-1
            break
        }
    }

}

sum(is.na(assignments))         
sum(!is.na(assignments))        
length(unique(na.omit(assignments)))  

#some unmatched, but I think it is reasonable to impose this restriction.

#Here I can merge this with the results from the mixlogit. 



#Here I merge back with original light dataset in both cases. 


######EOF
