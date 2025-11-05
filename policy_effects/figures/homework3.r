##Author: Jordi Torres
##Date: 05/11/2025
##TAKEHOME 3: RDD
#-------------------------------------------------------------------------------
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
library(xtable)
library(fixest)

rm(list = ls())


##Define relative paths 
relative_path_data <- "/Users/jorditorresvallverdu/Library/Mobile Documents/com~apple~CloudDocs/tse/year2/term1/bobba/data" ##place here the relative path. 

out_path_tables <- "/Users/jorditorresvallverdu/Documents/GitHub/TSE-2025-2026/policy_effects/figures/"


data <- read_dta(file.path(relative_path_data,"panel_06.dta"), encoding = "latin1")
main <- haven::zap_labels(data) |> as.data.frame() ###just to remove the annoying labels that make this unrenderable in R. 
View(main)

rm(data)

##with dta we have labels passing by usual names... 

#Exercise 1
classification <- main |>
    group_by(cvemun) |>
    summarise(
        stayer_always_treated= all(sp==1) ,
        stayer_never_treated= all(sp==0) , 
        switcher= any(sp==0) & any(sp==1) 
        
    )

classification <- classification |>
  mutate(group = case_when(
    switcher ~ "Switcher",
    stayer_always_treated ~ "Always Treated",
    stayer_never_treated ~ "Never Treated",
    TRUE ~ "Other"
  ))

table(classification$group)
prop.table(table(classification$group))

classification_summary <- classification |>
  count(group) |>
  mutate(rel_freq = n / sum(n))

latex_table <- xtable(classification_summary,
                      caption = "Classification of municipalities",
                      label = "tab:classification")

print(latex_table, 
      file = file.path(out_path_tables, "table1.tex"), 
      include.rownames = FALSE, 
      booktabs = TRUE)  # booktabs = nicer formatting for LaTeX


#Exercise 1.1 estimate it using a collapsed dataset: the level of observation is the individual, but we want the level of observation to be the municipality. 

data_analysis <- main |>
  group_by(cvemun, quarter) |>
  summarise(
    treat= first(sp) ,
    mean_age      = mean(age, na.rm = TRUE),
    mean_hwage    = mean(hwage, na.rm = TRUE),
    mean_sec_occup = mean(sec_occup, na.rm = TRUE),
    share_formal   = mean(occup == 2, na.rm = TRUE),  # example: share formal workers
    share_high_educ = mean(educ == 1, na.rm = TRUE),  # share high schooling
    .groups = "drop"
  ) |>
  mutate(lwage= log(mean_hwage))

##try a 

fe_model <- feols(lwage ~ treat | cvemun + quarter, 
                  data = data_analysis, 
                  cluster = ~cvemun)
summary(fe_model)

