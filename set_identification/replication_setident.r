##Replication exercise Mourifié, Henry, Romuango
##Date: 25/11/2025
##Author: Jordi Torres

rm(list = ls())

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

#Watchout the data is confidential; so maybe I need to actually de-anonimize a little bit more. 

relative_path <- "/Users/jorditorresvallverdu/Library/Mobile Documents/com~apple~CloudDocs/tse/year1/papers_final_memoire/data/clean/"

data <- read_dta(file.path(relative_path, "roymodel_database.dta"))