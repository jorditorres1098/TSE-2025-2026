##This is a replication of Mourifie et al (2020)
##Author: Jordi Torres Vallverdú
##date: 10/11/2025

##Part 1: data cleaning 
#for sure I need to use another measure of Y1  y0... D is selection into stem or non-stem



library(haven)

data_path <- "/Users/jorditorresvallverdu/Library/Mobile Documents/com~apple~CloudDocs/tse/year1/papers_final_memoire/data/clean/"

data <- read_dta(file.path(data_path, "complete_panel_clean_trackadded.dta"))

#If I need to do Bootstrap, do I need to use Julia to be faster?

summary(data)
