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
library(plm)
library(TwoWayFEWeights)
library(bacondecomp)
library(texreg)
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

#collapse the data 
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

##this is the same as the reghdfe command in STATA

fe_model <- feols(lwage ~ treat | cvemun + quarter, 
                  data = data_analysis, 
                  cluster = ~cvemun)
summary(fe_model)


fe_model_wc <- feols(lwage ~ treat + mean_age +  share_formal + mean_sec_occup  | cvemun + quarter, 
                  data = data_analysis, 
                  cluster = ~cvemun)
summary(fe_model_wc)


# first differences 
pdata <- pdata.frame(data_analysis, index = c("cvemun", "quarter")) #same as xtset

# First-difference model
fd_model <- plm(lwage ~ treat, data = pdata, model = "fd", cluster=cvemun)

fd_model_wc <- plm(lwage ~ treat + mean_age+ share_formal + share_high_educ, data = pdata, model = "fd", cluster=cvemun)


texreg(
  list(fe_model, fe_model_wc, fd_model, fd_model_wc),
  custom.model.names = c("2WFE", "2WFE", "FD", "FD"),
  stars = c(0.01, 0.05, 0.1),
  caption = "Fixed Effects and First Differences Models",
  label = "tab:fe_fd",
  booktabs = TRUE,
  use.packages = FALSE,
  dcolumn = TRUE,
  file = file.path(out_path_tables, "fe_fd_results.tex"),
  override.gof = list(
    c(nobs(fe_model2), nobs(fe_model2_wc), nobs(fd_model), nobs(fd_model_wc))
  ),
  override.gof.names = c("Observations"),
  override.gof.decimal = c(0)
)

##Should we add cluster se at the g,t 

#bacon

data_transitions <- data_analysis %>%
  arrange(cvemun, quarter) %>%          # make sure sorted by id & time
  group_by(cvemun) |>
  mutate(
    lag_treat = dplyr::lag(treat),
    switched  = treat != lag_treat      # TRUE whenever treatment changes
  ) |>
  summarise(
    n_switches = sum(switched, na.rm = TRUE)   # count TRUEs
  ) |>
  ungroup()


data_bacon <- data_analysis |>
  group_by(cvemun) |>
  filter(all(!is.na(lwage))) |>
  ungroup() |>
  filter(cvemun != 29005) ##we filter out the weird switcher across time.

  #I can also plot across time the changes of the outcomes to see if potential outcomes hold. This is a plot I should do today. 

data_patterns <- data_bacon |>
  arrange(cvemun, quarter) |>
  group_by(cvemun) |>
  summarise(code = paste(treat, collapse = "")) |>
  ungroup()

data_bacon <- data_bacon |>
  left_join(data_patterns, by = "cvemun")

pattern_means <- data_bacon |>
  group_by(code, quarter) |>
  summarise(mean_hwage = mean(mean_hwage, na.rm = TRUE))

ggplot(pattern_means, aes(x = quarter,
                          y = mean_hwage,
                          color = code,
                          group = code,
                          shape = code)) +
  geom_line(linewidth = 1) +        # modern equivalent of size=
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set2") +   # clean, qualitative palette
  theme_minimal(base_size = 13) +
  labs(
    title = "Average hourly wage by treatment pattern",
    x = "Quarter",
    y = "Average hourly wage",
    color = "Pattern",
    shape = "Pattern"
  )


#WE NEED to drop those obs with missings - we can't have missings nor unbalancedness in the panel dataset. 

baconres <- bacon(lwage ~ treat,
                  data = data_bacon,
                  id_var = "cvemun",
                  time_var = "quarter")

ggplot(baconres) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x = "Weight", y = "Estimate", shape = "Type")

#twowayfixedeffects

twoway1 <- twowayfeweights(data_bacon, 
  Y="lwage",
  G="cvemun",
  T="quarter" ,
  D="treat"

)

twoway2 <- twowayfeweights(data_bacon, 
  Y="lwage",
  G="cvemun",
  T="quarter" ,
  D="treat",
  type="fdTR", 
  D0= "mean_hwage"
  
)

##unsure about this