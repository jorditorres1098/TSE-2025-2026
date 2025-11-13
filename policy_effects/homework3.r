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
library(did)
rm(list = ls())


##Define relative paths 
relative_path_data <- "/Users/jorditorresvallverdu/Library/Mobile Documents/com~apple~CloudDocs/tse/year2/term1/bobba/data" ##place here the relative path. 

out_path_tables <- "/Users/jorditorresvallverdu/Documents/GitHub/TSE-2025-2026/policy_effects/figures/"


data <- read_dta(file.path(relative_path_data,"panel_06.dta"), encoding = "latin1")
main <- haven::zap_labels(data) |> as.data.frame() ###just to remove the annoying labels that make this unrenderable in R. 
View(main)

rm(data)

##with dta we have labels passing by usual names... 

##______________________________________________________________________________
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
      booktabs = TRUE)  


#Exercise 1.1 estimate it using a collapsed dataset: the level of observation is the individual, but we want the level of observation to be the municipality. First differences does not make sense... 

#Exercise 2
##______________________________________________________________________________

#collapse the data 
data_analysis <- main |>
  group_by(cvemun, quarter) |>
  summarise(
    treat= first(sp) ,
    mean_age      = mean(age, na.rm = TRUE),
    mean_hwage    = mean(hwage, na.rm = TRUE),
    mean_sec_occup = mean(sec_occup, na.rm = TRUE),
    share_formal   = mean(occup == 2, na.rm = TRUE),  
    share_high_educ = mean(educ == 1, na.rm = TRUE),  # share high schooling
    .groups = "drop"
  ) |>
  mutate(lwage= log(mean_hwage))

##TWFE

fe_model <- feols(lwage ~ treat | cvemun + quarter, 
                  data = data_analysis, 
                  cluster = ~cvemun)
summary(fe_model)


fe_model_wc <- feols(lwage ~ treat + mean_age +  share_formal + mean_sec_occup  | cvemun + quarter, 
                  data = data_analysis, 
                  cluster = ~cvemun)
summary(fe_model_wc)


# first differences -->useless! 
pdata <- pdata.frame(data_analysis, index = c("cvemun", "quarter")) #same as xtset

# First-difference model
fd_model <- plm(lwage ~ treat, data = pdata, model = "fd", cluster=cvemun)

fd_model_wc <- plm(lwage ~ treat + mean_age+ share_formal + share_high_educ, data = pdata, model = "fd", cluster=cvemun)


#export directly to latex doc (tables update automatically)
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
    c(nobs(fe_model), nobs(fe_model_wc), nobs(fd_model), nobs(fd_model_wc))
  ),
  override.gof.names = c("Observations"),
  override.gof.decimal = c(0)
)

###weights...
##______________________________________________________________________________

##First differences data
data_transitions <- data_analysis %>%
  arrange(cvemun, quarter) %>%         #sort
  group_by(cvemun) |>
  mutate(
    lag_treat = dplyr::lag(treat),
    switched  = treat != lag_treat      # TtrRUE when treatment changes
  ) |>
  summarise(
    n_switches = sum(switched, na.rm = TRUE)   # count trues
  ) |>
  ungroup()

#prepare data
data_bacon <- data_analysis |>
  group_by(cvemun) |>
  filter(all(!is.na(lwage))) |>
  ungroup() |>
  filter(cvemun != 29005) ##we filter out the weird switcher across time (makes it non-staggered treatment; this goes against all programs)

#preliminary analysis
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
  geom_line(linewidth = 1) +       
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set2") +  
  theme_minimal(base_size = 13) +
  labs(
    title = "Average hourly wage by treatment pattern",
    x = "Quarter",
    y = "Average hourly wage",
    color = "Pattern",
    shape = "Pattern"
  )

#WE NEED to drop those obs with missings - we can't have missings nor unbalancedness in the panel dataset. 
##These are averages, add CI!!

#Bacon_decomp

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
  D="treat", 
  summary_measures = TRUE

)

#plot
unique_weights <- twoway1$dat_result |>
          distinct(weight) |>
          arrange(weight)

ggplot(unique_weights, aes(x = seq_along(weight), y = weight)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Weights from Two-Way FE Decomposition",
    x = "Observation index",
    y = "Weight"
  )


#need to take averages across time

ggplot(twoway1$dat_result, aes(x = as.factor(T), y = weight)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal(base_size = 13) +
  labs(title = "Distribution of TWFE Weights by Quarter",
       x = "Quarter (T)", y = "Weight")



#regression

weights_bygt <- twoway1$dat_result |>
              rename(cvemun=G, quarter=T) 
              
main_muni <- main |>
  select(cvemun, pobtot, indmarg95) |>
  distinct(cvemun, .keep_all = TRUE)

data_reg <- data_bacon |>
          left_join(weights_bygt, weight, by=c("cvemun", "quarter")) |>
          left_join(main_muni |> select(cvemun, pobtot, indmarg95), by=c("cvemun") ) 


reg_weights <- lm(weight ~ mean_age + mean_sec_occup + share_high_educ + pobtot + indmarg95 , data=data_reg ) 

summary(reg_weights)

#Pending: add X as controls to further support parallel trend assumption

##first differences

first_dif <- data_bacon |>
        arrange(cvemun, quarter) |>
        group_by(cvemun) |>
        mutate(
          lagwage= lwage- dplyr::lag(lwage) ,
          lagtreat= treat- dplyr::lag(treat)
        ) |>
        ungroup() |>
        filter(!is.na(lagwage) & !is.na(lagtreat)) ##this is to drop the first lime of missings


#this needs to be done on first difference data!!!
twoway2 <- twowayfeweights(first_dif, 
  Y="lagwage",
  G="cvemun",
  T="quarter" ,
  D="lagtreat",
  type="fdTR", 
  D0= "lwage"
  
)

unique_weights2 <- twoway2$dat_result |>
          distinct(weight) |>
          arrange(weight)

ggplot(unique_weights2, aes(x = seq_along(weight), y = weight)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Weights from First Differences Decomposition",
    x = "Observation index",
    y = "Weight"
  )

##unsure about how to plot this. maybe it is better to add time in X... 

##______________________________________________________________________________
#Exercise 3

##NOTE: exclude always treated (the following is all wrong: exclude always treated and add never treated as F+1!!!! )->I had to do it in Stata, as R is not aking well this package. 

data_exercise3 <- data_bacon |>
  group_by(cvemun) |>
  mutate(
    treat_first = ifelse(
      any(treat == 1),
      min(quarter[treat == 1]),
      treat_first +1
    )
  ) |>
  ungroup()

  data_exercise3 <- data_exercise3 |>
  mutate(event_time = quarter - treat_first)


#this is to generate a dataset at the time dimension lvel.
es_model <- feols(
  lwage ~ i(event_time, ref = -1) | cvemun + quarter,
  cluster = ~cvemun,
  data = data_exercise3
)

iplot(
  es_model,
  main = "Event Study: Effect of Seguro Popular on log(wage)",
  xlab = "Event time (quarters relative to treatment)",
  ylab = "Estimated effect (log points)",
  ref.line = 0
)


#this does not work.
library(DIDmultiplegt)

  data_exercise3 <- data_exercise3 |>
  filter(event_time>=0 & event_time<=3)

##this does not work because we can't identify the effect of not having treatment... it is colinear with time fixed effects. Also, identification of the rest is weak... maybe we need to restrict sample to small changers:
did_multiplegt_dyn(
df,
outcome,
group,
time,
treatment,
effects = 1,
design = NULL,
normalized = FALSE,
normalized_weights = FALSE,
effects_equal = FALSE,
placebo = 0,
controls = NULL,
trends_nonparam = NULL,
trends_lin = FALSE,
continuous = NULL,
weight = NULL,
cluster = NULL,
by = NULL,
by_path = NULL,
predict_het = NULL,
date_first_switch = NULL,
same_switchers = FALSE,
same_switchers_pl = FALSE,
switchers = "",
only_never_switchers = FALSE,
ci_level = 95,
graph_off = FALSE,
save_results = NULL,
save_sample = FALSE,
less_conservative_se = FALSE,
bootstrap = NULL,
dont_drop_larger_lower = FALSE,
drop_if_d_miss_before_first_switch = FALSE,
ggplot_args = NULL
)
#em falta aplicar l'altre command. Moure a Stata? no idea, revisar millor la identificació d'això; escriure bé


#fer el gràfic

