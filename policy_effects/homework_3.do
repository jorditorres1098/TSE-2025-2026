use "/Users/jorditorresvallverdu/Library/Mobile Documents/com~apple~CloudDocs/tse/year2/term1/bobba/data/panel_06.dta", clear

global path_graphs "/Users/jorditorresvallverdu/Documents/GitHub/TSE-2025-2026/policy_effects/figures/"

collapse (mean) sp educ age sec_occup hwage pobtot indmarg95, by(cvemun quarter)

gen logwage= log(hwage)

reghdfe logwage sp, absorb(cvemun quarter) cluster(cvemun)

**regular panel commands in stata

xtset cvemun quarter

xtreg logwage sp i.quarter, fe i(cvemun) robust cluster(cvemun)

*xtreg logwage sp i.quarter, be i(cvemun) robust cluster(cvemun)
drop if cvemun==29005


*Question 3
gen quarter_treat=quarter if sp==1
bysort cvemun: egen first_treat = min(quarter_treat) 


gen dif = quarter - first_treat

gen g_2 = dif <= -2
gen g0 = dif == 0
gen g1 = dif == 1
gen g2 = dif >= 2


eventstudyweights g_2 g0 g1 g2, absorb(i.cvemun i.quarter) cohort(first_treat) rel_time(dif) saveweights("$path_graphs/weights")
mat list e(weights)

*Question 4
drop if cvemun==29005

did_multiplegt (dyn) logwage cvemun quarter sp  , effects(5) placebo(2) cluster(cvemun)

did_multiplegt (dyn) logwage cvemun quarter sp  , effects(5) placebo(2) cluster(cvemun) controls(educ age sec_occup)


*question 3 graph 

import excel "$path_graphs/weights.xlsx", clear firstrow
keep g_2 first_treat dif
reshape wide g_2, i(dif) j(first_treat)
graph twoway line g_2* dif, xtitle("relative time") ytitle("weight in TWFE g_2 coefficient") graphregion(fcolor(white)) scheme(sj)



