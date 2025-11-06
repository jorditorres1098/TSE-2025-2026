use "/Users/jorditorresvallverdu/Library/Mobile Documents/com~apple~CloudDocs/tse/year2/term1/bobba/data/panel_06.dta", clear

collapse (mean) sp educ age sec_occup hwage pobtot indmarg95, by(cvemun quarter)

gen logwage= log(hwage)

reghdfe logwage sp, absorb(cvemun quarter) cluster(cvemun)

**regular panel commands in stata

xtset cvemun quarter

xtreg logwage sp i.quarter, fe i(cvemun) robust cluster(cvemun)

*xtreg logwage sp i.quarter, be i(cvemun) robust cluster(cvemun)
