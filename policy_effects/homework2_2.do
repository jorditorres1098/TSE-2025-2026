
*Date 20/10/2025
*Author: Jordi Torres
*Purpose: use clogit and mlogit models to estimate predicted utilities and counterfactuals.


set seed  1234

use "/Users/jorditorresvallverdu/Downloads/Model_sample.dta", clear
ssc install parmest

*Some data cleaning
keep if score>=cutoff

gen time2= time^2
gen logpop2= logpop^2


//clean time for q.7 and 8
gen time_norm= time*60
gen population_norm= exp(logpop)


gen treat_extremerural = (population_norm <= 500 & time_norm >= 120)

gen treat_rural = ((population_norm <= 500 & time >= 30 & time_norm < 120) | ///
					   (population_norm > 500 & population_norm < 2000 & time_norm >= 120)) & treat_extremerural==0

gen treat_modrural = ((population_norm > 500 & population_norm < 2000 & time_norm < 120) | ///
						  (population_norm <= 500 & time_norm < 30)) & treat_extremerural==0 & treat_rural==0




***************************************************************++***************
********************+1. CLOGIT *************************************************
********************************************************************************
**Q.6


estimates use clogit_model
predict vij, xb

gen u = runiform()
gen err = -ln(-ln(u)) //gen type 1 error

gen uij= vij+err //with just one draw


preserve 

	sort id_teach id_school
	bys id_teach: egen max_uij = max(uij) //maxutil
	drop if max_uij < 0 //filter out teachers whose highest utility is lower than the outside option. N:2,474,855 observations deleted

	keep uij n_plazas score id_school id_teach treat_extremerural treat_rural treat_modrural population_norm time_norm wage
	export delimited using "model_restricted_clogit.csv", replace
	!gzip model_restricted_clogit.csv, replace //I use this as input of my algorithm. Inefficient...

*save as csv
restore


*COUNTERFACTUAL: q.8 

gen prev_wage= wage

gen wage_cf= wage
replace wage_cf=wage_cf -0.5 if treat_extremerural==1
replace wage_cf= wage_cf-0.1 if treat_rural==1
replace wage_cf= wage_cf-0.07 if treat_modrural==1

replace wage= wage_cf
 
estimates use clogit_model

*How to change wage? I need to incorporate information on the rurality and treatment status of the school. 
predict vij_cf, xb

gen uij_cf= vij_cf+err //with just one draw. same draw, though? I would say so.


preserve 

	sort id_teach id_school
	bys id_teach: egen max_uij_cf = max(uij_cf)
	drop if max_uij_cf < 0 //(1,689,156 observations deleted)

	keep uij_cf n_plazas score id_school id_teach  population_norm time_norm treat_extremerural treat_rural treat_modrural wage
	export delimited using "model_restricted_clogit_cf.csv", replace
	!gzip model_restricted_clogit_cf.csv, replace

*save as csv
restore


replace wage=prev_wage 




***************************************************************++***************
********************+2. MIXLOGIT *************************************************
********************************************************************************

estimates use mixlogit_model
predict mix_vij, xb


gen mix_uij= mix_vij+err //with just one draw


preserve 

	sort id_teach id_school
	bys id_teach: egen max_mix_uij = max(mix_uij) //maxutil
	drop if max_mix_uij < 0 //filter out teachers whose highest utility is lower than the outside option. N:2,474,855 observations deleted


	keep mix_uij n_plazas score id_school id_teach population_norm time_norm treat_extremerural treat_rural treat_modrural wage
	drop if mix_uij==. //drop missing
	export delimited using "model_restricted_mixlogit.csv", replace
	!gzip model_restricted_mixlogit.csv, replace

*save as csv
restore

**CF


replace wage= wage_cf
 
estimates use mixlogit_model

*How to change wage? I need to incorporate information on the rurality and treatment status of the school. 
predict mix_vij_cf, xb

gen mix_uij_cf= mix_vij_cf+err //with just one draw


preserve 

	sort id_teach id_school
	bys id_teach: egen max_mix_uij_cf = max(mix_uij_cf)
	drop if max_mix_uij_cf < 0 //(1,689,156 observations deleted)

	keep mix_uij_cf n_plazas score id_school id_teach  population_norm time_norm treat_extremerural treat_rural treat_modrural wage
	export delimited using "model_restricted_mixlogit_cf.csv", replace
	!gzip model_restricted_mixlogit_cf.csv, replace

	*save as csv
restore


replace wage=prev_wage 
drop prev_wage 

********************************************************************************
***EOF


