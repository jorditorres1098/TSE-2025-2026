
***This is just to be able to compute the effect of no bonus. 

set seed  1234

use "/Users/jorditorresvallverdu/Downloads/Model_sample.dta", clear
ssc install parmest

*Some data cleaning
keep if score>=cutoff

gen time2= time^2
gen logpop2= logpop^2


***************************************************************++***************
********************+1. CLOGIT *************************************************
********************************************************************************
**Q.6

clogit match wage distance index_ccpp time time2 logpop logpop2 i.multigrado i.bilingue i.unidocente i.frontera i.vraem, group(id_teach)
eststo mixlogit_model
estimates save clogit_model, replace
parmest, saving("clogit_model.dta", replace)
matrix b0 = e(b) //store to optimize afterwards.


estimates use clogit_model
predict vij, xb

gen u = runiform()
gen err = -ln(-ln(u)) //gen type 1 error

gen uij= vij+err //with just one draw


preserve 

sort id_teach id_school
bys id_teach: egen max_uij = max(uij) //maxutil
drop if max_uij < 0 //filter out teachers whose highest utility is lower than the outside option. N:2,474,855 observations deleted

gsort id_teach -uij
bys id_teach: gen rank = _n //not needed actually, but...


keep uij n_plazas score id_school id_teach rank 
export delimited using "model_restricted_clogit.csv", replace
!gzip model_restricted_clogit.csv, replace //I use this as input of my algorithm.

*save as csv
restore


*COUNTERFACTUAL: q.8 

preserve 
	use "/Users/jorditorresvallverdu/Downloads/RD_sample.dta", clear //find school charact... in this dta I don't undersant the unit of measurement. Check paper!
	drop if time < 30

	gen treat_extremerural = (population <= 500 & time >= 120)

	gen treat_rural = ((population <= 500 & time >= 30 & time < 120) | ///
					   (population > 500 & population < 2000 & time >= 120)) & treat_extremerural==0

	gen treat_modrural = ((population > 500 & population < 2000 & time < 120) | ///
						  (population <= 500 & time < 30)) & treat_extremerural==0 & treat_rural==0

	collapse (mean) treat_extremerural treat_rural treat_modrural, by(id_school)

						  //sanity checks done and ommitted but this works. 
						  
	keep id_school treat_extremerural treat_rural treat_modrural
	tempfile rurality_school
	save `rurality_school'
restore


merge m:1 id_school using `rurality_school', keepusing(treat_extremerural treat_rural treat_modrural) nogen


gen prev_wage= wage

gen wage_cf= wage
replace wage_cf=wage_cf -0.5 if treat_extremerural==1
replace wage_cf= wage_cf-0.1 if treat_rural==1
replace wage_cf= wage_cf-0.07 if treat_modrural==1

replace wage= wage_cf
 
estimates use clogit_model

*How to change wage? I need to incorporate information on the rurality and treatment status of the school. 
predict vij_cf, xb

gen uij_cf= vij_cf+err //with just one draw. same draw, though?


preserve 

sort id_teach id_school
bys id_teach: egen max_uij_cf = max(uij_cf)
drop if max_uij_cf < 0 //(1,689,156 observations deleted)

gsort id_teach -uij_cf
bys id_teach: gen rank = _n //not needed actually, but...


keep uij_cf n_plazas score id_school id_teach rank 
export delimited using "model_restricted_clogit_cf.csv", replace
!gzip model_restricted_clogit_cf.csv

*save as csv
restore


replace wage=prev_wage 
drop prev_wage 




***************************************************************++***************
********************+2. MIXLOGIT *************************************************
********************************************************************************

*Run it on a subset of random teachers, as it takes ages

bys id_teach: gen double id_random = runiform()
gen byte sample= (id_random<0.10)

matrix bstart = b0, 0.1, 0.1

mixlogit match index_ccpp time time2 logpop logpop2 ///
         multigrado bilingue unidocente frontera vraem ///
         if sample==1, group(id_teach) rand(wage distance) nrep(150)  ///
           from(bstart, skip) technique(bfgs) difficult 
		 
eststo mixlogit_model
estimates save mixlogit_model, replace

estimates use mixlogit_model
parmest, saving("mixlogit_params.dta", replace) ///estimate this on a random subset of teachers.



estimates use mixlogit_model
predict mix_vij, xb


gen mix_uij= mix_vij+err //with just one draw


preserve 

sort id_teach id_school
bys id_teach: egen max_mix_uij = max(mix_uij) //maxutil
drop if max_mix_uij < 0 //filter out teachers whose highest utility is lower than the outside option. N:2,474,855 observations deleted


keep mix_uij n_plazas score id_school id_teach  
drop if mix_uij==. //drop missing
export delimited using "model_restricted_mixlogit.csv", replace
!gzip model_restricted_mixlogit.csv

*save as csv
restore

**CF

gen wage_cf= wage
replace wage_cf=wage_cf -0.5 if treat_extremerural==1
replace wage_cf= wage_cf-0.1 if treat_rural==1
replace wage_cf= wage_cf-0.07 if treat_modrural==1

replace wage= wage_cf
 
estimates use mixlogit_model

*How to change wage? I need to incorporate information on the rurality and treatment status of the school. 
predict mix_vij_cf, xb

gen mix_uij_cf= mix_vij_cf+err //with just one draw


preserve 

sort id_teach id_school
bys id_teach: egen max_mix_uij_cf = max(mix_uij_cf)
drop if max_mix_uij_cf < 0 //(1,689,156 observations deleted)

gsort id_teach -mix_uij_cf
bys id_teach: gen rank = _n //not needed actually, but...


keep mix_uij_cf n_plazas score id_school id_teach rank 
export delimited using "model_restricted_mixlogit_cf.csv", replace
!gzip model_restricted_mixlogit_cf.csv

*save as csv
restore


replace wage=prev_wage 
drop prev_wage 

********************************************************************************
***EOF


