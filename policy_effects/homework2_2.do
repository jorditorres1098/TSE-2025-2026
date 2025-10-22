***This is just to compute 4-5 in Stata...or should I do it in R?


use "/Users/jorditorresvallverdu/Downloads/Model_sample.dta", clear
ssc install parmest

*Some data cleaning
keep if score>=cutoff

gen time2= time^2
gen logpop2= logpop^2

//compress
//export delimited using "model_restricted.csv", replace //use on R directly, should be faster
//!gzip model_restricted.csv



**Q.6

clogit match wage distance index_ccpp time time2 logpop logpop2 i.multigrado i.bilingue i.unidocente i.frontera i.vraem, group(id_teach)
eststo mixlogit_model
estimates save mixlogit_model, replace
parmest, saving("mixlogit_params.dta", replace)
matrix b0 = e(b)

predict vij, xb

set seed  1234
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
!gzip model_restricted_clogit.csv

*save as csv
restore


*COUNTERFACTUAL: q.8 

gen prev_wage= wage
 
*How to change wage? I need to incorporate information on the rurality and treatment status of the school. 
predict vij_cf, xb

set seed  1234
gen u_cf = runiform()
gen err_cf = -ln(-ln(u)) //gen type 1 error

gen uij_cf= vij_cf+err_cf //with just one draw








**Q.7 MIX LOGIT.
matrix bstart = b0, 0.1, 0.1

mixlogit match index_ccpp time time2 logpop logpop2 ///
         multigrado bilingue unidocente frontera vraem ///
         , group(id_teach) rand(wage distance) nrep(150)  ///
           from(bstart, skip) technique(bfgs) difficult 
		 
eststo mixlogit_model
estimates save mixlogit_model, replace
parmest, saving("mixlogit_params.dta", replace) ///estimate this on a random subset of teachers.
