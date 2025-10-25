**


use "/Users/jorditorresvallverdu/Downloads/Model_sample.dta", clear
ssc install parmest

*Some data cleaning
keep if score>=cutoff

gen time2= time^2
gen logpop2= logpop^2

gen time


***************************************************************++***************
********************+1. CLOGIT:estimation **************************************
********************************************************************************

clogit match wage distance index_ccpp time time2 logpop logpop2 i.multigrado i.bilingue i.unidocente i.frontera i.vraem, group(id_teach)
eststo mixlogit_model
estimates save clogit_model, replace
parmest, saving("clogit_model.dta", replace)
matrix b0 = e(b) //store to optimize afterwards.


***************************************************************++***************
********************+2. MIXLOGIT:estimation ************************************
********************************************************************************

bys id_teach: gen double id_random = runiform()
gen byte sample= (id_random<0.10)

matrix bstart = b0, 0.1, 0.1

mixlogit match index_ccpp time time2 logpop logpop2 ///
         multigrado bilingue unidocente frontera vraem ///
         if sample==1, group(id_teach) rand(wage distance) nrep(150)  ///
           from(bstart, skip) technique(bfgs) difficult 
		 
eststo mixlogit_model
estimates save mixlogit_model, replace
parmest, saving("mixlogit_params.dta", replace) ///estimate this on a random subset of teachers.

********************************************************************************
***EOF
