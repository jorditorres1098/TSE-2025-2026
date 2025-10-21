***This is just to compute 4-5 in Stata...or should I do it in R?


use "/Users/jorditorresvallverdu/Downloads/Model_sample.dta", clear


*Some data cleaning
keep if score>=cutoff

gen time2= time^2
gen logpop2= logpop^2

compress
export delimited using "model_restricted.csv", replace
!gzip model_restricted.csv


clogit match wage distance index_ccpp time time2 logpop logpop2 i.multigrado i.bilingue i.unidocente i.frontera i.vraem, group(id_teach)
eststo m1

