/*
Date 19/09/2025
Author: Jordi Torres Vallverdú
Purpose: Homework Policy evaluation
*/


*********************************************************************************
*********************************************************************************

*Define relative paths:

global local_path "/Users/jorditorresvallverdu/Library/Mobile Documents/com~apple~CloudDocs/tse/year2/term1/bobba/data"




*1. Run the dataset

use "$local_path/progresa-sample", clear

*Question 1: 

*Conditional expectation:

bys progresa: sum age household_income household_edu distance_provcapital household_size grade_enr enroll distance_secschool indig male

*Plus statistical test of difference in means. There should be a package that does this directly...

foreach v in age household_income household_edu distance_provcapital household_size grade_enr enroll distance_secschool indig male{
	reg `v' i.progresa, vce(cluster village_id)
}
*

*Check for attrition being random...
gen enroll_obs = !missing(enroll)
reg enroll_obs i.progresa, vce(cluster village_id)

*Way to test SUTVA: argue about the fact that treatment is a whole village, maybe I can check if 

*Question 2:

preserve 

collapse (mean) household_edu household_income, by(village_id)


restore



*Question 3: 

bys male: reg enroll i.progresa, vce(cluster village_id)
bys indig: reg enroll i.progresa, vce(cluster village_id)

*Question 4

*Question 5


preserve 
	collapse (p10) lwage=household_income, by(village_id)
	tempfile lwage
	save `lwage'
restore

merge m:1 village_id using `lwage', gen(m1)


probit enroll household_income lwage if progresa==0


*Question 6 //no kids in multiples h.h
gen tau=150
replace tau=150+70 if  (grade_enr==3 ) //here we dont condition on whether progresa was applied, we suppose what would happen to this agents if they receive a boost...
replace tau=150+80 if (grade_enr==4 )
replace tau=150+105 if  (grade_enr==5 )                                                                                                    
replace tau=150+135 if  (grade_enr==6 )                                                                                                    
replace tau=150+210 if  (grade_enr==7  & male==0)                                                                                                    
replace tau=150+200 if  (grade_enr==7  & male==1)                                                                                                    
replace tau=150+235 if  (grade_enr==8  & male==0)                                                                                                    
replace tau=150+210 if  (grade_enr==8  & male==1)   
replace tau=150+255 if  (grade_enr==9  & male==0)                                                                                                    
replace tau=150+235 if  (grade_enr==9  & male==1)   


probit enroll household_income lwage if progresa==0 
predict u_enroll_nt if enroll!=., xb //we predict model for all! but those that we don't use in estimation.

scalar beta_hat= _b[household_income]
scalar omega_hat= _b[lwage] //not sure I have understood this properly, maybe if we do 1+ beta is enough?
display beta_hat
display omega_hat


//gen u_enroll_t= u_enroll_nt+ (beta_hat + omega_hat)*tau if progresa==1 & enroll!=. //revise this; not correct because we want to see how the probability would change if we give the subsidy to everybody

gen u_enroll_t= u_enroll_nt+ (-beta_hat + omega_hat)*tau if enroll!=.


/*
gen prob_enroll_ct=normal(u_enroll_t) if progresa==1 & enroll!=.
replace prob_enroll_ct= normal(u_enroll_nt) if progresa==0 & enroll!=.
*/

gen prob_enroll_ctf_t=normal(u_enroll_t) if  enroll!=.
gen prob_enroll_ctf_base= normal(u_enroll_nt) if  enroll!=.

gen dif_prob=prob_enroll_ctf_t-prob_enroll_ctf_base



preserve 

drop if enroll==.
collapse (mean) dif_prob, by(indig)

restore


*Question 7










