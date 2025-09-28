/*
Date 19/09/2025
Author: Jordi Torres Vallverdú //Carlos Àlvarez
Purpose: Homework Policy evaluation
*/


*********************************************************************************
*********************************************************************************

*Define relative paths:

global local_path "/Users/jorditorresvallverdu/Library/Mobile Documents/com~apple~CloudDocs/tse/year2/term1/bobba/data"

global outtex "${path}\LATEX TABLES"

*1. Run the dataset

use "$local_path/progresa-sample", clear



*Labeling variables
label variable progresa              "Progresa beneficiary"
label variable male                  "Male child "
label variable indig                 "Indigenous household"
label variable distance_secschool    "Distance to school"
label variable enroll                "Enrolled"
label variable grade_enr             "Grade enrolled in"
label variable household_size        "Household size"
label variable distance_provcapital  "Distance to provincial capital (km)"
label variable household_edu         "Household education"
label variable household_income      "Household income"
label variable age                   "Age of child"
label variable village_id            "Village identifier"
label variable household_id          "Household identifier"


********************************************************************************
//Question 1: 

*Conditional expectation:

bys progresa: sum age household_income household_edu distance_provcapital household_size grade_enr enroll distance_secschool indig male

*Plus statistical test of difference in means. There should be a package that does this directly...

foreach v in age household_income household_edu distance_provcapital household_size grade_enr enroll distance_secschool indig male{
	reg `v' i.progresa, vce(cluster village_id) //maybe no need to cluster at this dimension.
}
*

*Check for attrition being random...
gen enroll_obs = !missing(enroll)
reg enroll_obs i.progresa, vce(cluster village_id)

*Way to test SUTVA: argue about the fact that treatment is a whole village, maybe I can check if 



//Question 3: 

* Gender
reg enroll progresa if male == 1, vce(cluster village_id)
eststo reg_male_base
reg enroll progresa age distance_secschool household_edu household_income ///
if male == 1, vce(cluster village_id)
eststo reg_male_full

reg enroll progresa if male == 0, vce(cluster village_id)
eststo reg_female_base
reg enroll progresa age distance_secschool household_edu household_income ///
if male == 0, vce(cluster village_id)
eststo reg_female_full

* Export LaTeX table
esttab reg_male_base reg_male_full reg_female_base reg_female_full /// 
	using "${outtex}\q3gender.tex", ///
    se label replace ///
    booktabs title("Impact of Progresa on School Enrollment (by Gender) \label{tab:q3gender}") ///
    mtitles("\begin{tabular}{@{}c@{}}Males: \\ Baseline\end{tabular}" ///
            "\begin{tabular}{@{}c@{}}Males: \\ Full\end{tabular}" ///
            "\begin{tabular}{@{}c@{}}Females: \\ Baseline\end{tabular}" ///
            "\begin{tabular}{@{}c@{}}Females: \\ Full\end{tabular}") 

* Indigenous
reg enroll progresa if indig == 0, vce(cluster village_id)
eststo reg_noindig_base
reg enroll progresa age distance_secschool household_edu household_income ///
if indig == 0, vce(cluster village_id)
eststo reg_noindig_full

reg enroll progresa if indig == 1, vce(cluster village_id)
eststo reg_indig_base
reg enroll progresa age distance_secschool household_edu household_income ///
if indig == 1, vce(cluster village_id)
eststo reg_indig_full

* Export LaTeX table
esttab reg_noindig_base reg_noindig_full reg_indig_base reg_indig_full ///
using "${outtex}\q3indig.tex", ///
    se label replace ///
    booktabs ///
	title("Impact of Progresa on School Enrollment (by Indigenous status) \label{tab:q3indig})" ) ///
    mtitles("\begin{tabular}{@{}c@{}}Not Indigenous: \\ Baseline\end{tabular}" ///
            "\begin{tabular}{@{}c@{}}Not Indigenous: \\ Full\end{tabular}" ///
            "\begin{tabular}{@{}c@{}}Indigenous: \\ Baseline\end{tabular}" ///
            "\begin{tabular}{@{}c@{}}Indigenous: \\ Full\end{tabular}") 
 




//Question 4 //using ritest


ritest progresa _b[1.progresa], reps(1000) cluster(village_id): ///
    reg enroll i.progresa if male==0, vce(cluster village_id)

ritest progresa _b[1.progresa], reps(1000) cluster(village_id): ///
    reg enroll i.progresa if male==1, vce(cluster village_id)

ritest progresa _b[1.progresa], reps(1000) cluster(village_id): ///
    reg enroll i.progresa if indig==0, vce(cluster village_id)

ritest progresa _b[1.progresa], reps(1000) cluster(village_id): ///
    reg enroll i.progresa if indig==1, vce(cluster village_id)


	
*Question 4: own coded, trying to learn...
reg enroll i.progresa if male==0, vce(cluster village_id)
scalar beta_obs = _b[1.progresa]

local reps = 1000

matrix betas = J(`reps',1,.)
 
forvalues r = 1/`reps' {
    preserve
        collapse (mean) progresa, by(village_id)

        quietly summarize progresa
        scalar total_t = r(sum)

        gen u = runiform()
        gsort -u
        gen treat_cf_`r' = (_n <= total_t) //i think this is wrong, cause I am not permutating, I allow repetition, or de facto no due to randomness?

        tempfile village_random
        save `village_random'
    restore
    merge m:1 village_id using `village_random', keepusing(treat_cf_`r') nogen

    quietly reg enroll i.treat_cf_`r' if male==0, vce(cluster village_id)
    matrix betas[`r',1] = _b[1.treat_cf_`r']
}

preserve 
clear
svmat betas, names(col)
rename c1 betas1
gen abs_beta = abs(betas1)
scalar abs_obs = abs(beta_obs)

gen extreme = (abs_beta >= abs_obs)
sum extreme
display "Fisher p-value = " r(mean)
	
restore

		
//Question 5

sort village_id household_income
bys village_id: egen lwage = pctile(household_income), p(10)

label variable lwage "Child wage"

probit enroll household_income lwage if progresa == 0, vce(cluster village_id)
eststo probit_wage

esttab probit_wage ///
    using "${outtex}\q5_probit.tex", ///
    se label replace ///
    booktabs ///
    title("School Enrollment in Control Villages") ///
    star(* 0.05 ** 0.01 *** 0.001)




//Question 6 

gen tau=150+70 if  (grade_enr==3 ) //here we dont condition on whether progresa was applied, we suppose what would happen to this agents if they receive a boost...
replace tau=150+80 if (grade_enr==4 )
replace tau=150+105 if  (grade_enr==5 )                                                                                                    
replace tau=150+135 if  (grade_enr==6 )                                                                                                    
replace tau=150+210 if  (grade_enr==7  & male==0)                                                                                                    
replace tau=150+200 if  (grade_enr==7  & male==1)                                                                                                    
replace tau=150+235 if  (grade_enr==8  & male==0)                                                                                                    
replace tau=150+210 if  (grade_enr==8  & male==1)   
replace tau=150+255 if  (grade_enr==9  & male==0)                                                                                                    
replace tau=150+235 if  (grade_enr==9  & male==1)   


probit enroll household_income lwage if progresa==0 , vce(cluster village_id)
predict u_enroll_nt if enroll!=., xb //we predict model for all! but those that we don't use in estimation.

scalar beta_hat= _b[household_income]
scalar omega_hat= _b[lwage] 
display beta_hat
display omega_hat


gen u_enroll_t= u_enroll_nt+ (beta_hat - omega_hat)*tau if enroll!=. //REVISE


gen prob_enroll_ctf_t=normal(u_enroll_t) if  enroll!=.
gen prob_enroll_ctf_base= normal(u_enroll_nt) if  enroll!=.

gen dif_prob=prob_enroll_ctf_t-prob_enroll_ctf_base



preserve 

drop if enroll==.
collapse (mean) dif_prob, by(indig)

restore

preserve 

drop if enroll==.
collapse (mean) dif_prob, by(male)

restore



//Question 7

*Make assignment 
gen tau2= progresa*tau


*Estimate model where there is disutility of work and utility of the subsidy. 

probit enroll tau2 household_income lwage i.grade_enr, vce(cluster village_id) //unsure whether to control for grade fixed effects
gen tau2_orig=tau2
replace tau2=0 if tau2!=.

predict p_nosubsidy, pr

replace tau2=tau if tau2!=.

predict p_subsidy, pr


replace tau2= tau2_orig
drop tau2_orig

gen average_7= p_subsidy- p_nosubsidy

bys male: sum average_7
bys indig: sum average_7


*Question 8

*Nonlinear profild:
gen tau_new_unscaled2 = 0 if grade_enr==3
replace tau_new_unscaled2 = 0 if grade_enr==4
replace tau_new_unscaled2 = 80  if grade_enr==5
replace tau_new_unscaled2 = 150 if grade_enr==6
replace tau_new_unscaled2 = 350 if grade_enr==7 & male==0
replace tau_new_unscaled2 = 330 if grade_enr==7 & male==1
replace tau_new_unscaled2 = 450 if grade_enr==8 & male==0
replace tau_new_unscaled2 = 420 if grade_enr==8 & male==1
replace tau_new_unscaled2 = 600 if grade_enr==9 & male==0
replace tau_new_unscaled2 = 550 if grade_enr==9 & male==1

sum tau, meanonly
scalar mean_orig = r(mean)

sum tau_new_unscaled2, meanonly
scalar mean_new = r(mean)

scalar scale = mean_orig / mean_new

gen tau_new = tau_new_unscaled2 * scale //just to ensure budget balance

//Plot
preserve 

keep if progresa==0
collapse (mean) enroll tau tau_new (sem) std_enroll=enroll , by(age)

gen upperb = enroll + 1.96*std_enroll
gen lowerb = enroll - 1.96*std_enroll

twoway ///
(rarea upperb lowerb age, lcolor(gs12)) /// 
(line enroll age, lcolor(blue) lwidth(medthick)) ///
(line tau age, lcolor(red) yaxis(2)) ///
(line tau_new age, lcolor(orange) yaxis(2)) ///
, ytitle("Average Enrollment", axis(1)) ///
ytitle("Subsidy Amount", axis(2)) ///
xtitle("Age") ///
legend(off) 

restore



*Conlcusion: make sure to start subsidizing more from age 13 and in an increasing manner -nonlinearly...Maybe invert this function... that would be the ideal scheme...


*Generate simulations, under 6:
gen u_enroll_t_7= u_enroll_nt+ (beta_hat - omega_hat)*tau_new if enroll!=.

gen prob_enroll_ctf_t7=normal(u_enroll_t_7) if  enroll!=.

gen dif_prob_t7=prob_enroll_ctf_t7-prob_enroll_ctf_t


bys male: sum dif_prob_t7
bys indig: sum dif_prob_t7


*Under 7, more easily:

probit enroll tau2 household_income lwage i.grade_enr, vce(cluster village_id)

gen tau2_orig=tau2
replace tau2=tau_new if tau2!=.

predict p_subsidy2, pr

replace tau2=tau2_orig if tau2!=.
drop tau2_orig

gen dif_prob_t8=p_subsidy2-p_subsidy


bys male: sum dif_prob_t8
bys indig: sum dif_prob_t8
 *******************************************************************************EOF



