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
reg enroll_obs progresa, vce(cluster village_id)

*Way to test SUTVA: argue about the fact that treatment is a whole village, maybe I can check if 

*Question 2:

preserve 

collapse (mean) household_edu household_income, by(village_id)


restore



*Question 3: 

bys male: reg enroll_obs i.progresa, vce(cluster village_id)
bys indig: reg enroll_obs i.progresa, vce(cluster village_id)

*Question 4





