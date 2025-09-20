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


*
