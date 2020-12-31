clear
set more off
macro drop _all

** Name: extension_analysis.do
** Date Created: 09/22/2020 by Rayyan Mobarak (rmobarak@macalester.edu)
** Last Updated: 12/30/2020

* Data In: [analysis_raw.dta]
* Data Out: [analysis_clean.dta]

* Purpose of do-file: Creates all figures and tables for the extension to the replication study.

*********************************************************** DATA URL for replication results: 
/* 

https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/DDCNEW/SXOQH8&version=1.1

All non-immigrants are dropped from the original data for this analysis.

*/ 
***********************************************************

* Organization :
/* 
*** PART I *** Data Cleaning
*** PART II *** Analysis
*/

// Setting working directory:

cd "C:\Senior Research Capstone\"

*****************************************************
*** PART I *** Data Cleaning
*****************************************************

use "Raw Data\analysis_raw.dta",clear // loading data

* PART IA * Statistics
*****************************************************

* Drop voters that were not registered anymore by the time of the election

keep if registered == 1 

/* 63 unregistered voters dropped */

* Drop immigrant observations for regions not consistent with my theory

drop if inlist(birth_region,"Eastern Europe","North America","Western Europe", "South America") 

/* 598 observations dropped from developed countries */

** Generating interaction term to reflect ATE **

* ITT ATE Variable

gen atevar = treatment*gender 
la var atevar "genderXassigntreat"

* TOT ATE Variable

gen actvar = actualtreatment*gender 
la var actvar "genderXactualtreat"

* Save clean data 

save "Clean Data\analysis_clean.dta",replace

*****************************************************
*** PART II *** Analysis
*****************************************************

****** Define sets of variables ******

* Defining control variables 

global controlsstrata = "st2 - st314" 
global controlsind = "agevote agevote2"   
global controlsbat = "pricem2 distance zus" 

/* Setting specifications of control variables for nested regressions. These are strata, individual and builidng controls respectively for the three macros. */

* Save control variables within global macros

global Mod1 
global Mod2 $controlsstrata 
global Mod3 $controlsstrata $controlsind $controlsbat 

/* Now the variables are saved within macros to create three different specifications. First model with no controls, second with Strata fixed effects and final model with strata fixed effects, individual controls and building controls */

* Setting dependent variables

global depvar2010 vote2010_1st vote2010_2nd vote2010_av 
global depvar2011 vote2011_1st vote2011_2nd vote2011_av 

/* All 2010 and 2011 dependent variables. */

* Means t test balance table : Table 2 

gen var=""
for any diff p control_mean control_sd treatment_mean treatment_sd N: gen X=.

#delimit;
local vars= 38;

for any cityind1 cityind2 cityind3 cityind4 cityind5 cityind6 cityind7 cityind8 zus pricem2 distance gender agevote origin_maghreb origin_africa origin_asia origin_otherorigins survey_attempt surveydone nodegree degreecep degreebepc degreebepcap degreebac degreebacplus2 degreehigher worker unemploy_worker student retired inactiveother spc_id2 spc_id3 spc_id4 spc_id5 spc_id6 spc_id7 spc_id8 
\ num 1/`vars': 
replace var="X" if _n==Y \ 
ivreg2 X treatment, cl(cluster) \ 
replace N=e(N) if _n==Y \ 
mat beta=e(b) \ 
replace diff=beta[1,1] if _n==Y \ 
test treatment=0 \ 
replace p=r(p) if _n==Y \ 
sum X if treatment==0 \ 
replace control_mean=r(mean) if _n==Y \ 
replace control_sd=r(sd) if _n==Y \ 
sum X if treatment==1  \ 
replace treatment_mean=r(mean) if _n==Y \ 
replace treatment_sd=r(sd) if _n==Y;
for var diff-treatment_sd: replace X=round(X, 0.001);
outsheet var control_mean control_sd treatment_mean treatment_sd diff p N if _n<=`vars' using "Extension Tables\balance_table2.xls",replace;

#delimit cr

/* Creates balance t test table to see if the treatment and control groups behave similarly in terms of observable factors. */


*** VISUALIZATIONS ***

* Install Packages
//ssc install egenmore

* Calculate Values:

sort actualtreatment gender 

/* sorts data in ascending order by treatment and then gender */

forv i= 0/1 {
	by actualtreatment gender: egen avg_vote_201`i' = mean(vote201`i'_av)
	format avg_vote_201`i' %9.3f
    by actualtreatment gender: egen se_vote_201`i' = semean(vote201`i'_av)
	gen hi_val_201`i' = avg_vote_201`i' + 1.96*se_vote_201`i'
	gen low_val_201`i' = avg_vote_201`i' - 1.96*se_vote_201`i'
}

/* The above program calculates all values needed for the visualizations. First the data is sorted by treatment and then gender. The variable "i" takes values of either 0 or 1 to signify the last digit of 2010 or 2011. This automates the variable generation for both graphs. */

* Mean bar graph for 2010 voting: Figure 1

twoway (bar avg_vote_2010 gender,barwidth(0.8)) /// 
	   (rcap low_val_2010 hi_val_2010 gender), /// 
	   xlabel( 0 "Female" 1 "Male", noticks) ///
	   ytitle("Voting 2010 Average") by(actualtreatment) || ///
	   scatter avg_vote_2010 gender, ms(none) ///
	   legend(label(1 "2010 Average of Both Rounds") ///
	   label(2 "Confidence Interval") label(3 "")) /// 
	   mla(avg_vote_2010) mlabpos(1) 
	   
graph export "Figures\fig1.png", as(png) name("Graph") replace
	   
* * Mean bar graph for 2011 voting: Figure 2

twoway (bar avg_vote_2011 gender,barwidth(0.8)) /// 
	   (rcap low_val_2011 hi_val_2011 gender), /// 
	   xlabel( 0 "Female" 1 "Male", noticks) ///
	   ytitle("Voting 2011 Average") by(actualtreatment) || ///
	   scatter avg_vote_2011 gender, ms(none) ///
	   legend(label(1 "2011 Average of Both Rounds") ///
	   label(2 "Confidence Interval") label(3 "")) /// 
	   mla(avg_vote_2011) mlabpos(1) 
	   
graph export "Figures\fig2.png", as(png) name("Graph") replace
	   
*********************************************************
* (i) REGRESSIONS (ITT): 
*********************************************************

* 2010 ITT: Table 8 

local avgtreat atevar gender treatment 

cap erase "Extension Tables\table8.doc" 
cap erase "ExtensionTables\table8.txt" 

foreach var in $depvar2010 { 
  forv i = 1/3 { 
    quietly reg `var' `avgtreat' ${Mod`i'},cl(cluster) 
    outreg2 using "Extension Tables\table8.doc", nonote se append nolabel keep(treatment atevar gender) bdec(3) 
  }
}

* 2011 ITT: Table 9 

local avgtreat atevar gender treatment

cap erase "Extension Tables\table9.doc"
cap erase "Extension Tables\table9.txt"

foreach var in $depvar2011 {
  forv i = 1/3 {
    quietly reg `var' `avgtreat' ${Mod`i'},cl(cluster)
    outreg2 using "Extension Tables\table9.doc", nonote se append nolabel keep(treatment atevar gender) bdec(3)
  }
}

/* Intent to treat regressions are done here using the original treatment assignment. This is done separately for the two rounds and average in 2010 and 2011 as dependent variables. This is then further looped over the three specification types. These are then saved in word documents with the documents being erased before hand. */

*********************************************************
* (i) REGRESSIONS (TOT): 
*********************************************************

* 2010 TOT: Table 10

local avgtreat actvar gender actualtreatment

cap erase "Extension Tables\table10.doc"
cap erase "Extension Tables\table10.txt"

foreach var in $depvar2010 {
  forv i = 1/3 {
    quietly reg `var' `avgtreat' ${Mod`i'},cl(cluster)
    outreg2 using "Extension Tables\table10.doc", nonote se append nolabel keep(actualtreatment actvar gender) bdec(3)
 }
}

* 2011 TOT : Table 11

local avgtreat actvar gender actualtreatment

cap erase "Extension Tables\table11.doc"
cap erase "Extension Tables\table11.txt"

foreach var in $depvar2011 {
  forv i = 1/3 {
    quietly reg `var' `avgtreat' ${Mod`i'},cl(cluster)
    outreg2 using "Extension Tables\table11.doc", nonote se append nolabel keep(actualtreatment actvar gender) bdec(3)
  }
}

/* Treatment on treated regressions are done here. */

*********************************************************
* (i) REGRESSIONS (LATE): 
*********************************************************

* 2010 LATE : Table 12

local avgtreat gender (actvar actualtreatment = atevar treatment)

cap erase "Extension Tables\table12.doc"
cap erase "$Extension Tables\table12.txt"

foreach var in $depvar2010 {
  forv i = 1/3 {
    quietly ivreg2 `var' `avgtreat' ${Mod`i'},cl(cluster)
    outreg2 using "Extension Tables\table12.doc", nonote se append nolabel keep(actvar actualtreatment gender) bdec(3)
  }
}

* 2011 LATE : Table 13

local avgtreat gender (actvar actualtreatment = atevar treatment)

cap erase "Extension Tables\table13.doc"
cap erase "Extension Tables\table13.txt"

foreach var in $depvar2011 {
  forv i = 1/3 {
    quietly ivreg2 `var' `avgtreat' ${Mod`i'},cl(cluster)
    outreg2 using "Extension Tables\table13.doc", nonote se append nolabel keep(actvar actualtreatment gender) bdec(3)
  }
} 

/* Two stage least squares regressions are done here with the actual treament being instrumented by original assignment. */

**********************************************************
* ROBUSTNESS CHECKS (LATE) 
*********************************************************

* Drop immigrants who are from middle east

drop if birth_region == "Middle East"

* I only use two specifications which are: strata fixed effects and strata fixed effects including building and individual controls. I also look at only the average of both rounds

* Table 14: 

* LATE estimate on 2010 average

quietly ivreg2 vote2010_av gender ${controlsstrata} (actvar actualtreatment = atevar treatment),cl(cluster)

outreg2 using "Extension Tables\table14.doc", nonote se replace nolabel keep(actvar actualtreatment gender) bdec(3)

quietly ivreg2 vote2010_av gender ${controlsstrata} ${controlsind} ${controlsbat} (actvar actualtreatment = atevar treatment),cl(cluster)

outreg2 using "Extension Tables\table14.doc", nonote se append nolabel keep(actvar actualtreatment gender) bdec(3)

* LATE estimate on 2011 average

quietly ivreg2 vote2011_av gender ${controlsstrata} (actvar actualtreatment = atevar treatment),cl(cluster)

outreg2 using "Extension Tables\table14.doc", nonote se append nolabel keep(actvar actualtreatment gender) bdec(3)

quietly ivreg2 vote2011_av gender ${controlsstrata} ${controlsind} ${controlsbat} (actvar actualtreatment = atevar treatment),cl(cluster)

outreg2 using "Extension Tables\table14.doc", nonote se append nolabel keep(actvar actualtreatment gender) bdec(3)

