clear 
set more off
macro drop _all

/*
** Name: Rayyan Mobarak Do file.do
** Date Created: 10/10/2019 by Rayyan Mobarak (rmobarak@macalester.edu)

* Data In: [smoking.csv]

* Purpose of do-file: Performs all analysis and visualizations for Rayyan's Econometrics Project
*/

* Working Directory

cd "C:\Metrics Final Paper"

* Set Global Macros: 

global onewayoptions e(r2_o r2_b r2_w F F_f rho) append 

global twowayoptions e(r2_o r2_b r2_w F F_f rho) append drop(i.year)

* Import CSV Data

import delimited "Raw Data\smoking.csv"

* Save Raw Data 

save "Raw Data\smoking_data_raw.dta", replace

********************************************************
*** DATA CLEANING ***
********************************************************

* Label Main Variables 

// ssc install labutil2

labvars minimumage mdh uer cigc taxstr ma edu "MLSA Law" "Median Household Income" "Unemployment Rate" "Cigarette Consumption" "Cigarette Taxes" "Medicaid Coverage of Cessation" "Education"

* Panel ID Variable

egen id = group(state)

* Medicaid coverage of smoking cessation

gen MD1 = 1 if ma == "Yes"
replace MD1 = 0 if ma == "No" 

* Encode "minimumage" string variable

encode minimumage, gen(mlsa_cat)

* Generate Dummy Variable of Interest

gen MLSA_law = 0 if minimumage == "No Provision"
replace MLSA_law = 1 if MLSA_law == .

* Save Clean Data 

save "Clean Data\smoking_data_clean.dta",replace

**************************************************
*** ANALYSIS and RESULTS ***
**************************************************

use "Clean Data\smoking_data_clean.dta",clear

* Setting Panel Data

xtset id year 

****************************
* Visualizations 
****************************

**** Aggregate Kernel Density Graph : Figure 1 ****

* Calculate Means

sum cigc
local meanval = round(r(mean),0.01)

/* Calculates the average of Cigarette consumption by using the "r(mean)" scalar after the sum command. The mean is stored in local variable "meanval" */

* Plot Graph

tw (kdensity cigc, lp(dash) lc(blue)) ///
   (hist cigc, recast(scatter) mc(blue) msize(small) xaxis(1 2)), ///
   xscale(lstyle(none)) leg(off) /// 
   xline(`meanval',lcolor(blue%15)) ///
   xlab(`meanlab' "`meanval'",axis(1) noticks) ///
   xti("Packs per Capita",axis(2)) yti("Freq Density") ///
   note("Vertical Line showing Mean Smoking Rate") 

gr export "Figures\fig1.png", as(png) name("Graph") replace

**** Separate Kernel Density by MLSA: Figure 2 ****

* Calculate Means

sum cigc if MLSA_law == 1 
local law_mean = round(r(mean),0.01) 
local law_label = `law_mean' - 4.2 

sum cigc if MLSA_law == 0 
local no_law_mean = round(r(mean),0.01) 
local no_law_label = `no_law_mean' + 4.2 

/* The label variables ensures good positioning of the labels that don't collide.*/

* Plot Graph

tw (kdensity cigc if MLSA_law == 1,lp(dash) lc(green))  /// 
   (kdensity cigc if MLSA_law == 0,lp(dash) lc(red)) ///
   (hist cigc if MLSA_law == 1, xaxis(1 2) recast(scatter) mc(green) msize(small)) ///
   (hist cigc if MLSA_law == 0,recast(scatter) mc(red) msize(small)), ///
   xline(`law_mean',lc(green%15)) xline(`no_law_mean',lc(red%15)) ///
   xscale(lstyle(none)) ///
   xlab(`law_label' "`law_mean'" `no_law_label' "`no_law_mean'",axis(1) noticks) ///
   xti("Packs per Capita",axis(2)) yti("Freq Density") ///
   note("Histogram Recasted as Scatter-points") ///
   leg(order(1 "Law Active" 2 "No Law"))
   
gr export "Figures\fig2.png", as(png) name("Graph") replace

/* The graph is the same as fig1, except now separated by MLSA. */

**** State Faceted Scatterplot: Figure 3 *****

tw (scatter cigc year,ms(o)),by(state) 

gr export "Figures\fig3.png", as(png) name("Graph") replace

**** MLSA distribution over Time: Figure 4 ****

spineplot mlsa_cat year, ///
  percent ///
  xlab(4 "2007" 96 "2018",axis(2)) 
  
gr export "Figures\fig4.png", as(png) name("Graph") replace

****************************
* Regression Results 
****************************

* State Fixed Effects Regression (table 1)

cap erase "Tables\table1.doc"
cap erase "Tables\table1.txt"

local mod1 MLSA_law
local mod2 MLSA_law taxstr 
local mod3 MLSA_law taxstr uer mdh
local mod4 MLSA_law taxstr uer mdh edu 
local mod5 MLSA_law taxstr uer mdh edu MD1

forv i = 1/5 {
  xtreg cigc `mod`i'',fe
  outreg2 using "Tables\table1.doc", ctitle(Model `i') ///
							$onewayoptions 
}

* State and Time Fixed Effects Regression (table 2)

cap erase "Tables\table2.doc"
cap erase "Tables\table2.txt"

local mod1 MLSA_law i.year
local mod2 MLSA_law taxstr i.year
local mod3 MLSA_law taxstr uer mdh i.year
local mod4 MLSA_law taxstr uer mdh edu i.year
local mod5 MLSA_law taxstr uer mdh edu MD1 i.year

forv i = 1/5 {
  xtreg cigc `mod`i'',fe 
  outreg2 using "Tables\table2.doc", ctitle(Model `i') ///
							$twowayoptions
}

****************************
* Sensitivity Analysis 
****************************

* Excluding States Individually (table 3)

cap erase "Tables\table3.txt"
cap erase "Tables\table3.doc"

local excluded_states ///
      Delaware Kentucky Minnesota Massachussets
	  
foreach level in `excluded_states' {
  xtreg cigc MLSA_law taxstr uer mdh edu MD1 i.year if state != "`level'",fe
  outreg2 using "Tables\table3.doc", ctitle(No `level') ///
							$twowayoptions
}

* Excluding all States (table 3)

xtreg cigc MLSA_law taxstr uer mdh edu MD1 i.year ///
 if !inlist(state, "Delaware", "Kentucky", "Minnesota", "Massachussets"),fe
 
outreg2 using "Tables\table3.doc", ctitle(All Excluded) $twowayoptions
