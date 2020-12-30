clear 
set more off 

/*
** Name: project.do
** Date Created: 12/2/2020 by Rayyan Mobarak (rmobarak@macalester.edu)

* Data In: [metrics.dta]

* Purpose of do-file: Performs wide range of visualizations using Rayyan's metrics data
*/


* Working Directory (set directory here)

cd "C:\Fall 2020\Intro to Data Science\Stata Visualizations"

****************************************************
*** Data Preparation *** 
****************************************************

* Load Metrics Data: 

use "Raw Data\metrics.dta"

* Exporting to CSV: 

export delimited "Raw Data\smoking.csv", nolab replace

/* This saves into a CSV file if one wishes to load to a different software.  */

* General look at all variables

describe

* Generating Policy Indicator

gen min_age = 0 if minimumage == "No Provision"
replace min_age = 1 if min_age == .

/*  Dummy indicating 1 for the presence of a law, and 0 otherwise */

* Save clean data

save "Clean Data\metrics_clean.dta",replace

*****************************************************
*** Visualizations *** 
***************************************************** 

/* NOTE: Run the sum and/or local commands together the code for each graph that uses such defined variables. Otherwise the local variables will not be read within the parameters of the graph. */

***********************************************
** Aggregate Kernel Density Graph : fig1 **
***********************************************

* Calculate Means
sum CigC
local meanval = round(r(mean),0.01)

/* Calculates the average of Cigarette consumption by using the "r(mean)" scalar after the sum command. Use "return list" after the "sum" command to see list of scalars. The mean is stored in local variable "meanval" */

* Plot Graph

tw (kdensity CigC, lp(dash) lc(blue)) ///
   (hist CigC, recast(scatter) mc(blue) msize(small) xaxis(1 2)), ///
   xscale(lstyle(none)) leg(off) /// 
   xline(`meanval',lcolor(blue%15)) ///
   xlab(`meanlab' "`meanval'",axis(1) noticks) ///
   ti("Distribution of Cigarette Consumption") ///
   xti("Packs per Capita",axis(2)) yti("Freq Density") ///
   note("Vertical Line showing Mean Smoking Rate") 

gr export "Visualizations\fig1.png", as(png) name("Graph") replace

/* This graph shows the distribution of Cigarette Consumption within the data. The low opacity vertical line signifies the mean of cigarette consumption which has been stored in a local variable. The histogram has been recasted as a scatter plot to look more appealing visually. */

*********************************************
** Separate Kernel Density by MLSA: fig2 **
*********************************************

* Calculate Means

sum CigC if min_age == 1 
local law_mean = round(r(mean),0.01) 
local law_label = `law_mean' - 4.2 

sum CigC if min_age == 0 
local no_law_mean = round(r(mean),0.01) 
local no_law_label = `no_law_mean' + 4.2 

/* The label variables ensures good positioning of the labels that don't collide.*/

* Plot Graph

tw (kdensity CigC if min_age == 1,lp(dash) lc(green))  /// 
   (kdensity CigC if min_age == 0,lp(dash) lc(red)) ///
   (hist CigC if min_age == 1, xaxis(1 2) recast(scatter) mc(green) msize(small)) ///
   (hist CigC if min_age == 0,recast(scatter) mc(red) msize(small)), ///
   xline(`law_mean',lc(green%15)) xline(`no_law_mean',lc(red%15)) ///
   xscale(lstyle(none)) ///
   xlab(`law_label' "`law_mean'" `no_law_label' "`no_law_mean'",axis(1) noticks) ///
   ti(Distribution of Cigarette Consumption by MLSA) ///
   xti("Packs per Capita",axis(2)) yti("Freq Density") ///
   note("Histogram Recasted as Scatter-points") ///
   leg(order(1 "Law Active" 2 "No Law"))
   
gr export "Visualizations\fig2.png", as(png) name("Graph") replace

/* The graph is the same as fig1, except now separated by MLSA. */

****************************************
** State Faceted Scatterplot: fig3 **
****************************************

tw (scatter CigC year,ms(o)),by(state) 

gr export "Visualizations\fig3.png", as(png) name("Graph") replace

/* Cigarette Consumption trend for each State in the data. */

*********************************************************
** Mean bar graph by MLSA with Confidence Level: fig4 **
*********************************************************

// ssc install egenmore

/* This package extends the "egen" command. We will use the standard error calculation from this package. */

* Calculations:

bysort min_age: egen avg_cig_mlsa = mean(CigC)  
by min_age: egen se_cig_mlsa = semean(CigC)
format avg_cig_mlsa %9.2f

/* First we sort by minimum age and then by "min_age" variable, calculate mean of cigarette consumption. The same is done for the standard errors. The average values are then formatted to two decimal places to be used as marker labels in the plot.  */

gen hi_ci_val = avg_cig_mlsa + 1.96*se_cig_mlsa
gen low_ci_val = avg_cig_mlsa - 1.96*se_cig_mlsa

/* The low and high values for the 95% confidence intervals are calculated by subtracting and adding 1.96 times the standard errors to mean values respectively. */

tw (bar avg_cig_mlsa min_age,barwidth(0.8)) ///
   (rcap low_ci_val hi_ci_val min_age), ///
   yscale(r(0 80)) ylab(0 (20) 80,format(%9.0f)) ///
   yti("Average Cigarette Consumption") /// 
   xlabel(0 "No Law" 1 "Law Active") xti("") ///
   ti("Mean Smoking Rate by MLSA Law") ///
|| ///
   scatter avg_cig_mlsa min_age, ///
   ms(none) mlab(avg_cig_mlsa) mlabpos(2) ///
   leg(order(1 "Avg Smoking" 2 "95% Confidence Interval"))
   
gr export "Visualizations\fig4.png", as(png) name("Graph") replace

/* Average cigarette consumption by MLSA law. "rcap" lets us plot the calculated confidence intervals. */

**********************************************
** Mean Line Plot by MLSA throughout time: fig5 ** 
**********************************************

* Calculate Mean

bysort year min_age: egen avg_cig_points = mean(CigC)

tw (scatter avg_cig_points year if min_age == 0, mcolor(red) msymb(o)) ///
   (scatter avg_cig_points year if min_age == 1, mcolor(midblue) msymb(o)) ///
   (line avg_cig_points year if min_age == 0, lcolor(maroon) lpattern(_)) ///
   (line avg_cig_points year if min_age == 1, lcolor(blue) lpattern(_)), ///
   legend(order(1 "No Law" 2 "Law Active")) ///
   ti("Trend in Mean Smoking Rate by MLSA") ///
   yti("Mean Smoking Rate")
   
gr export "Visualizations\fig5.png", as(png) name("Graph") replace

/* Average cigarette consumption by MLSA law over time.*/

*********************************************
** Mean by MLSA by Income Groups: fig6 ** 
*********************************************

* Create Income Quantiles
xtile income_group = MDH,nq(3)

* Save clean data
save "Clean Data\metrics_clean.dta",replace

/* This creates three quantiles of income_group namely low, middle and high within the variable "income_group". */

forv i = 1/3 {
  sum MDH if income_group == `i'
}

/* Summarizes median household income for the three values of income_group. The loop creates an index "i" which takes the values of income_group. Low: $34916 to $54691. Middle: $54716 to $ 62742. High: $62779 to $86345. */

* The Plot

gr bar CigC, over(min_age) asyvars ///
   over(income_group,relabel(1 "Low" 2 "Middle" 3 "High")) ///
   blabel(total,format(%9.2f)) ///
   b1ti("Income Groups") ///
   yti("Mean Smoking Rates") ///
   ti("Mean Smoking Rate by MLSA and Income Groups") ///
   text(70 60 "Low: $34916 to $54691" "Middle: $54716 to $ 62742" "High: $62779 to $86345", color(green) size(small) place(ne) box just(center) height(10)) ///
   leg(order(1 "No Law" 2 "Law Active"))
   
gr export "Visualizations\fig6.png", as(png) name("Graph") replace

/* Plots average cigarette consumption by MLSA law across the three generated income groups. */

*****************************************************
** Income group distribution by MLSA: fig7,fig8 **
*****************************************************

* Encode "minimumage" string variable

encode minimumage, gen(mlsa_cat)

/* Assigns each category of "minimumage" a number. This is done since the following graphs do not take string variables. */

* Save clean data

save "Clean Data\metrics_clean.dta",replace

* The Category Plot: 

// ssc install catplot

catplot income_group mlsa_cat, ///
   asyvars stack percent(mlsa_cat) ///
   recast(bar) ///
   leg(order(1 "Low" 2 "Middle" 3 "High")) ///
   subtitle("Percentage of Income Group Observations by MLSA Category") 
   
gr export "Visualizations\fig7.png", as(png) name("Graph") replace

/* Plot showing how the defined income groups are distributed across minimumage laws in percentage terms. Withouth the "recast(bar)" option, Stata defaults to a horizontal bar graph.*/

* The Tab Plot: 

// ssc install tabplot

tabplot income_group mlsa_cat, ///
  percent(mlsa_cat) showval ///
  ti("Percentage of Income Group Observations by MLSA Category",size(medium)) bcolor(red) subti("") ///
  bfcolor(blue%15) ///
  ylab(1 "High" 2 "Middle" 3 "Low") frame(100) 
  
gr export "Visualizations\fig8.png", as(png) name("Graph") replace

/* Another way of plotting figure 7, except now separate bars are created for each category, representing the percentage of its observations from the total observations */

*********************************************
** MLSA distribution over Time: fig9 **
*********************************************

// ssc install spineplot

* The Spine(Mosaic) Plot: 

spineplot mlsa_cat year, ///
  percent ///
  xlab(4 "2007" 96 "2018",axis(2)) 
  
gr export "Visualizations\fig9.png", as(png) name("Graph") replace

/* This plot shows the trend in MLSA observations over time in percentages. As a certain color dominates each bar, the more observations it gets attributed to itself. */

******************************************
** Trend in smoking by MLSA: fig10 **
******************************************

* Scatter Plot: 

tw (scatter CigC year if min_age == 0,mcol(orange%15) msymb(o)) ///
   (scatter CigC year if min_age == 1,mcol(blue%15) msymb(o)) ///
   (lfit CigC year if min_age == 0,lcol(orange)) ///
   (lfit CigC year if min_age == 1,lcol(blue)), ///
   leg(order(3 "No Law" 4 "Law Active")) ///
   ti("Trend in Smoking Rates by MLSA") yti("Cig Cons")
   
gr export "Visualizations\fig10.png", as(png) name("Graph") replace

****************************************** 
** Regression Results of MLSA: fig11 ** 
******************************************

use "Clean Data\metrics_clean.dta",clear
* Coefficient Plot

* Set Panel Data

egen id = group(state) // State identifier

sort id // Ascending order by id

* Save Data
save "Clean Data\metrics_clean.dta",replace

xtset id year // Declare Panel dataset.

* Regression analysis

local Mod1 i.year
local Mod2 taxstr i.year 
local Mod3 uer MDH taxstr i.year
local Mod4 uer MDH taxstr Edu i.year

cap erase "Tables\Regression_Output.doc"
cap erase "Tables\Regression_Output.txt"

est drop _all

forv i = 1/4 {
  xtreg CigC min_age `Mod`i'',fe cluster(id)
  outreg2 using "Tables\Regression_Output.doc", ///
          append e(r2_o r2_b r2_w F F_f rho)
  est stor m`i'
}


/* The set of local variables are the specifications of the models. */

/* Regression tables are erased before-hand so that multiple runs of the code can continue to replace the outreg2 tables. "cap" ensures Stata does not display an error the very first time the code is run.  */

/* A two-way fixed effects regression is performed with the y variable being "CigC" and the variable of interest being "min_age". The loop indexes the local variable models based on the last digit within the variable name. Regression results are saved using "outreg2". The estimates are then stored in four variables ranging from "m1" to "m4" for the coefficient plot. */

* The Plot: 

coefplot m1 m2 m3 m4, keep(min_age) ///
  xline(0, lp(dash)) ///
  ti("Coefficient Plot of MLSA Variable of Interest") ///
  mlabel format(%9.3f) mlabpos(1) msymbol(D) ///
  plotlabels("No Controls" "Taxes" "Tax,UER,MDH" "Tax,UER,MDH,Edu") ///
  note("Legend signifying control variables")
  
gr export "Visualizations\fig11.png", as(png) name("Graph") replace

/* Plots the estimates of "min_age", our variable of interest across all specifications. The confidence bands show the significance levels based on the dashed vertical line at x = 0. */

*****************************************************
** Map showing US MLSA laws in 2018: fig12 ** 
*****************************************************

* Required Packages

// ssc install shp2dta 
// ssc install spmap 
// ssc install mif2dta

* Convert Shapefiles to Stata Format:

shp2dta using "Raw Data\cb_2018_us_state_500k", database("Raw Data\usdb") coordinates("Raw Data\uscoord") replace 

/* Creates two Stata datasets "usdb" and "uscoord" with longitudinal and coordinate data respectively for US States. */

* Reshape Data:

use "Clean Data\metrics_clean.dta",clear

local varlist minimumage MDH uer taxstr MA Edu min_age income_group CigC mlsa_cat state

reshape wide `varlist', i(id) j(year)

/* Data is reshaped to wide to form a cross section with each original variable being divided into separate ones for each year. This allows us to construct maps for any point in time and also allows us to take advantage of animated maps. */

* Save Cross Sectional Data

save "Clean Data\metrics_cross_section.dta",replace

* Drop States not in Main data from Shape data: 

use "Raw Data\usdb",clear

drop if inlist(NAME,"American Samoa","Commonwealth of the Northern Mariana Islands","Guam","United States Virgin Islands","Puerto Rico")

sort NAME

gen id = _n

/* "NAME" represents name of states. This is sorted and then the id variable is generated for an accurate 1:1 merge with the main data. */

* Merge both data
merge 1:1 id using "Clean Data\metrics_cross_section.dta"

* The Map 

spmap mlsa_cat2018 using "Raw Data\uscoord" if state2018 != "Alaska" & state2018 != "Hawaii", ///
      id(_ID) fcolor(Pastel1) clmethod(unique) ///
	  legend(region(color(white))) ///
	  ti("MLSA Laws in the US as of 2018") ///
	  plotregion(icolor(bluishgray)) ///
	  note("Source: Shapefiles from US Census Bureau") 
	  
gr export "Visualizations\fig12.png", as(png) name("Graph") replace
	  
/* The attribute variable is categorical which is why "clmethod(unique)" is specified. Alaska and Hawaii are excluded to "zoom" into the US. */

*****************************************************
** Animated Map of US Cigarette Consumption: fig13 ** 
*****************************************************

* Guide to install "ffmpeg" software

/* 

1) Go to following link and download any set of zip files for appropriate operating system: https://ffmpeg.org/download.html#build-windows 

2) Extract the bin folder containing the ffmpeg executable file (ffmpeg.exe) to computer directory

3) Type in "path" in start menu and select "Edit System Environment Variables". Go to "System variables" section, select "Path" and click on "edit" 

4) Add file path to folder where "ffmpeg.exe" has been extracted to the computer and click "ok".

*/

* File path to "ffmpeg" software (set directory here)

global video_path "C:\FFMEG\ffmpeg.exe" 

* Generate Maps 

forv i = 2007/2018 {
     spmap CigC`i' using "Raw Data\uscoord" if state`i' != "Alaska" & state`i' != "Hawaii", ///
      id(_ID) fcolor(Heat) ///
	  clnumber(5) clmethod(custom) ///
	  clbreaks(0 20 50 75 100 150) ///
      legend(region(color(white))) ///
      ti("Cigarette Consumption on `i'") ///
	  plotregion(icolor(bluishgray)) ///
	  note("Source: Shapefiles from US Census Bureau")
  local suffix = string(`i'-2007)
  gr export "Animate\Cig_map`suffix'.png", as(png) name("Graph") replace
}

/* This creates a map for cigarette consumption with a separate map made for each year. The maps are then exported as PNG. To make the video creation easier it has been named in a sequential numeric way from 0 to 11 for years 2007 to 2018. */

* Create Video: 

!del "Animate\map.mpg"
winexec $video_path -i "Animate\Cig_map%d.png" -b:v 512k "Animate\map.mpg" 

/* Runs "ffmpeg" to create a video of all the maps we have created. The "%d" tells "ffmpeg" to look for a images named with single digit sequence between "map" and ".png". The "!del" command deletes the video beforehand for repeat reruns of the code. */

* Create GIF: 

!del "Visualizations\fig13.gif" 
winexec $video_path -r 2 -i "Animate\map.mpg" -t 10 -r 10 "Visualizations\fig13.gif" 

/* This converts the created video into a GIF. */





