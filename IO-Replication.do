**************************************************************
** Replication Code for 
** Article "Hello, Goodbye: When do States Withdraw from International Organizations?"
** Inken von Borzyskowski and Felicity Vabulas
** Review of International Organizations 2019  
**
** Last Updated: March 2019

** Replication Attempt by Shreyas Meher, PhD student PPPE, The University of Texas at Dallas
** 10/15/2023
**************************************************************


// Windows 11
// Software: Stata 14 MP
// Set working directory to the location of RIO_replication data.dta and load data
cd ""
use "RIO_replication data.dta", clear



// Install if you haven't yet
ssc install estout
ssc install sutex
ssc install grstyle
ssc install egenmore
ssc install listtab
net install gr0002_3, from (http://www.stata-journal.com/software/sj4-3)   
ssc install cem 
ssc install coefplot

* install relogit.ado: 
* 	follow instructions posted at https://gking.harvard.edu/scholar_software/relogit-rare-events-logistic-regression/1-1-stata
* 	step 1: download folder/package from https://gking.harvard.edu/relogit
* 	step 2: place in correct directory as described in King's readme file: 
* 		To install RELOGIT, launch Stata and then type
*		sysdir
* 		at the command prompt.             
*    If RELOGIT is strictly for your own use, copy all files to the PLUS
*    directory listed on your screen.  For example, the directory is
*    c:\ado\plus under Windows and ~/ado/plus/ under UNIX.  
* 	 If, on the other hand, you are a site administrator who wants to share RELOGIT with
*    other users on your network, copy all files to the SITE directory.  In
*    some cases the PLUS and SITE directories may not yet exist on your
*    computer.  For those cases, please use your operating system to create
*    the directory  that will hold the files.

* install relogitll.ado 
* 	This is to get model fit statistics (AIC, BIC, LL)
* 	http://travisbraidwood.altervista.org/dataverse.html
* 	follow instructions posted at http://travisbraidwood.altervista.org/Relogitll_Instructions_&_Example.do
* 	download / copy code from http://travisbraidwood.altervista.org/relogitll.ado // This is the main file that we need to install, the two ado directories are different. 
* 	put relogitll.ado in the folder "Stata/ado/base/r/"

* download latex at https://www.latex-project.org/get/ to open table output files





***************************************
* MANUSCRIPT
***************************************	


// Figure 1A
use "RIO_replication data.dta", clear
bysort IGO_num year: egen WDsumperIOyear=sum(Withdrawal)
collapse (max) Withdrawal WDsumperIOyear cowcode, by(IGO_num year) 	
by IGO_num year, sort: gen nvals = _n == 1 
set more off
by year, sort: count if nvals
by year, sort: egen totalIGOs= total(nvals)
bysort year: egen WDsumperyear=sum(WDsumperIOyear)
collapse (max) totalIGOs WDsumperyear , by(year)
twoway line  WDsumperyear  year, yscale(range(0(1)10)  alt axis(1)) ylabel(0(1)10) lpattern(solid dash) lcolor (black)   ///
	ytitle("Number of IGO Withdrawals",  axis(1))  ///
	|| line totalIGOs year, sort yaxis(2) yscale(alt axis(2)) ///
	lpattern(dot) lcolor (black black)   ///
	xtitle("Year") ytitle("Number of IGOs",  axis(2)) scheme(lean1) aspectratio(1.0) ///
	legend(order(1 "# IGO Withdrawals (right axis)" 2 "# of IGOs (left axis)" ) ///
	size(small) ring(0) position(12) bmargin(none) region(style(none)))  
graph export "Figure1A.pdf", replace



// Figure 1B
use "RIO_replication data.dta", clear
bysort year: egen WDsumperyear=sum(Withdrawal)
set more off
bysort year: sum WDsumperyear 
egen numIOsperYear = nvals(IGO_num), by(year) 
collapse (max) WDsumperyear numIOsperYear, by(year)
bysort year: gen WDperIOperYear=(WDsumperyear/numIOsperYear)*100
sort year 
twoway line  WDperIOperYear  year, lcolor (black)   ///
	ytitle("Percent" ) xtitle("Year") scheme(lean1) aspectratio(1.0) ///
	legend(off ) yscale(range(0 12)) yla(0(5)10)
graph export "Figure1B.pdf", replace



// Figure 2
use "RIO_replication data.dta", clear
gen obs=1
replace Withdrawal=0 if Withdrawal==.
by COUNTRY_NAME, sort: egen SumWithdrawal=total(Withdrawal)
collapse (max) SumWithdrawal, by(COUNTRY_NAME)
gsort -SumWithdrawal COUNTRY_NAME
drop if SumWithdrawal==0
gsort -SumWithdrawal
drop if SumWithdrawal<2

set scheme lean1
grstyle init
grstyle color background white
grstyle color major_grid white

graph hbar SumWithdrawal,  ///
	blabel(bar, position(base) format(%9.0f) color(black)) ///
	over(COUNTRY_NAME, sort(1) descending  gap(*0.7) lab(labsize(vsmall ))) ytitle("Frequency",size(vsmall))  ///
	graphregion(color(white)) ylabel(,labsize(vsmall)) ysize(4.8) xsize(3) 
graph export "Figure2.pdf", replace
grstyle clear



// Figure 3
use "RIO_replication data.dta", clear
gen obs=1
replace Withdrawal=0 if Withdrawal==.
by IGO_short, sort: egen SumWithdrawal=total(Withdrawal)
collapse (max) SumWithdrawal, by(IGO_long)
gsort -SumWithdrawal IGO_long
drop if SumWithdrawal==0
gsort -SumWithdrawal
drop if SumWithdrawal<3

set scheme lean1
grstyle init
grstyle color background white
grstyle color major_grid white

graph hbar SumWithdrawal, ///
	blabel(bar, position(base) format(%9.0f) color(black)) ///
	over(IGO_long, sort(1) descending  gap(*0.7) lab(labsize(vsmall ))) ytitle("Frequency",size(vsmall))  ///
	graphregion(color(white)) ylabel(,labsize(vsmall)) ysize(4.8) xsize(3) 
graph export "Figure3.pdf", replace
grstyle clear



// Figure 4  
use "RIO_replication data.dta", clear
keep if Withdrawal==1
tab GeneralReason, mi

set scheme lean1
grstyle init
grstyle color background white
grstyle color major_grid white

catplot GeneralReason, percent var1opts(sort(1) descending) ///
	  ytitle("Percent")  l1title("")  
graph export "Figure4.pdf", replace

grstyle clear



// Figure 5
use "RIO_replication data.dta", clear
set more off	
local PolDom		Democracy_Lag1   GovmtOrientChange     
local controls		IOMembershipDuration_Lag1Log IOsize_Lag1Log  time time2 time3
eststo: relogitll  Withdrawal NationalistExec `PolDom'   	`controls' if year>=1990 , cluster(IGO_num)
est store Fig5m1
scalar list 
estadd scalar AIC
estadd scalar BIC
eststo: relogitll  Withdrawal NationalistExecVote `PolDom'   `controls' if year>=1990, cluster(IGO_num)
est store Fig5m2
scalar list 
estadd scalar AIC
estadd scalar BIC
eststo: relogitll  Withdrawal NationalistGov `PolDom'   	`controls' if year>=1990 , cluster(IGO_num)
est store Fig5m3
scalar list 
estadd scalar AIC
estadd scalar BIC
eststo: relogitll  Withdrawal NationalistGovVote `PolDom'   `controls'  if year>=1990 , cluster(IGO_num)
est store Fig5m4
scalar list 
estadd scalar AIC
estadd scalar BIC
eststo: relogitll  Withdrawal NationalistOpp `PolDom'   	`controls' if year>=1990 , cluster(IGO_num)
est store Fig5m5
scalar list 
estadd scalar AIC
estadd scalar BIC
eststo: relogitll  Withdrawal NationalistOppVote `PolDom'  	`controls' if year>=1990 , cluster(IGO_num)
est store Fig5m6
scalar list 
estadd scalar AIC
estadd scalar BIC
eststo: relogitll  Withdrawal Populist `PolDom'   			`controls' if year>=1990 , cluster(IGO_num)
est store Fig5m7
scalar list 
estadd scalar AIC
estadd scalar BIC

set scheme lean1
grstyle init
grstyle color background white
grstyle color major_grid white

coefplot (Fig5m1) (Fig5m2) (Fig5m3) (Fig5m4) (Fig5m5) (Fig5m6) (Fig5m7) ///
	, drop( _cons time*  ///
	Institutionalization_Lag1 WithdrawalClause  ///
	IOissuePolitics IOissueEcon ///
	Democracy_Lag1 BS_Lag1 GovmtOrientChange  ///
	PrefDiversionFromIOavg_Lag1 WithdrawalLeadState_Lag1 ///
	StatePower_Lag1 StatePowerChange   ///
	IOMembershipDuration_Lag1Log IOsize_Lag1Log ) xline(0, lpattern(dash)) ///
	legend(off) msymbol(d) nooffsets mcolor(blue) ciopts(color (blue))
graph export "Figure5.pdf", replace
grstyle clear




// Table 1
use "RIO_replication data.dta", clear
set more off	
local PolDom		Democracy_Lag1  GovmtOrientChange Nationalist_Lag1   
local Function 		Institutionalization_Lag1 IOavgDemScore  ///
					IOissuePolitics IOissueEcon
local PolIntl		StatePowerChange PrefDiversionFromIOavg_Lag1 ///
					WithdrawalLeadState_Lag1
local controls		IOMembershipDuration_Lag1Log IOsize_Lag1Log time time2 time3   

relogitll  Withdrawal  `PolDom'  					`controls' , cluster(IGO_num)
est store T1m1
scalar list 
estadd scalar AIC
estadd scalar BIC

relogitll  Withdrawal  `Function'  					`controls' , cluster(IGO_num)
est store T1m2
scalar list 
estadd scalar AIC
estadd scalar BIC

relogitll  Withdrawal  `PolIntl'  					 `controls' , cluster(IGO_num)
est store T1m3
scalar list 
estadd scalar AIC
estadd scalar BIC

relogitll  Withdrawal `PolDom'  `Function'  `PolIntl' `controls' , cluster(IGO_num)
est store T1m4
scalar list 
estadd scalar AIC
estadd scalar BIC
// Table 1 output
esttab T1m* using Table1.tex, replace b(3) se(3) staraux ///
	star(* 0.10 ** 0.05 *** 0.01) scalar(AIC BIC)  ///
	noconstant label alignment(D{.}{.}{-1}) ///
	title(Determinants of IGO Withdrawals\label{results})  ///
	order(Democracy_Lag1  GovmtOrientChange Nationalist_Lag1 ///
	Institutionalization_Lag1 IOavgDemScore  ///
	IOissuePolitics IOissueEcon ///
	PrefDiversionFromIOavg_Lag1 WithdrawalLeadState_Lag1 ///
	StatePowerChange ///
	IOMembershipDuration_Lag1Log IOsize_Lag1Log) ///
	drop ( _cons time*  )
	
	


// CEM results mentioned in results section  
use "RIO_replication data.dta", clear
set more off	
foreach varname in  Withdrawal Democracy_Lag1 GovmtOrientChange Nationalist_Lag1   ///
	time time2 time3  GDPGrowth_Lag1 {
	drop if `varname'==.
	}	

cem Democracy_Lag1 (#5) GovmtOrientChange (#1) GDPGrowth_Lag1(#5), tr(Nationalist_Lag1)

relogitll Withdrawal Nationalist_Lag1 [iweight=cem_weights]

relogitll Withdrawal Nationalist_Lag1 ///
	Democracy_Lag1 GovmtOrientChange GDPGrowth_Lag1 [iweight=cem_weights]
	
relogitll Withdrawal Nationalist_Lag1 ///
	Democracy_Lag1 GovmtOrientChange GDPGrowth_Lag1 ///
	IOMembershipDuration_Lag1Log IOsize_Lag1Log time time2 time3 ///
	[iweight=cem_weights] 
	
	