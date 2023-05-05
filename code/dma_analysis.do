**************
*** JMP: DMA-Analysis 
*** Author: Aycan Katitas
*** Date Updated: August 2022
**************
clear
set more off

cd "/Users/aycankatitas/Dropbox/jmp"

use trade_dma_cum_analysis.dta, clear

* encode comp_alt, gen(comp_alt_f)

* Variable Labels 
** Trade 
lab var manuloss7007_pw "Manufacturing Decline 1970-2007"
lab var bartik_iv_07 "Bartik Instrument 1970-2007"
lab var manuloss7000_pw "Manufacturing Decline 1970-2000"
lab var bartik_iv_00 "Bartik Instrument 1970-2000"
lab var manuloss7016_pw "Manufacturing Decline 1970-2016"
lab var bartik_iv_16 "Bartik Instrument 1970-2016"
lab var manuloss0007_pw "Manufacturing Decline 2000-2007"
lab var bartik_iv_0007 "Bartik Instrument 2000-2007"
lab var manuloss0016_pw "Manufacturing Decline 2000-2016"
lab var bartik_iv_0016 "Bartik Instrument 2000-2016" 
lab var manuloss_cbp_8707_pw "Manufacturing Decline (CBP) 1987-2007"
lab var bartik_8707 "Bartik Instrument (CBP) 1987-2007"
lab var manuloss_cbp_0007_pw "Manufacturing Decline (CBP) 2000-2007"
lab var bartik_0007 "Bartik Instrument (CBP) 2000-2007"
lab var d_ip_usa_com "China Trade Shock 2000-2007"
lab var d_ip_mexusa94_com "NAFTA Trade Shock 1994-2007"
lab var d_ip_lowus_com "Low-Income Trade Shock 2000-2007"
lab var d_ip_d9_com "China Trade Shock Instrument"

** Ads 
lab var per_trade "\% Anti-Trade Ads"
lab var per_trade_h "\% Anti-Trade Ads House"
lab var per_trade_s "\% Anti-Trade Ads Senate"
lab var per_trade_p "\% Anti-Trade Ads Pres"
lab var per_trade_D "\% Anti-Trade Ads Democrats"
lab var per_trade_R "\% Anti-Trade Ads Republicans"
lab var tradead_hsp "Count of Trade Ads"
lab var totalad_hsp_log "Total Ads Aired (logged)"
lab var totalad_hsp "Total Ads Aired"
lab var totalad_house_log "Total Ads Aired House (logged)"
lab var totalad_senate_log "Total Ads Aired Senate (logged)"
lab var totalad_pres_log "Total Ads Aired Pres (logged)"
lab var totalad_hsp00_log "Total Ads Aired (logged)"
lab var totalad_hsp14_log "Total Ads Aired (logged)"
lab var totalad_hsp_D_log "Total Ads Aired Democrats (logged)"
lab var totalad_hsp_R_log "Total Ads Aired Republicans (logged)"
lab var per_china "\% China Ads"
lab var per_jobs "\% Jobs Ads"
lab var per_imm "\% Immigration Ads"


* Controls
lab var per_white70 "\% White"
lab var per_college70 "\% College-Educated"
lab var per_male70 "\% Male"
lab var per_old70 "\% Old"
lab var per_white00 "\% White"
lab var per_college00 "\% College-Educated"
lab var per_male00 "\% Male"
lab var per_old00 "\% Old"
lab var permanu70 "\% Manufacturing Employment 1970"
lab var comp_cont_scaled "Competitive Election (Scaled)"
lab var ave_unemp_0816_c "Average Unemployment Rate 2008-2016"
lab var d_ser7007_per "Service Employment Increase 1970-2007"
lab var per_inttrade_hsp "\% Interest Group Anti-Trade Ads"
lab var per_inttrade_h "\% Interest Group Anti-Trade Ads"
lab var per_inttrade_s "\% Interest Group Anti-Trade Ads"
lab var per_inttrade_p "\% Interest Group Anti-Trade Ads"


global controls per_white70 per_college70 per_male70 per_old70 comp_cont_scaled


************ MAIN RESULTS ************
********** TABLE 1 - FIRST STAGE - media market, short
********** Geographic Media Market Targeting of Anti-Trade Ads 2008-2016
** LTMD is instrumented by Bartik
capture estimates drop *
qui {	
reg manuloss7007_pw bartik_iv_07, robust 
est sto first0
estadd local totalad "No", replace
estadd local comp "No", replace 
estadd local dems "No", replace

reg manuloss7007_pw bartik_iv_07 $controls, robust
est sto first1
estadd local totalad "Yes", replace
estadd local comp "Yes", replace 
estadd local dems "Yes", replace
}

esttab first0 first1 using output/table1.tex, se(3) wrap label noomitted noconstant replace parentheses stats(N totalad comp dems, fmt(0 3) labels("Observations" "Total Advertising Control" "Election Control" "Demographic Controls" ))  ///
	keep(bartik_iv_07) ///
	mgroups("\shortstack{Decline in \% Manufacturing Employment \\ 1970-2007}" "\shortstack{Decline in \% Manufacturing Employment \\ 1970-2007} ", pattern(1 1)) mlabels(none) ///
	booktabs nonotes ///
	starlevels( * 0.10 ** 0.05 *** 0.010) 

est clear 

********** TABLE 2 - OLS + 2SLS - Main: all, house, senate - media market, short
********** Geographic Media Market Targeting of Anti-Trade Ads 2008-2016
** LTMD is instrumented by Bartik
capture estimates drop *
qui{
*** OLS 
* Trade HSP 
reg per_trade manuloss7007_pw totalad_hsp_log $controls, robust 
est sto ols1
estadd local totalad "Yes", replace
estadd local comp "Yes", replace 
estadd local dems "Yes", replace
** Trade House 
reg per_trade_h manuloss7007_pw totalad_house_log $controls, robust 
est sto ols2
estadd local totalad "Yes", replace
estadd local comp "Yes", replace 
estadd local dems "Yes", replace
** Trade Senate
reg per_trade_s manuloss7007_pw totalad_senate_log $controls, robust 
est sto ols3
estadd local totalad "Yes", replace
estadd local comp "Yes", replace 
estadd local dems "Yes", replace
*** 2SLS 
** Trade HSP
ivreg2 per_trade totalad_hsp_log $controls (manuloss7007_pw= bartik_iv_07), robust 
est sto sls1
estadd local totalad "Yes", replace
estadd local comp "Yes", replace 
estadd local dems "Yes", replace
estadd local fstat "72.713", replace
** Trade House 
ivreg2 per_trade_h totalad_house_log $controls (manuloss7007_pw= bartik_iv_07), robust first 
est sto sls2
estadd local totalad "Yes", replace
estadd local comp "Yes", replace 
estadd local dems "Yes", replace
estadd local fstat "73.293", replace
** Trade Senate 
ivreg2 per_trade_s totalad_senate_log $controls (manuloss7007_pw= bartik_iv_07), robust first 
est sto sls3
estadd local totalad "Yes", replace
estadd local comp "Yes", replace 
estadd local dems "Yes", replace
estadd local fstat "75.964", replace
} 

** prefix should be in mgroup, keep quotation marks around the output file 
esttab ols1 ols2 ols3 sls1 sls2 sls3 using "output/table2.tex", se(3) wrap label noomitted noconstant replace parentheses stats(N totalad comp dems fstat, fmt(0 3) labels("Observations"  "Total Advertising Control" "Election Control" "Demographic Controls" "Weak Identification F Statistic"))  ///
	keep(manuloss7007_pw) ///
	mgroups("OLS" "2SLS", pattern(1 0 0 1 0 0) ///
	prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) alignment(D{.}{.}{-1}) ///
	mtitles("\shortstack{\% Trade Ads \\ (All)}" "\shortstack{\% Trade Ads \\ (House)}" "\shortstack{\% Trade Ads \\ (Senate)}" "\shortstack{\% Trade Ads \\ (All)}" "\shortstack{\% Trade Ads \\ (House)}" "\shortstack{\% Trade Ads \\ (Senate)}") ///
	booktabs nonotes ///
	starlevels( * 0.10 ** 0.05 *** 0.010)

est clear 
********** TABLE 3 - 2SLS - china, jobs, immigration, media market level, short
********** Geographic Media Market Targeting of  China/Jobs Ads 2008-2016
********** LTMD - Bartik 1970-2007
*** 2SLS 
capture estimates drop *
qui{
** China HSP
ivreg2 per_china totalad_hsp_log $controls (manuloss7007_pw= bartik_iv_07), robust 
est sto china1
estadd local totalad "Yes", replace
estadd local comp "Yes", replace 
estadd local dems "Yes", replace
** China HSP - 1970-2000
ivreg2 per_china totalad_hsp_log $controls (manuloss7000_pw= bartik_iv_00), robust 
est sto china2
estadd local totalad "Yes", replace
estadd local comp "Yes", replace 
estadd local dems "Yes", replace
** China HSP - 2000-2007
ivreg2 per_china totalad_hsp_log $controls (manuloss0007_pw= bartik_iv_0007), robust 
est sto china3
estadd local totalad "Yes", replace
estadd local comp "Yes", replace 
estadd local dems "Yes", replace
** Jobs HSP 
ivreg2 per_jobs totalad_hsp_log $controls (manuloss7007_pw= bartik_iv_07), robust 
est sto jobs
estadd local totalad "Yes", replace
estadd local comp "Yes", replace 
estadd local dems "Yes", replace
** Immigration HSP 
ivreg2 per_immig totalad_hsp_log $controls (manuloss7007_pw= bartik_iv_07), robust 
est sto imm
estadd local totalad "Yes", replace
estadd local comp "Yes", replace 
estadd local dems "Yes", replace
}

esttab china1 china2 china3 jobs imm using "output/table3.tex", se(3) wrap label noomitted noconstant replace parentheses stats(N totalad comp dems, fmt(0 3) labels("Observations"  "Total Advertising Control" "Election Control" "Demographic Controls"))  ///
	keep(manuloss*) ///
	mgroups("\shortstack{\% China Ads \\ (All)}" "\shortstack{\% China Ads \\ (All)}" "\shortstack{\% China Ads \\ (All)}" "\shortstack{\% Jobs Ads \\ (All)}" "\shortstack{\% Immigration Ads \\ (All)}", pattern(1 1 1 1 1)) mlabels(none) ///
	booktabs nonotes ///
	starlevels( * 0.10 ** 0.05 *** 0.010) 




est clear 
************ APPENDIX ************
********** TABLE A1 - Media Market List with Anti-Trade Ad Proportions 

estpost tabstat per_trade, by(market) c(stat) stat(mean)

esttab using output/tablea1.tex, cells("mean(fmt(a3))") replace nonumber ///
   nomtitle nonote varlabels(`e(labels)') collabels("\% Anti-Trade Ads") ///
   booktabs longtable


est clear 
********** TABLE A2 - Summary Statistics, Media Market Level 
estpost tabstat per_trade per_trade_h per_trade_s per_trade_p per_trade_D per_trade_R per_china per_jobs per_immig manuloss7007_pw manuloss7000_pw manuloss7016_pw manuloss0007_pw manuloss0016_pw permanu70 bartik_iv_07  bartik_iv_00  bartik_iv_16 bartik_iv_0007 bartik_iv_0016 d_ip_usa_com d_ip_d9_com totalad_hsp_log totalad_house_log totalad_senate_log totalad_pres_log per_inttrade_hsp per_white70 per_male70 per_old70 per_college70 comp_cont_scaled ave_unemp_0816_c d_ser7007_per, c(stat) stat(n mean sd min max p25 p75)

esttab using output/tablea2.tex, cells("mean(fmt(a3)) sd min max p25 p75") replace nonumber ///
   nomtitle nonote label collabels("Mean" "Std. Deviation" "Min" "Max" "25th Percentile" "75th percentile") ///
   booktabs
   
est clear 

  
********** TABLE A3 - First Stage - Media Market, Full
********** Geographic Media Market Targeting of Anti-Trade Ads 2008-2016
********** LTMD - Bartik 1970-2007
capture estimates drop *
qui {	
reg manuloss7007_pw bartik_iv_07, robust 
est sto first0

reg manuloss7007_pw bartik_iv_07 totalad_hsp_log $controls, robust
est sto first1

}

esttab first0 first1 using output/tablea3.tex, se(3) wrap label noomitted noconstant replace parentheses stats(N,  labels("Observations"))  ///
	order(bartik_iv_07 $controls totalad_hsp_log) ///
	mgroups("\shortstack{Decline in \% Manufacturing Employment \\ 1970-2007}" "\shortstack{Decline in \% Manufacturing Employment \\ 1970-2007} ", pattern(1 1)) mlabels(none) ///
	booktabs nonotes ///
	starlevels( * 0.10 ** 0.05 *** 0.010) 

est clear 

********** TABLE A4 - OLS + 2SLS - main: all, house, senate - media market, full
********** Geographic Media Market Targeting of Anti-Trade Ads 2008-2016
********** LTMD - Bartik 1970-2007
capture estimates drop *
qui{
*** OLS 
* Trade HSP 
reg per_trade manuloss7007_pw totalad_hsp_log $controls, robust 
est sto olsa1
** Trade House 
reg per_trade_h manuloss7007_pw totalad_house_log $controls, robust 
est sto olsa2
** Trade Senate
reg per_trade_s manuloss7007_pw totalad_senate_log $controls, robust 
est sto olsa3
*** 2SLS 
** Trade HSP
ivreg2 per_trade totalad_hsp_log $controls (manuloss7007_pw= bartik_iv_07), robust 
est sto slsa1
** Trade House 
ivreg2 per_trade_h totalad_house_log $controls (manuloss7007_pw= bartik_iv_07), robust
est sto slsa2
** Trade Senate 
ivreg2 per_trade_s totalad_senate_log $controls (manuloss7007_pw= bartik_iv_07), robust
est sto slsa3
} 

** prefix should be in mgroup, keep quotation marks around the output file 
esttab olsa1 olsa2 olsa3  slsa1 slsa2 slsa3  using "output/tablea4.tex", se(3) wrap label noomitted noconstant replace parentheses stats(N, labels("Observations"))  ///
	order(manuloss7007_pw $controls totalad*) ///
	mgroups("OLS" "2SLS", pattern(1 0 0  1 0 0 ) ///
	prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) alignment(D{.}{.}{-1}) ///
	mtitles("\shortstack{\% Trade Ads \\ (All)}" "\shortstack{\% Trade Ads \\ (House)}" "\shortstack{\% Trade Ads \\ (Senate)}" "\shortstack{\% Trade Ads \\ (All)}" "\shortstack{\% Trade Ads \\ (House)}" "\shortstack{\% Trade Ads \\ (Senate)}") ///
	booktabs nonotes ///
	starlevels( * 0.10 ** 0.05 *** 0.010)

********** TABLE A5 - 2SLS - president ad + service + unemployment control, media market
********** Geographic Media Market Targeting of Anti-Trade Ads 2008-2016
********** LTMD - Bartik 1970-2007
*** 2SLS 
** Trade HSP  + Trade Pres
capture estimates drop *
qui{
*** 2SLS
* Trade Pres
ivreg2 per_trade_p totalad_pres_log $controls (manuloss7007_pw= bartik_iv_07), robust 
est sto presa1
* Average unemployment control
ivreg2 per_trade totalad_hsp_log $controls ave_unemp_0816_c d_ser7007_per (manuloss7007_pw= bartik_iv_07), robust
est sto unempa1
}

esttab presa1 unempa1 using output/tablea5.tex, b(3) se(3) wrap label noomitted noconstant replace parentheses stats(N, fmt(0)  labels("Observations"))  ///
	order(manuloss7007_pw $controls totalad* d_ser7007_per ave_unemp_0816_c) ///
	mgroups("\shortstack{\% Trade Ads \\ (President)}" "\shortstack{\% Trade Ads \\ (All)}", pattern(1 1)) mlabels(none) ///
	booktabs nonotes ///
	starlevels( * 0.10 ** 0.05 *** 0.010) 

********** TABLE A6 - IV Poisson specifications 
********** Geographic Media Market Targeting of Anti-Trade Ads 2008-2016
** LTMD is instrumented by Bartik
** Trade HSP
** ivpoisson total ads as control
capture estimates drop *
qui{
ivpoisson gmm tradead_hsp totalad_hsp_log $controls (manuloss7007_pw= bartik_iv_07)
est sto ivpoisa1
*margins, at((asobserved)) at(manuloss7007_pw=generate(manuloss7007_pw+10)) contrast(at(r._at)) 
*looking at the number of ads aired from 25th percentile to 75th percentile
* offset 
ivpoisson gmm tradead_hsp $controls (manuloss7007_pw= bartik_iv_07), exposure(totalad_hsp)
est sto ivpoisa2
** ivpoisson control function
ivpoisson cfunction tradead_hsp totalad_hsp $controls (manuloss7007_pw= bartik_iv_07)
est sto ivpoisa3
}

esttab ivpoisa1 ivpoisa2 ivpoisa3 using output/tablea6.tex, b(3) se(3) wrap label noomitted noconstant replace parentheses stats(N, fmt(0)  labels("Observations"))  ///
	order(manuloss7007_pw $controls totalad*) ///
	mgroups("\shortstack{Count of Trade Ads \\ (All)}" "\shortstack{Count of Trade Ads \\ (All)}" "\shortstack{Count of Trade Ads \\ (All)}", pattern(1 1 1)) mlabels(none) ///
	booktabs nonotes ///
	starlevels( * 0.10 ** 0.05 *** 0.010)
	
********** TABLE A7 - 2SLS - Anti-Trade Ads 2000-2016, 2008-2014
********** Geographic Media Market Targeting of Anti-Trade Ads
********** LTMD - Bartik 1970-2007
*** 2SLS 
capture estimates drop *
qui{
** Trade 2000-2016
ivreg2 per_trade00 totalad_hsp_log $controls (manuloss7007_pw= bartik_iv_07), robust 
est sto trade00
* Average unemployment control
ivreg2 per_trade14 totalad_hsp_log $controls (manuloss7007_pw= bartik_iv_07), robust
est sto trade14
}

esttab trade00 trade14 using output/tablea7.tex, b(3) se(3) wrap label noomitted noconstant replace parentheses stats(N, fmt(0)  labels("Observations"))  ///
	order(manuloss7007_pw $controls totalad*) ///
	mgroups("\shortstack{\% Trade Ads \\ 2000-2016}" "\shortstack{\% Trade Ads \\ 2008-2014}", pattern(1 1)) mlabels(none) ///
	booktabs nonotes ///
	starlevels( * 0.10 ** 0.05 *** 0.010) 
	

********** TABLE A11 - Differences in Timing of Deindustrialization 
********** Geographic Media Market Targeting of Anti-Trade Ads 2008-2016
** LTMD is instrumented by Bartik
capture estimates drop *
qui{
*** 2SLS
* Trade HSP - 1970-2000
ivreg2 per_trade totalad_hsp_log $controls (manuloss7000_pw= bartik_iv_00), robust
est sto dm1
estadd local fstat "12.109", replace
* Trade HSP - 1970-2016 
ivreg2 per_trade totalad_hsp_log $controls (manuloss7016_pw= bartik_iv_16), robust
est sto dm2
estadd local fstat "77.929", replace
* Trade HSP - 2000-2007 
ivreg2 per_trade totalad_hsp_log $controls (manuloss0007_pw= bartik_iv_0007), robust
est sto dm3
estadd local fstat "193.884", replace
* Trade HSP - 2000-2016 
ivreg2 per_trade totalad_hsp_log $controls (manuloss0016_pw= bartik_iv_0016), robust 
est sto dm4
estadd local fstat "177.239", replace
* Trade HSP - initial manufacturing hub 1970 
reg per_trade permanu70 totalad_hsp_log $controls, robust
est sto dm5
} 

esttab dm1 dm2 dm3 dm4 dm5 using output/tablea11.tex, b(3) se(3) wrap label noomitted noconstant replace parentheses stats(N fstat, fmt(0 3)  labels("Observations" "Weak Identification F Statistic"))  ///
	order(manuloss* permanu70 $controls totalad*) ///
	mgroups("\shortstack{\% Trade Ads \\ (All)}" "\shortstack{\% Trade Ads \\ (All)}" "\shortstack{\% Trade Ads \\ (All)}" "\shortstack{\% Trade Ads \\ (All)}" "\shortstack{\% Trade Ads \\ (All)}", pattern(1 1 1 1 1)) mlabels(none) ///
	booktabs nonotes ///
	starlevels( * 0.10 ** 0.05 *** 0.010) 

********** TABLE A12 - Manufacturing Subindustries County Business Pattern (1987-2007,2000-2007)
********** Geographic Media Market Targeting of Anti-Trade Ads 2008-2016
** LTMD CBP is instrumented by Bartik
** 2SLS 
capture estimates drop *
qui{
** Trade HSP
ivreg2 per_trade totalad_hsp_log $controls (manuloss_cbp_8707_pw= bartik_8707), robust 
est sto cbp1
estadd local fstat "37.949", replace
** Trade HSP 
ivreg2 per_trade totalad_hsp_log $controls (manuloss_cbp_0007_pw= bartik_0007), robust 
est sto cbp2
estadd local fstat "221.619", replace
}
esttab cbp1 cbp2 using "output/tablea12.tex", se(3) wrap label noomitted noconstant replace parentheses stats(N fstat, fmt(0 3) labels("Observations" "Weak Identification F Statistic"))  ///
	order(manuloss* $controls totalad_hsp_log) ///
	mgroups("\shortstack{\% Trade Ads \\ (All)}" "\shortstack{\% Trade Ads \\ (All)}", pattern(1 1)) mlabels(none) ///
	booktabs nonotes ///
	starlevels( * 0.10 ** 0.05 *** 0.010) 

********** TABLE A13 - 2SLS - china, jobs, immigration, media market level, full
********** Geographic Media Market Targeting of  China/Jobs/Immigration Ads 2008-2016
********** LTMD - Bartik 1970-2007
*** 2SLS 
capture estimates drop *
qui{
** China HSP
ivreg2 per_china totalad_hsp_log $controls (manuloss7007_pw= bartik_iv_07), robust 
est sto chinaa1
** China HSP - 1970-2000
ivreg2 per_china totalad_hsp_log $controls (manuloss7000_pw= bartik_iv_00), robust 
est sto chinaa2
** China HSP - 2000-2007
ivreg2 per_china totalad_hsp_log $controls (manuloss0007_pw= bartik_iv_0007), robust 
est sto chinaa3
** Jobs HSP 
ivreg2 per_jobs totalad_hsp_log $controls (manuloss7007_pw= bartik_iv_07), robust 
est sto jobsa
** Immigration HSP 
ivreg2 per_immig totalad_hsp_log $controls (manuloss7007_pw= bartik_iv_07), robust 
est sto imma
estadd local totalad "Yes", replace
estadd local comp "Yes", replace 
estadd local dems "Yes", replace
}

esttab chinaa1 chinaa2 chinaa3 jobsa imma using "output/tablea13.tex", se(3) wrap label noomitted noconstant replace parentheses stats(N, fmt(0) labels("Observations"))  ///
	order(manuloss* $controls totalad_hsp_log) ///
	mgroups("\shortstack{\% China Ads \\ (All)}" "\shortstack{\% China Ads \\ (All)}" "\shortstack{\% China Ads \\ (All)}" "\shortstack{\% Jobs Ads \\ (All)}" "\shortstack{\% Immigration Ads \\ (All)}", pattern(1 1 1 1 1)) mlabels(none) ///
	booktabs nonotes ///
	starlevels( * 0.10 ** 0.05 *** 0.010) 
	
ivreg2 per_china totalad_hsp_log $controls (d_ip_usa_com= d_ip_d9_com), robust 
	
********** TABLE A14 - 2SLS - China, Mexico, Low income shock 
********** Geographic Media Market Targeting of Anti-Trade Ads 2008-2016
********** LTMD and China shock 
*** 2SLS 
capture estimates drop *
qui{
** Trade HSP - China Shock	
ivreg2 per_trade totalad_hsp_log $controls (d_ip_usa_com=d_ip_d9_com), robust
est sto adh1 
estadd local fstat "170.115", replace
** Trade HSP - LTMD instrumented ADH
ivreg2 per_trade totalad_hsp_log $controls (manuloss0007_pw=d_ip_d9_com), robust 
est sto adh2
estadd local fstat "66.083", replace
** Trade HSP - Mexico Shock	
ivreg2 per_trade totalad_hsp_log $controls (d_ip_mexusa94_com=d_ip_mexoth94_com), robust 
est sto mex1
estadd local fstat "132.990", replace
** Trade HSP - LTMD instrumented Mexico
ivreg2 per_trade totalad_hsp_log $controls (manuloss0007_pw=d_ip_mexoth94_com), robust 
est sto mex2
estadd local fstat "8.669", replace
** Trade HSP - Low Income Shock	
ivreg2 per_trade totalad_hsp_log $controls (d_ip_lowus_com=d_ip_lowoth_com), robust 
est sto low1
estadd local fstat "36.363", replace
** Trade HSP - LTMD instrumented low income
ivreg2 per_trade totalad_hsp_log $controls (manuloss0007_pw=d_ip_lowoth_com), robust 	
est sto low2
estadd local fstat "26.106", replace
}

esttab adh1 mex1 low1 adh2 mex2 low2 using "output/tablea14.tex", se(3) wrap label noomitted noconstant replace parentheses stats(N fstat, fmt(0) labels("Observations" "Weak Identification F Statistic"))  ///
	order(d* manuloss*  $controls totalad_hsp_log) ///
	mgroups("\shortstack{\% Trade Ads \\ (All)}" "\shortstack{\% Trade Ads \\ (All)}" "\shortstack{\% Trade Ads \\ (All)}" "\shortstack{\% Trade Ads \\ (All)}" "\shortstack{\% Trade Ads \\ (All)}" "\shortstack{\% Trade Ads \\ (All)}", pattern(1 1 1 1 1 1)) mlabels(none) ///
	booktabs nonotes ///
	starlevels( * 0.10 ** 0.05 *** 0.010) 

********** TABLE A15 - 2SLS - Decomposition of China Shock and other shocks 
********** Geographic Media Market Targeting of Anti-Trade Ads 2008-2016
********** LTMD and China shock - 2000-2007 
*** DECOMPOSING THE MEASURE 

capt program drop mlr

program define mlr, rclass 
	syntax, y(name) controls(varlist)

	* Step 1) Regress Bartik IV (00-07) on ADH trade shock instrument and controls, predict residuals
	reg bartik_iv_0007 d_ip_d9_com  `controls'
	predict shift_share_tilde, res

	* Step 2) Regress change in manufacturing share on residual, obtain fitted value - this is Equation 5 in Charles et al. 2019  
	reg manuloss0007_pw shift_share_tilde
	predict d_manuf_hat_ss, xb

	* Step 3) Regress change in manufacturing share on shift share residual, trade shock instrument, and controls
	* Obtain d_manuf_hat from the trade shock
	reg manuloss0007_pw shift_share_tilde d_ip_d9_com  `controls' 
	gen d_manuf_hat_ts = _b[d_ip_d9_com ]*d_ip_d9_com 

	* Step 4) Multiple outcomes of interest: per_trade: house, senate, presidential, all
	* predicting trade ads by the percentage-point change in a media market's manufacturing employment share predicted by 1) all other shocks 2) trade shocks
	reg `y' d_manuf_hat_ss d_manuf_hat_ts `controls', vce(robust)

	* Capture coefficients & R-squared
	
	local beta_ss = _b[d_manuf_hat_ss]
	local beta_ts = _b[d_manuf_hat_ts]
	local R2 	  = e(r2)
	
	return scalar beta_ss = `beta_ss'
	return scalar beta_ts = `beta_ts'
	return scalar R2	  = `R2'
	
	drop shift_share_tilde d_manuf_hat_ss d_manuf_hat_ts
		
	end

set seed 1206

* Do all the steps with given outcomes and controls 
mlr, y(per_trade) controls(totalad_hsp_log per_white70 per_college70 per_male70 per_old70 comp_cont_scaled)
	return list
	local R2 = `r(R2)'
	
* Bootstrap the SEs because we are doing 2SLS by hand 	
bootstrap (mlr: beta_ts = r(beta_ts) beta_ss = r(beta_ss)) : ///
	mlr, y(per_trade) controls(totalad_hsp_log per_white70 per_college70 per_male70 per_old70 comp_cont_scaled)
	
* Compare the coefficients 
test [mlr]beta_ts = [mlr]beta_ss
eret2 scalar pvdiff = `r(p)'
eststo res1

esttab res1 using "output/tablea15.tex", replace ///
order(beta_ts beta_ss) se(3) wrap nostar not noomitted noconstant ///
stats(N, labels("Observations")) ///
	coeflabel(beta_ss "Manufacturing Decline 2000-2007 (Other Forces)" beta_ts "Manufacturing Decline 2000-2007 (Chinese Imports)")  ///
	scalars("pvdiff p-value of Difference") ///
	mgroups("\shortstack{\% Trade Ads \\ (All)}", pattern(1)) mlabels(none) ///
	booktabs nonotes

eststo clear 
********** TABLE A16 - 2SLS - Anti-Trade Ads 2008-201 with Dems + Reps + Interest Group Ads Control
********** Geographic Media Market Targeting of Anti-Trade Ads
********** LTMD - Bartik instrument
*** 2SLS 
capture estimates drop *
qui{
** Trade HSP 08-2016
** Democrats 
ivreg2 per_trade_D totalad_hsp_D_log $controls (manuloss7007_pw= bartik_iv_07), robust 
est sto dema1
** Republicans
ivreg2 per_trade_R totalad_hsp_R_log $controls (manuloss7007_pw= bartik_iv_07), robust 
est sto repa1
** Control for Interest Group Ads
ivreg2 per_trade totalad_hsp_log $controls per_inttrade_hsp (manuloss7007_pw= bartik_iv_07), robust 
est sto int1
}

esttab dema1 repa1 int1 using "output/tablea16.tex", b(3) se(3) wrap label noomitted noconstant replace parentheses stats(N, fmt(0)  labels("Observations"))  ///
	order(manuloss7007_pw $controls totalad* per_inttrade_hsp) ///
	mgroups("\shortstack{\% Trade Ads \\ (Democrats)}" "\shortstack{\% Trade Ads \\ (Repulicans)}" "\shortstack{\% Trade Ads \\ (All)}", pattern(1 1 1)) mlabels(none) ///
	booktabs nonotes ///
	starlevels( * 0.10 ** 0.05 *** 0.010) 
	





