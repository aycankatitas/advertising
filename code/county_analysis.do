**************
*** JMP: County-Analysis 
*** Author: Aycan Katitas
*** Date Updated: November 2022
**************

clear
set more off

cd "/Users/aycankatitas/Dropbox/jmp"

use trade_county_cum_analysis.dta, clear


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
lab var d_ip_usa_com "China Trade Shock 2000-2007"
lab var d_ip_mexusa94_com "NAFTA Trade Shock 1994-2007"
lab var d_ip_lowus_com "Low-Income Trade Shock 2000-2007"
lab var d_ip_d9_com "China Trade Shock Instrument"

** Ads 
lab var per_trade "\% Anti-Trade Ads"
lab var per_trade_h "\% Anti-Trade Ads House"
lab var per_trade_s "\% Anti-Trade Ads Senate"
lab var per_trade_p "\% Anti-Trade Ads Pres"
lab var tradead_hsp "Count of Trade Ads"
lab var totalad_hsp_log "Total Ads Aired (logged)"
lab var totalad_hsp "Total Ads Aired"
lab var totalad_house_log "Total Ads Aired House (logged)"
lab var totalad_senate_log "Total Ads Aired Senate (logged)"
lab var totalad_pres_log "Total Ads Aired Pres (logged)"
lab var totalad_hsp00_log "Total Ads Aired (logged)"
lab var totalad_hsp14_log "Total Ads Aired (logged)"
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
lab var comp_cont_scaled "Competitive Election (Scaled)"
lab var ave_unemp_0816_c "Average Unemployment Rate 2008-2016"
lab var d_ser7007_per "Service Employment Increase 1970-2007"
lab var per_inttrade_hsp "\% Interest Group Anti-Trade Ads"
lab var per_inttrade_h "\% Interest Group Anti-Trade Ads"
lab var per_inttrade_s "\% Interest Group Anti-Trade Ads"
lab var per_inttrade_p "\% Interest Group Anti-Trade Ads"

label define whitecat 0 "Minority White County" 1 "Majority White County"
lab values whitedum whitecat

global controls per_white70 per_college70 per_male70 per_old70 comp_cont_scaled

************ APPENDIX ************
********** TABLE A8 - FIRST STAGE - full, county level
********** Geographic Media Market Targeting of Anti-Trade Ads 2008-2016
** LTMD is instrumented by Bartik
** Deindustrialization increases % trade ads by 17 percentage points 
* Bivariate 
capture estimates drop *
qui{
reg manuloss7007_pw bartik_iv_07 i.state_fips, cluster(FIPS)
est sto firstca1
estadd local state "Yes", replace
* Full - Trade Ads - HSP
reg manuloss7007_pw bartik_iv_07 $controls totalad_hsp_log i.state_fips, cluster(FIPS)
est sto firstca2
estadd local state "Yes", replace
}

esttab firstca1 firstca2 using output/tablea8.tex, b(3) se(3) wrap label noomitted noconstant replace parentheses stats(N state, fmt(0) labels("Observations" "State Fixed Effects"))  ///
	order(bartik_iv_07 $controls totalad_hsp_log) ///
	drop(*.state_fips _cons) ///
	mgroups("\shortstack{Decline in \% Manufacturing Employment \\ 1970-2007}" "\shortstack{Decline in \% Manufacturing Employment \\ 1970-2007} ", pattern(1 1)) mlabels(none) ///
	booktabs nonotes ///
	starlevels( * 0.10 ** 0.05 *** 0.010) 

est clear 

********** TABLE A9 - OLS + 2SLS - main: all, house, senate - full, county level
********** Geographic Media Market Targeting of Anti-Trade Ads 2008-2016
********** LTMD - Bartik 1970-2007
*** OLS 
** Trade HSP 
capture estimates drop *
qui{
reg per_trade manuloss7007_pw $controls totalad_hsp_log i.state_fips, cluster(FIPS) 
eststo olsca1
estadd local state "Yes", replace
** Trade House
reg per_trade_h manuloss7007_pw $control totalad_house_log i.state_fips, cluster(FIPS)
eststo olsca2  
estadd local state "Yes", replace
** Trade Senate
reg per_trade_s manuloss7007_pw $controls totalad_senate_log i.state_fips, cluster(FIPS) 
eststo olsca3
estadd local state "Yes", replace
*** 2SLS 
** Trade HSP 
ivreg2 per_trade $controls totalad_hsp_log i.state_fips (manuloss7007_pw= bartik_iv_07), cluster(FIPS) partial(i.state_fips)
eststo slsca1
estadd local fstat "235.554", replace
estadd local state "Yes", replace
** Trade House 
ivreg2 per_trade_h $controls totalad_house_log i.state_fips (manuloss7007_pw= bartik_iv_07), cluster(FIPS)  partial(i.state_fips)
eststo slsca2
estadd local fstat "235.285", replace
estadd local state "Yes", replace
** Trade Senate 
ivreg2 per_trade_s $controls totalad_senate_log i.state_fips (manuloss7007_pw= bartik_iv_07), cluster(FIPS)  partial(i.state_fips)
eststo slsca3
estadd local fstat "237.166", replace
estadd local state "Yes", replace
}

** prefix should be in mgroup, keep quotation marks around the output file 
esttab olsca1 olsca2 olsca3 slsca1 slsca2 slsca3 using "output/tablea9.tex", b(3) se(3) wrap label noomitted noconstant replace parentheses stats(N fstat state, fmt(0 3) labels("Observations"  "Weak Identification F Statistic" "State Fixed Effects"))  ///
	order(manuloss7007_pw $controls totalad*) ///
	drop(*.state_fips _cons) ///
	mgroups("OLS" "2SLS", pattern(1 0 0 1 0 0) ///
	prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) alignment(D{.}{.}{-1}) ///
	mtitles("\shortstack{\% Trade Ads \\ (All)}" "\shortstack{\% Trade Ads \\ (House)}" "\shortstack{\% Trade Ads \\ (Senate)}" "\shortstack{\% Trade Ads \\ (All)}" "\shortstack{\% Trade Ads \\ (House)}" "\shortstack{\% Trade Ads \\ (Senate)}") ///
	booktabs nonotes ///
	starlevels( * 0.10 ** 0.05 *** 0.010)

********** TABLE A10 - 2SLS - Racial Targeting of Ads - dummy + continuous 
********** Geographic Media Market Targeting of Anti-Trade Ads 2008-2016
********** Interaction between LTMD (1970-2007) and White Dummy + % White Population 1970 
capture estimates drop *
qui{
* White Dummy Variable
ivreg2 per_trade totalad_hsp_log per_college70 per_male70 per_old70 comp_cont_scaled i.whitedum i.state_fips (c.manuloss7007_pw##i.whitedum= c.bartik_iv_07##i.whitedum), cluster(FIPS) partial(i.state_fips)
eststo whitea1
estadd local state "Yes", replace
*margins, at(manuloss7007_pw=(-180(20)60) whitedum=(0 1)) post
*marginsplot, by(whitedum)
*margins, at(manuloss7007_pw=(-130 40) whitedum=(0 1)) post
*lincom _b[4._at ] - _b[3._at ]
* White Continuous
ivreg2 per_trade totalad_hsp_log per_college70 per_male70 per_old70 comp_cont_scaled c.per_white70 i.state_fips (c.manuloss7007_pw##c.per_white70= c.bartik_iv_07##c.per_white70), cluster(FIPS) partial(i.state_fips)
eststo whitea2
estadd local state "Yes", replace
}

esttab whitea1 whitea2 using output/tablea10.tex, b(3) se(3) wrap label noomitted nobaselevels noconstant replace parentheses stats(N state, fmt(0)  labels("Observations" "State Fixed Effects"))  ///
	order(manuloss7007_pw *whitedum  *.whitedum#c.manuloss7007_pw *per_white70 per_white70#c.manuloss7007_pw per_college70 per_male70 per_old70 comp_cont_scaled totalad_hsp_log) ///
	mgroups("\shortstack{\% Trade Ads \\ (All)}" "\shortstack{\% Trade Ads \\ (All)}", pattern(1 1)) mlabels(none) ///
	booktabs nonotes ///
	interaction(" $\times$ ") ///
	starlevels( * 0.10 ** 0.05 *** 0.010) 
	
********** TABLE A17 - 2SLS - Racial Targeting of Ads - dummy + continuous 
********** Geographic Media Market Targeting of Anti-Trade Ads 2008-2016
********** Interaction between LTMD (1970-2007) and White Dummy + % White Population 1970 
ivreg2 per_trade totalad_hsp_log per_college70 per_male70 per_old70 comp_cont_scaled c.per_white70 i.state_fips (c.manuloss7007_pw##c.per_white70= c.bartik_iv_07##c.per_white70), cluster(FIPS)
margins, at(manuloss7007_pw=(-20(5)30) per_white70=(58(10)100)) post
marginsplot
	
ivreg2 per_trade_D totalad_hsp_D_log per_college70 per_male70 per_old70 comp_cont_scaled i.state_fips (c.manuloss7007_pw= c.bartik_iv_07), cluster(FIPS) partial(i.state_fips)	

ivreg2 per_trade_R totalad_hsp_R_log per_college70 per_male70 per_old70 comp_cont_scaled i.state_fips (c.manuloss7007_pw= c.bartik_iv_07), cluster(FIPS) partial(i.state_fips)	

ivreg2 per_trade_D totalad_hsp_D_log per_college70 per_male70 per_old70 comp_cont_scaled i.whitedum i.state_fips (c.manuloss7007_pw##i.whitedum= c.bartik_iv_07##i.whitedum), cluster(FIPS) partial(i.state_fips)	

ivreg2 per_trade_R totalad_hsp_R_log per_college70 per_male70 per_old70 comp_cont_scaled i.whitedum i.state_fips (c.manuloss7007_pw##i.whitedum= c.bartik_iv_07##i.whitedum), cluster(FIPS) partial(i.state_fips)			

	
** Checking for outlier effects 	
egen mperwhite= mean(per_white70)
gen tstd=mperwhite+(2*15.4701)
gen t2std=mperwhite-(2*15.4701)
gen sig = 0
replace sig =1 if per_white70<t2std | per_white70>tstd

drop if sig==1 	
	
	
	



