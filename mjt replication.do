********************************************************************************************************************************
*** Main code for "Effects of International Trade on Income Revisited", published in Review of International Economics, 2023 ***
**************************************************** Author: Jiantao Ma ********************************************************
********************************************************************************************************************************

clear all
global folder="F:\trade_Jiantao_replication"  //the folder path can be replaced
cd "$folder"



use $folder\trade_data, clear
keep if year>=1950
rename ltrade ln_trade
joinby wbcode1 wbcode2 using $folder\bilateral, unmatched(master)
drop _merge

global yeargap=5
gen year$yeargap = int(year/$yeargap)*$yeargap

drop if oilexpor1==1 | oilexpor2==1
drop if ln_dist==. | ln_canal_ps==.

*drop if wbcode1=="USA" | wbcode2=="USA" | wbcode1=="CAN" | wbcode2=="CAN"
replace ln_trade = ln_trade + ln(0.8) if p1=="USA-E" | p2=="USA-E" | p1=="CAN-E" | p2=="CAN-E"
replace ln_trade = ln_trade + ln(0.2) if p1=="USA-W" | p2=="USA-W" | p1=="CAN-W" | p2=="CAN-W"
compress

forvalues yy == 1950($yeargap)1995 {
	foreach var of varlist ln_canal_ps ln_dist {
		gen `var'_y`yy' = `var' * (`yy'==year$yeargap)
	}	
}

tab year, g(dy)
egen earliest=min(year) if ln_trade!=., by(pairid2)


*added code to avoid using trade of 1951-1955 to predict 1950
gen interval=0
replace interval=1 if inlist(year, 1950, 1955, 1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995)
keep if interval==1
drop interval
tab year


egen count = count(ln_trade) if earliest==1950 , by(pairid2)
gen balanced = (earliest==1950 & count == 10)
compress

*ensure predicted trade using pairid2 is nonmissing
egen count2= count(ln_trade), by(pairid2)
drop if count2==1 | count2==0 

save $folder\working_series, replace


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
***make_trade_predictions for equations(5a)(5b)
use $folder\working_series, clear

joinby year using F:\trade_Jiantao_replication\cpi, unmatched(master)
drop _merge

gen trade=exp(ln_trade)
gen trade2=trade
gen trade3=trade
replace trade3=. if balanced!=1
replace trade=0 if trade==.


eststo clear
*Table A1 (1)
ppmlhdfe trade ln_canal_ps_* ln_dist_* dy* dc* , cluster(pairid2)
eststo b1 
ppmlhdfe_p trade_p1 if e(sample)  
replace trade_p1=0 if trade_p1==.
tab year if trade_p1==0


*Table A1 (3)
ppmlhdfe trade ln_canal_ps_* ln_dist_* dy* dc* if trade>0 , cluster(pairid2)
eststo b2
ppmlhdfe_p trade_p1_p_p //replace trade_p1_p_p=. if tradei==. later 

*Table A1 (5)
*no extra controls -- balanced panel
ppmlhdfe trade3 ln_canal_ps_* ln_dist_* dy* dc* if balanced==1, cluster(pairid2)  //用trade3和trade一样的  //没用absorb就不用加d(fe)
eststo b3
ppmlhdfe_p trade_p1_b if e(sample)


*Table A1 (2)
*pair fixed effects -- full sample
ppmlhdfe trade ln_canal_ps_y1955-ln_canal_ps_y1995 ln_dist_y1955-ln_dist_y1995 dy* , cluster(pairid2) a(pairid2) d(fe)
eststo b4
ppmlhdfe_p trade_p4   
replace trade_p4=0 if trade_p4==.   //sum trade equals sum trade_p4


*Table A1 (4)
*pair fixed effects -- only positive panel
ppmlhdfe trade ln_canal_ps_y1955-ln_canal_ps_y1995 ln_dist_y1955-ln_dist_y1995 dy* if trade>0, cluster(pairid2) a(pairid2) d(fe2)
eststo b5
egen ffee2 = max(fe2), by(pairid2)  //! becuase pairid2 is in absorb, do not use predict. without this, will only gen prediction as if there is e(sample) 
replace fe2=ffee2 
ppmlhdfe_p trade_p4_p_p  
ppmlhdfe_p trade_p4_p if e(sample)


*Table A1 (6)
*pair fixed effects -- balanced panel
ppmlhdfe trade3 ln_canal_ps_y1955-ln_canal_ps_y1995 ln_dist_y1955-ln_dist_y1995 dy* if balanced==1, cluster(pairid2) a(pairid2) d(fe3)
eststo b6
ppmlhdfe_p trade_p4_b if e(sample)

drop fe fe2 fe3 ffee2



*impute missing trade data after estimating gravity model
areg ln_trade dy*, cluster(pairid2) a(pairid2)
predict ln_trade_dummy, d
predict ln_trade_xb, xb
egen paird = max(ln_trade_dummy),by(pairid2)  //for a pair, as long as there is a single obervation of bilateral trade, estimate for this pair generated in every year 
gen ln_tradei = ln_trade_xb+ paird
drop ln_trade_xb ln_trade_dummy paird
replace ln_tradei=ln_trade if ln_trade!=.   //replace predicted tradei if there is actual trade
gen tradei = exp(ln_tradei)



keep trad* wbcode1 wbcode2 year* ln_canal_ps ln_dist pairid2
save F:\trade_Jiantao_replication\temp, replace
rename wbcode1 wbcode3
rename wbcode2 wbcode1
rename wbcode3 wbcode2
append using $folder\temp

compress
save $folder\bilateral_predict, replace


***from bilateral to aggreagted trade
use $folder\bilateral_predict, clear
rename wbcode1 wbcode
replace trade_p1_p_p=. if tradei==. 

global yeargap=5
keep if mod(year,$yeargap)==0

collapse (sum) trade* (count) count = pairid2 ,by(wbcode year year$yeargap)
foreach var of varlist trade* {   
	gen ln_`var' = ln(`var')
}

*join in the country year level stuff (from PWT)
joinby wbcode year using $folder\panel_vars, unmatched(master)
drop _merge

compress
save $folder\working, replace
//////////////////////////////////////////////////////////////////////////////////

*gen indicators to indicate same countries for 1950-1970 and 1975-1995
use $folder\working, clear

ivreghdfe ln_gdpc (ln_trade=ln_trade_p1) if year<=1970, absorb(wbcode year)
gen sample_zero=1 if e(sample)
ivreghdfe ln_gdpc (ln_tradei=ln_trade_p1_p_p)  if year<=1970, absorb(wbcode year)
gen sample_positive=1 if e(sample)
ivreghdfe ln_gdpc (ln_trade3=ln_trade_p1_b)  if year<=1970, absorb(wbcode year)
gen sample_balanced=1 if e(sample)

replace sample_zero=0 if sample_zero==.
replace sample_positive=0 if sample_positive==.
replace sample_balanced=0 if sample_balanced==.
keep wbcode year sample_*

egen sample_zero_sum= sum(sample_zero), by(wbcode)
egen sample_positive_sum= sum(sample_positive), by(wbcode)
egen sample_balanced_sum= sum(sample_balanced), by(wbcode)

count if sample_zero_sum==1   
count if sample_positive_sum==1
count if sample_balanced_sum==1


keep wbcode  sample_zero_sum sample_positive_sum sample_balanced_sum
duplicates drop
replace sample_zero_sum=1 if sample_zero_sum>0
replace sample_positive_sum=1 if sample_positive_sum>0
replace sample_balanced_sum=1 if sample_balanced_sum>0

rename sample_zero_sum zero
rename sample_positive_sum positive
rename sample_balanced_sum balanced

save $folder\sample_select, replace


//////////////////////////////////////////////////////////////////////////////////////
********Table 2 *************
*small sample adjustment, adjusted DoF of cluster)
//by unique_id: drop if _N<2
use $folder\working, clear
merge n:1 wbcode using $folder\sample_select

***Panel B***
eststo clear
ivreghdfe ln_gdpc (ln_trade=ln_trade_p1) if year<=1970 & zero==1, cluster(wbcode) absorb(wbcode year) first 
eststo b1
estadd scalar FF = e(widstat) 
estadd scalar countries=e(N_clust)

ivreghdfe ln_gdpc (ln_trade=ln_trade_p4)  if year<=1970 & zero==1, cluster(wbcode) absorb(wbcode year) first
eststo b2
estadd scalar FF = e(widstat) 
estadd scalar countries=e(N_clust)

ivreghdfe ln_gdpc (ln_tradei=ln_trade_p1_p_p) if year<=1970 & positive==1, cluster(wbcode) absorb(wbcode year) first 
eststo b3
estadd scalar FF = e(widstat) 
estadd scalar countries=e(N_clust)

ivreghdfe ln_gdpc (ln_tradei=ln_trade_p4_p_p)  if year<=1970 & positive==1, cluster(wbcode) absorb(wbcode year) first 
eststo b4
estadd scalar FF = e(widstat)  
estadd scalar countries=e(N_clust) 

ivreghdfe ln_gdpc (ln_trade3=ln_trade_p1_b) if year<=1970 & balanced==1, cluster(wbcode) absorb(wbcode year) first
eststo b5
estadd scalar FF = e(widstat)
estadd scalar countries=e(N_clust) 

ivreghdfe ln_gdpc (ln_trade3=ln_trade_p4_b) if year<=1970 & balanced==1, cluster(wbcode) absorb(wbcode year) first
eststo b6
estadd scalar FF = e(widstat)   
estadd scalar countries=e(N_clust)

#delimit ;
esttab b1 b2 b3 b4 b5 b6 using $folder\result.tex, replace
	nomtitles  
	label style(tex)
	cells(b( star fmt(3 3 3) ) se(par fmt(3)))   
	se star(* 0.1 ** 0.05 *** 0.01)
	stats( FF N countries, fmt(%9.2f %9.0fc %9.0fc) labels( "Instrument F"  "Observations" "Countries"))
	title(Estimates of Trade on Per Capita GDP)
;	
#delimit cr


***Panel C***
eststo clear
ivreghdfe ln_gdpc (ln_trade=ln_trade_p1)  if year>=1975 & zero==1 , cluster(wbcode) absorb(wbcode year) first
eststo b1
estadd scalar FF = e(widstat) 
estadd scalar countries=e(N_clust)

ivreghdfe ln_gdpc (ln_trade=ln_trade_p4) if year>=1975 & zero==1 , cluster(wbcode) absorb(wbcode year) first
eststo b2
estadd scalar FF = e(widstat) 
estadd scalar countries=e(N_clust)


ivreghdfe ln_gdpc (ln_tradei=ln_trade_p1_p_p)  if year>=1975 & positive==1 , cluster(wbcode) absorb(wbcode year) first  
eststo b3
estadd scalar FF = e(widstat) 
estadd scalar countries=e(N_clust)

ivreghdfe ln_gdpc (ln_tradei=ln_trade_p4_p_p)  if year>=1975 & positive==1 , cluster(wbcode) absorb(wbcode year) first
eststo b4
estadd scalar FF = e(widstat)  
estadd scalar countries=e(N_clust) 

ivreghdfe ln_gdpc (ln_trade3=ln_trade_p1_b)  if year>=1975 & balanced==1 , cluster(wbcode) absorb(wbcode year) first
eststo b5
estadd scalar FF = e(widstat)
estadd scalar countries=e(N_clust) 


ivreghdfe ln_gdpc (ln_trade3=ln_trade_p4_b)  if year>=1975 & balanced==1 , cluster(wbcode) absorb(wbcode year) first
eststo b6
estadd scalar FF = e(widstat)   
estadd scalar countries=e(N_clust)


#delimit ;
esttab b1 b2 b3 b4 b5 b6 using $folder\result.tex, replace
	nomtitles  
	label style(tex)
	cells(b( star fmt(3 3 3) ) se(par fmt(3)))   
	se star(* 0.1 ** 0.05 *** 0.01)
	stats( FF N countries, fmt(%9.2f %9.0fc %9.0fc) labels( "Instrument F"  "Observations" "Countries"))
	title(Estimates of Trade on Per Capita GDP)
;	
#delimit cr


***Panel A***
eststo clear
ivreghdfe ln_gdpc (ln_trade=ln_trade_p1)  if zero==1 , cluster(wbcode) absorb(wbcode year) first
eststo b1
estadd scalar FF = e(widstat) 
estadd scalar countries=e(N_clust)

ivreghdfe ln_gdpc (ln_trade=ln_trade_p4)  if zero==1 , cluster(wbcode) absorb(wbcode year) first
eststo b2
estadd scalar FF = e(widstat) 
estadd scalar countries=e(N_clust)

ivreghdfe ln_gdpc (ln_tradei=ln_trade_p1_p_p)  if positive==1 , cluster(wbcode) absorb(wbcode year) first  
eststo b3
estadd scalar FF = e(widstat) 
estadd scalar countries=e(N_clust)

ivreghdfe ln_gdpc (ln_tradei=ln_trade_p4_p_p)  if positive==1 , cluster(wbcode) absorb(wbcode year) first
eststo b4
estadd scalar FF = e(widstat)  
estadd scalar countries=e(N_clust) 

ivreghdfe ln_gdpc (ln_trade3=ln_trade_p1_b)  if balanced==1 , cluster(wbcode) absorb(wbcode year) first
eststo b5
estadd scalar FF = e(widstat)
estadd scalar countries=e(N_clust) 

ivreghdfe ln_gdpc (ln_trade3=ln_trade_p4_b)  if balanced==1 , cluster(wbcode) absorb(wbcode year) first 
eststo b6
estadd scalar FF = e(widstat)   
estadd scalar countries=e(N_clust)


#delimit ;
esttab b1 b2 b3 b4 b5 b6 using $folder\result.tex, replace
	nomtitles  
	label style(tex)
	cells(b( star fmt(3 3 3) ) se(par fmt(3)))   
	se star(* 0.1 ** 0.05 *** 0.01)
	stats( FF N countries, fmt(%9.2f %9.0fc %9.0fc) labels( "Instrument F"  "Observations" "Countries"))
	title(Estimates of Trade on Per Capita GDP)
;	
#delimit cr


