use $folder\working_series, clear

***Figure 1: figure for proportion of missing value per year
use $folder\working_series, clear
bys year: egen count_n=count(_n)
bys year: egen count_m=total(missing(ln_trade))
keep year count_n count_m
duplicates drop
gen proportion=count_m/count_n
graph bar proportion, over(year) b1title("Year") ytitle("Proportion of Missing Bilateral Trade")


***Figure 5:OLS fitted residual: OLS trade residulas vs. predicted log trade flow
use $folder\working_series, clear

reg ln_trade ln_canal_ps_* ln_dist_* dy* dc*, robust // hetero robust is default
predict trade_hat if e(sample) //这里e(sample)不起作用，因为当用整个1950-1997样本时，没有两国间trade全部missing的
gen resid=ln_trade-trade_hat if e(sample)
gen residvar=resid^2

rename ln_trade trade
label var resid "Fitted Residual"
label var trade "Trade (million)"
binscatter residvar trade_hat, xtitle(Predicted Log Trade Flows) ytitle(Residual Square)



***Figure 2***
use $folder\working_series, clear

gen trade=exp(ln_trade)
gen trade2=trade
gen trade3=trade
replace trade3=. if balanced!=1
replace trade=0 if trade==.

ppmlhdfe trade ln_canal_ps_* ln_dist_* dy* dc* , cluster(pairid2)

gen yyear=.
gen coe_canal=.
gen se_canal=.
gen coe_air=.
gen se_air=.
local a=0
forvalues i=1950(5)1995{
local a=`a'+1       
replace coe_canal=_b[ln_canal_ps_y`i'] in `a'
replace se_canal=_se[ln_canal_ps_y`i'] in `a'
replace coe_air=_b[ln_dist_y`i'] in `a'
replace se_air=_se[ln_dist_y`i'] in `a'
replace yyear=`i' in `a'
}
rename coe_canal sea
rename coe_air air
label var yyear "year"

gen u_canal=sea+se_canal*1.96
gen l_canal=sea-se_canal*1.96
gen u_air=air+se_air*1.96
gen l_air=air-se_air*1.96
twoway (connected sea yyear) (rcap u_canal l_canal yyear) (connected air yyear, msymbol(triangle)) (rcap u_air l_air yyear) , ///
xlabel(1950(10)2000) ylabel(-1.5(0.5)0.5)  graphregion(color(white)) graphregion(margin(zero)) ///
plotregion(margin(tiny)) xsize(7)  saving(dist1,replace) graphregion(margin(l+10 r+10 t+8)) xtitle(Year) ytitle(Elasticity of Trade with Respect to Distance) legend(order(1 "Sea" 3 "Air"))


***Figure 3***
use $folder\working_series, clear

gen trade=exp(ln_trade)
gen trade2=trade
gen trade3=trade
replace trade3=. if balanced!=1
replace trade=0 if trade==.

ppmlhdfe trade ln_canal_ps_y1955-ln_canal_ps_y1995 ln_dist_y1955-ln_dist_y1995 dy* , cluster(pairid2) a(pairid2) d(fe)

gen yyear=.
gen coe_canal=.
gen se_canal=.
gen coe_air=.
gen se_air=.
replace coe_canal=0 in 1
replace coe_air=0 in 1
replace yyear=1950 in 1
local a=1
forvalues i=1955(5)1995{
local a=`a'+1       
replace coe_canal=_b[ln_canal_ps_y`i'] in `a'
replace se_canal=_se[ln_canal_ps_y`i'] in `a'
replace coe_air=_b[ln_dist_y`i'] in `a'
replace se_air=_se[ln_dist_y`i'] in `a'
replace yyear=`i' in `a'
}
rename coe_canal sea
rename coe_air air
label var yyear "year"

gen u_canal=sea+se_canal*1.96
gen l_canal=sea-se_canal*1.96
gen u_air=air+se_air*1.96
gen l_air=air-se_air*1.96
twoway (connected sea yyear) (rcap u_canal l_canal yyear)  (connected air yyear, msymbol(triangle)) (rcap u_air l_air yyear) , ///
 aspectratio(0.4) graphregion(color(white)) graphregion(margin(zero)) ///
plotregion(margin(tiny)) xsize(8) saving(dist2,replace) graphregion(margin(l+10 r+10 t+8)) xtitle(Year) ytitle(Elasticity of Trade with Respect to Distance) legend(order(1 "Sea" 3 "Air"))



***Figure 4 ***
*draw distribution
use $folder\bilateral_predict, clear
keep if trade==0
replace trade_p1=trade_p1/1000000      //scale
replace trade_p1_p_p=trade_p1_p_p/1000000 
label var trade_p1 "Zero bilateral trade"
label var trade_p1_p_p "Missing bilateral trade"


twoway (kdensity trade_p1 if trade_p1<1 , lpattern(dashed) lcolor(green) ) || (kdensity trade_p1_p_p if trade_p1_p_p<1), xtitle(Value of Fitted Bilateral Trade (Million)) ytitle(Probability Density) graphregion(margin(l+5 r+5 t+5))





