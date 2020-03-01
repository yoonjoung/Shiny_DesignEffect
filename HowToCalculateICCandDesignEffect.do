clear 
clear matrix
clear mata
set more off
set mem 300m
set maxvar 9000

cd "C:\Users\YoonJoung Choi\Dropbox\0 Project\Shiny_DesignEffect"
global data "C:\Users\YoonJoung Choi\Dropbox\0 Data\DHS"
/*
https://www.stata.com/manuals/svyglossary.pdf

DEFF and DEFT. DEFF and DEFT are design effects. Design effects compare the sample-to-sample
variability from a given survey dataset with a hypothetical SRS design with the same number of
individuals sampled from the population.
DEFF is the ratio of two variance estimates. The design-based variance is in the numerator; the
hypothetical SRS variance is in the denominator.
DEFT is the ratio of two standard-error estimates. The design-based standard error is in the
numerator; the hypothetical SRS with-replacement standard error is in the denominator. If the given
survey design is sampled with replacement, DEFT is the square root of DEFF.
*/
*********************************** Data list
#delimit;
global datalist "

	BurkinaFaso_1992
	BurkinaFaso_1998
	
	BurkinaFaso_2010

	Ethiopia_2000
	Ethiopia_2005
	Ethiopia_2011
	Ethiopia_2016
		
	Kenya_1993
	Kenya_1998
	Kenya_2003
	Kenya_2008
	Kenya_2014
	
	Niger_1992
	Niger_1998
	Niger_2006
	Niger_2012

	Uganda_1995
	Uganda_2000
	Uganda_2006

	Uganda_2016
	
	"; 
	#delimit cr

/*
surveys without v022: 
	BurkinaFaso_2003
	Uganda_2011	
*/	
************************************************************
* Calculate DEFF and construct summary dataset 
************************************************************
use "$data\\IR_Mali_2018.dta", clear	

	d v005 v021 v022

	gen xsurvey="Mali_2018"
	gen wt=v005/1000000
	gen mcpr		=v313==3
	gen edupricomp	=v149>=2
	gen married18	=v511<18
		replace married18 =. if v012<18
		
	svyset v021  [pw=wt], str(v022) singleunit(centered)
		
	foreach var of varlist mcpr edupricomp married18{
		svy: prop `var'
		estat effects, deff	
			scalar deff`var'=el(e(deff),1,1)
			gen deff_`var'=deff`var'
		}	

	foreach var of varlist mcpr edupricomp married18{		
		loneway `var' v021 [aw=wt]
		scalar rho`var'=r(rho)		
		gen icc_`var'=rho`var'
	}	
	
	collapse (mean) deff* icc*, by(xsurvey)
	save SummaryICC.dta, replace 

foreach ctry_yr in $datalist{
use "$data\\IR_`ctry_yr'.dta", clear	

	tab v000 v007

	gen xsurvey="`ctry_yr'"
	gen wt=v005/1000000
	gen mcpr		=v313==3
	gen edupricomp	=v149>=2
	gen married18	=v511<18
		replace married18 =. if v012<18
		
	svyset v021  [pw=wt], str(v022) singleunit(centered)
	
	foreach var of varlist mcpr edupricomp married18{
		svy: prop `var'
		estat effects, deff	
			scalar deff`var'=el(e(deff),1,1)
			gen deff_`var'=deff`var'
		}	

	foreach var of varlist mcpr edupricomp married18{		
		loneway `var' v021 [aw=wt]
		scalar rho`var'=r(rho)		
		gen icc_`var'=rho`var'
	}	
	
	collapse (mean) deff* icc*, by(xsurvey)
	append using SummaryICC.dta, 
	save SummaryICC.dta, replace
}	

use SummaryICC.dta, clear
	
	rename 	deff_edu 	 deff1
	rename 	deff_mcpr 	 deff2
	rename 	deff_married deff3
	
	rename 	icc_edu 	 icc1
	rename 	icc_mcpr 	 icc2
	rename 	icc_married  icc3	
	
	reshape long deff icc, i(xsurvey) j(indicator)
	gen indicatorname=""
		replace indicatorname="% women completed primary school" if indicator==1
		replace indicatorname="% women using modern contraceptives" if indicator==2
		replace indicatorname="% women married by age 18" if indicator==3
		tab indicatorname indicator, m
		
	gen year=substr(xsurvey, -4, .)		
		tab year
		destring(year), replace
		
		gen temp=strreverse(xsurvey) 
		replace temp=substr(temp,6,.)
	gen country=strreverse(temp)
		drop temp*
		
		egen temp=max(year), by(country)
	gen latest=year==temp
		tab xsurvey latest, m
		drop temp*
	/*	
	histogram deff	, w(0.2)
	histogram deff	, w(0.2) by(indicatorname, col(1))
	histogram deff if latest==1	, w(0.2) by(indicatorname, col(1))
	*/

	keep if latest==1
	keep country xsurvey indicatorname deff icc
	rename indicatorname indicator
	
	format icc %4.3f
	format deff %4.1f
	
save ICCfromDHS.dta, replace
	
export delimited using ICCfromDHS.csv, replace
	
GREAT		
		
/*	
************************************************************
* Investigation: v022 vs. v023
************************************************************

/*
Some surveys do not have v022, but v023. 
Some surveys have neighter. 
Compare DEFF estimates with v022 vs. v02, using surveys that have different values for the two var
*/ 

*1. find candidate surveys.  
foreach ctry_yr in $satalist{
use "$data\\IR_`ctry_yr'.dta", clear	

	tab v000 v007
	sum v021 v022 v023
}	

*1. compare DEFF estimates 
foreach ctry_yr in Kenya_2008{
use "$data\\IR_`ctry_yr'.dta", clear	

	tab v000 v007
	sum v021 v022 v023
	codebook v021 v022 v023
	
	gen xsurvey="`ctry_yr'"
	gen wt=v005/1000000
	gen mcpr		=v313==3
	gen edupricomp	=v149>=2
	gen married18	=v511<18
		replace married18 =. if v012<18
		
	svyset v021  [pw=wt], str(v022) singleunit(centered)
		svy: prop mcpr
		estat effects, deff	

	svyset v021  [pw=wt], str(v023) singleunit(centered)
		svy: prop mcpr
		estat effects, deff	
			
	}
