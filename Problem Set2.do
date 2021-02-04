*a)))OLS
ssc install outreg2

gen age2 = age^2
gen age3 = age^3
gen age4 = age^4
gen yob2 = yob^2
gen yob3 = yob^3
gen yob4 = yob^4

reg logearn schooling age age2 age3 age4 yob yob2 yob3 yob4, robust
*change the outreg2 exporting it to latex instead
outreg2 using myreg.doc, append ctitle(Model1)

*b

gen law = 0
replace law=1 if (yob>=33)

*Think theoretical approach

*c)

*Plot probablity leaving school before 15 against year of birth

gen leaves = 0
replace leaves=1 if (schooling<15)
scatter leaves yob, xline(33)


*Bisncatter of schooling and year birth

ssc install binscatter
binscatter schooling yob, xline(33)
*Binscatter of log earnings and year birth


binscatter logearn yob, xline(33)

*d)Calculate Wald estimator by hand with no controls (conditional averages)
*E(D/Z=1)
global iv law
egen x1mean = mean(schooling / ($iv==1)) 

*E(D/Z=0)
egen x0mean = mean(schooling / ($iv==0)) 

*E(Y/Z=1)
egen y1mean = mean(logearn / ($iv==1)) 

*E(Y/Z=0)
egen y0mean = mean(logearn / ($iv==0)) 


sum x0mean x1mean y1mean y0mean
gen wald = ((y1mean - y0mean)/(x1mean - x0mean))
sum wald


ivregress 2sls logearn (schooling=law), robust
*
*Results are the same...should we include other controls when comparing with ols?


*e)
*1ST STAGE
reg schooling law age age2 age3 age4 yob yob2 yob3 yob4, robust
outreg2 using myreg.doc, append ctitle(Model2)
gen delta = _b[law]

*REDUCED FORM

reg logearn law age age2 age3 age4 yob yob2 yob3 yob4, robust
outreg2 using myreg.doc, append ctitle(Model3)
gen lambda =_b[law]

gen ivestimate = lambda/delta 
sum ivestimate


*Separately estimate the first and second stage.

reg schooling law age age2 age3 age4 yob yob2 yob3 yob4, robust
predict schoolingestimate, xb


reg logearn schoolingestimate age age2 age3 age4 yob yob2 yob3 yob4, robust
outreg2 using myreg.doc, append ctitle(Model3)

*Estimate the model with an inbuilt 2SLS command
ivregress 2sls logearn (schooling=law) age age2 age3 age4 yob yob2 yob3 yob4, robust
*Coeff don´t match

*CHECK VALIDITY
reg schooling law age age2 age3 age4 yob yob2 yob3 yob4, robust
test law
