clear
cd "C:\Users\Himani\OneDrive\Documents\Reading Material\Econometrics\Eco trix2"

use assign2

***Exercise 2 a)
*** generating fourth order polynomials for age and year of birth

 
gen age2 = age*age
gen age3 = age*age*age
gen age4 = age*age*age*age

gen yob2 = yob*yob
gen yob3 = yob*yob*yob
gen yob4 = yob*yob*yob*yob

reg logearn schooling age age2 age3 age4 yob yob2 yob3 yob4, robust

*** Exercise 2 b)



gen dummy = 1 if schooling<15
replace dummy = 0 if dummy==.
probit dummy i.yob, robust
predict p
twoway scatter p yob  if schooling<15 ,xline(33)
graph export "C:\Users\Himani\OneDrive\Documents\Reading Material\Econometrics\Eco trix2\Graph_c(i).png", as(png) name("Graph")

**binscatter plots
ssc install binscatter

binscatter schooling yob, xline(33)
ssc install binscatter
binscatter logearn yob, xline(33)


***** Exercise 2 d)

gen policy = 1 if yob>33
replace policy = 0 if policy ==.

*manually computing Wald Estimator

mean schooling if policy ==1
mean schooling if policy ==0

mean logearn if policy ==1
mean logearn if policy ==0


** using ivreg

ivreg logearn (schooling = policy), robust


***** Exercise 2 e)


reg schooling policy age age2 age3 age4 yob yob2 yob3 yob4 ,robust

reg logearn policy age age2 age3 age4 yob yob2 yob3 yob4 ,robust


*Separately estimate the first and second stage.

reg schooling policy age age2 age3 age4 yob yob2 yob3 yob4, robust 
predict schoolingestimate, xb

reg logearn schoolingestimate age age2 age3 age4 yob yob2 yob3 yob4, robust

*Estimate the model with an inbuilt 2SLS command
ivregress 2sls logearn (schooling=policy) age age2 age3 age4 yob yob2 yob3 yob4, robust
*Coeff don´t match

*PART OF TESTS

ssc install ivreg2
ssc install ranktest
ivreg2 logearn (schooling=policy), robust

ivregress 2sls schooling policy age age2 age3 age4 yob yob2 yob3 yob4, robust
test law

