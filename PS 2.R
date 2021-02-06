library(tidyverse)
library(haven)
library(binsreg)
library(stargazer)
library(ivreg)
library(lmtest)
library(sandwich)
assign2 <- read_dta("C:/Users/btpta/Desktop/Metrics 2/PS 2/assign2.dta")
View(assign2)
ggplot(assign2, aes( x=logearn, y=schooling)) +geom_point()     #first visual inspection
lm_1 <-lm (logearn ~ schooling + I(age^4)+ I(age^3)+ I(age^2)+ age + I(yob^4) + I(yob^3) + I(yob^2) + yob, assign2)   #run OLS 
summary(lm_1)
stargazer(lm_1, title="OLS Regression Results",
          dep.var.labels=c("Earnings"), 
          covariate.labels= c("Schooling", "Age^4", "Year of birth^4"),
          omit.stat=c("LL","ser"), no.space=TRUE)

assign2 <- assign2 %>%
  add_column (LAW = if_else(.$yob<33, 0, 1),
              leaveschool = if_else(.$schooling<15, 1, 0))      #creat dummy for reform(yob<33) and schooling (<15)      

pmodel <- glm (leaveschool ~ as.factor(yob), data = assign2, family=binomial)   #Plot the probability that a person leaves school before age 15 against the year of birth.
summary(pmodel)
range(assign2$yob)
x <- seq(21, 45, 1)
y <- predict(pmodel,list(yob=x), type="response")
plot(c(21,45), c(0,1), type="n", xlab= "Year of birth", ylab = "Prob. of leaving school<15")
lines(x,y, type="p")
abline(v=33, col="red")

binsreg (assign2$schooling, assign2$yob)  #Binscatters
abline(v=33, col="blue")
binsreg (assign2$logearn, assign2$yob)
abline(v=33, col="red")

# the Wald estimator 
meanschol<-by(data=assign2$schooling, INDICES=assign2$LAW, FUN=mean)
meanearn<-by(data=assign2$logearn, INDICES=assign2$LAW, FUN=mean)
Wald <- (meanearn[2]-meanearn[1])/(meanschol[2]-meanschol[1])


#IV estimation
iv_nocontrols<-ivreg(logearn~schooling|LAW, data = assign2)
stargazer(iv_nocontrols, title="IV Regression Results without controlls",
          dep.var.labels=c("Earnings"), 
          covariate.labels= c("Schooling"),
          omit.stat=c("LL","ser"), no.space=TRUE)

#IV estimation with controlls

#first stage and reduced form
lm_2 <-lm (schooling~LAW + I(age^4)+ I(age^3)+ I(age^2)+ age + I(yob^4) + I(yob^3) + I(yob^2) + yob, assign2) #1st stage
scholHat<- fitted(lm_2)
summary(lm_2)

lm_reduced <-lm (logearn~LAW + I(age^4)+ I(age^3)+ I(age^2)+ age + I(yob^4) + I(yob^3) + I(yob^2) + yob, assign2) #1st stage

summary(lm_reduced)

cov1        <- vcovHC(lm_2, type = "HC0")
robust1     <- sqrt(diag(cov1))
cov2       <- vcovHC(lm_reduced, type = "HC0")
robust_reduced     <- sqrt(diag(cov2))

stargazer(lm_2,lm_reduced, title="First Stage and Reduced Form with controlls",
          align=TRUE, dep.var.labels=c("Schooling","Earnings"), 
          covariate.labels= c("Reform", "Age^4", "Age^3","Age^2","Age", "Year of birth^4", "Year of birth^3", "Year of birth^2",  "Year of birth"),
          omit.stat=c("LL","ser"), no.space=TRUE, se = list(robust1, robust_reduced))



# Second stage 
lm_3 <- lm(logearn~scholHat + I(age^4)+ I(age^3)+ I(age^2)+ age + I(yob^4) + I(yob^3) + I(yob^2) + yob, assign2)

summary(lm_3)


#in-buid function ( I've delited a couple of terms from first part of equestion beacuse it didn't work due to linearly dependent columns, i.e. strongly correlated variables)

ivmodel <- ivreg (logearn ~ schooling + I(age^4)+  I(age^2)+ age + I(yob^4) + I(yob^3) + yob | LAW + I(age^4)+ I(age^3)+ I(age^2)+ age + I(yob^4) + I(yob^3) + I(yob^2) + yob, data = assign2)
summary(ivmodel)




cov3        <- vcovHC(lm_3, type = "HC0")
robust3     <- sqrt(diag(cov3))
cov4       <- vcovHC(ivmodel, type = "HC0")
robust4    <- sqrt(diag(cov4))

stargazer(lm_2,lm_3,ivmodel, title="Fisrt, Second Stage and ireg results with controlls",
          align=TRUE, dep.var.labels=c("Schooling","Earnings","Earnings"), 
          covariate.labels= c("Reform", "Fitted Schooling","Schooling", "Age^4", "Age^3","Age^2","Age", "Year of birth^4", "Year of birth^3", "Year of birth^2",  "Year of birth"),
          omit.stat=c("LL","ser"), no.space=TRUE, se = list(robust1, robust3, robust4))




