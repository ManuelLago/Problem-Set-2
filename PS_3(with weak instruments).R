install.packages("AER")
library (AER) 
library (MASS)

alpha <- 0
beta <-1

#simulation
set.seed (666)
reps <- 10000
n<- c ( 50 , 100 , 250 , 1000 )

betaOLS <- matrix (0, nrow = reps, ncol = length (n))
betaIV <- matrix (0, nrow = reps, ncol = length(n))

#strong instruments

for (i in 1:length (n)) {
  
  for ( j in 1: reps) {
    
    w <- mvrnorm (n[i], c (0,0,0), matrix (c (1, 0.5, 0.4, 0.5, 1, 0, 0.4, 0, 1), 3, 3))
    x <- w[,1]
    z <- w[,2]
    epsilon <- w[,3]
    
    y <- alpha + beta*x + epsilon
    
    # OLS estimation
    betaOLS [j, i] <- lm(y~x )$coefficients[2]
    
    # IV estim a ti o n
    betaIV [j, i] <-ivreg (y~ x|z)$coefficients[2]
  }
}

graphics.off()
par(mar=c(3,1,1,3), mfrow=c(2,2))

OLShist<-hist (betaOLS [1:1000, 1], breaks ="Scott", prob = T, xlim = c(mean(betaOLS [1:1000, 1])-1.5, 
                                                                            mean(betaOLS [1:1000,1])+0.5), 
                                                                          main = "", xlab = "", col=rgb (0.8, 0.8, 0.8, 0.5))
IVhist<- hist (betaIV [1:1000, 1], breaks = "Scott", prob = T, add = T, col=rgb (0.8, 0.8, 0.8, 0.5))
lines (density (betaIV [1:1000,1]))
lines (density (betaOLS [1:1000, 1]))
legend ("topleft", c("OLS","IV"), fill=c(rgb (0.8, 0.8, 0.8, 0.5), rgb(0.1, 0.1, 0.1, 0.5)), cex=0.5)
box ( )
mtext (paste0 ("N=", n[1]), side=1, line=2)


OLShist<-hist (betaOLS [1:1000, 2], breaks ="Scott", prob = T, xlim = c(mean(betaOLS [1:1000, 2])-1.5, 
                                                                            mean(betaOLS [1:1000,2])+0.5), 
                                                                        main = "", xlab = "", col=rgb (0.8, 0.8, 0.8, 0.5))
IVhist<- hist (betaIV [1:1000, 2], breaks = "Scott", prob = T, add = T, col=rgb (0.8, 0.8, 0.8, 0.5))
lines (density (betaIV [1:1000,2]))
lines (density (betaOLS [1:1000, 2]))
legend ("topleft", c("OLS","IV"), fill=c(rgb (0.8, 0.8, 0.8, 0.5), rgb(0.1, 0.1, 0.1, 0.5)), cex=0.5)
box ( )
mtext (paste0 ("N=", n[2]), side=1, line=2)


OLShist<-hist (betaOLS [1:1000, 3], breaks ="Scott", prob = T, xlim = c(mean(betaOLS [1:1000, 3])-1.5, 
                                                                            mean(betaOLS [1:1000,3])+0.5), 
               main = "", xlab = "", col=rgb (0.8, 0.8, 0.8, 0.5))
IVhist<- hist (betaIV [1:1000, 3], breaks = "Scott", prob = T, add = T, col=rgb (0.8, 0.8, 0.8, 0.5))
lines (density (betaIV [1:1000,3]))
lines (density (betaOLS [1:1000, 3]))
legend ("topleft", c("OLS","IV"), fill=c(rgb (0.8, 0.8, 0.8, 0.5), rgb(0.1, 0.1, 0.1, 0.5)), cex=0.5)
box ( )
mtext (paste0 ("N=", n[3]), side=1, line=2)

OLShist<-hist (betaOLS [1:1000, 4], breaks ="Scott", prob = T, xlim = c(mean(betaOLS [1:1000, 4])-1.5, 
                                                                            mean(betaOLS [1:1000,4])+0.5), 
               main = "", xlab = "", col=rgb (0.8, 0.8, 0.8, 0.5))
IVhist<- hist (betaIV [1:1000, 4], breaks = "Scott", prob = T, add = T, col=rgb (0.8, 0.8, 0.8, 0.5))
lines (density (betaIV [1:1000,4]))
lines (density (betaOLS [1:1000, 4]))
legend ("topleft", c("OLS","IV"), fill=c(rgb (0.8, 0.8, 0.8, 0.5), rgb(0.1, 0.1, 0.1, 0.5)), cex=0.5)
box ( )
mtext (paste0 ("N=", n[4]), side=1, line=2)

#weak instruments

for (i in 1:length (n)) {
  
  for ( j in 1: reps) {
    
    w <- mvrnorm (n[i], c (0,0,0), matrix (c (1, 0.15, 0.4, 0.15, 1, 0, 0.4, 0, 1), 3, 3))
    x <- w[,1]
    z <- w[,2]
    epsilon <- w[,3]
    
    y <- alpha + beta*x + epsilon
    
    # OLS e s tim a ti o n
    betaOLS [j, i] <- lm(y~x )$coefficients[2]
    
    # IV e s tim a ti o n
    betaIV [j, i] <-ivreg (y~ x|z)$coefficients[2]
  }
}


par(mar=c(3,1,1,3), mfrow=c(2,2))

OLShist<-hist (betaOLS [1:1000, 1], breaks ="Scott", prob = T, xlim = c(mean(betaOLS [1:1000, 1])-1.5, 
                                                                        mean(betaOLS [1:1000,1])+0.5), 
               main = "", xlab = "", col=rgb (0.8, 0.8, 0.8, 0.5))
IVhist<- hist (betaIV [1:1000, 1], breaks = "Scott", prob = T, add = T, col=rgb (0.8, 0.8, 0.8, 0.5))
lines (density (betaIV [1:1000,1]))
lines (density (betaOLS [1:1000, 1]))
legend ("topleft", c("OLS","IV"), fill=c(rgb (0.8, 0.8, 0.8, 0.5), rgb(0.1, 0.1, 0.1, 0.5)), cex=0.5)
box ( )
mtext (paste0 ("N=", n[1]), side=1, line=2)


OLShist<-hist (betaOLS [1:1000, 2], breaks ="Scott", prob = T, xlim = c(mean(betaOLS [1:1000, 2])-1.5, 
                                                                        mean(betaOLS [1:1000,2])+0.5), 
               main = "", xlab = "", col=rgb (0.8, 0.8, 0.8, 0.5))
IVhist<- hist (betaIV [1:1000, 2], breaks = "Scott", prob = T, add = T, col=rgb (0.8, 0.8, 0.8, 0.5))
lines (density (betaIV [1:1000,2]))
lines (density (betaOLS [1:1000, 2]))
legend ("topleft", c("OLS","IV"), fill=c(rgb (0.8, 0.8, 0.8, 0.5), rgb(0.1, 0.1, 0.1, 0.5)), cex=0.5)
box ( )
mtext (paste0 ("N=", n[2]), side=1, line=2)


OLShist<-hist (betaOLS [1:1000, 3], breaks ="Scott", prob = T, xlim = c(mean(betaOLS [1:1000, 3])-1.5, 
                                                                        mean(betaOLS [1:1000,3])+0.5), 
               main = "", xlab = "", col=rgb (0.8, 0.8, 0.8, 0.5))
IVhist<- hist (betaIV [1:1000, 3], breaks = "Scott", prob = T, add = T, col=rgb (0.8, 0.8, 0.8, 0.5))
lines (density (betaIV [1:1000,3]))
lines (density (betaOLS [1:1000, 3]))
legend ("topleft", c("OLS","IV"), fill=c(rgb (0.8, 0.8, 0.8, 0.5), rgb(0.1, 0.1, 0.1, 0.5)), cex=0.5)
box ( )
mtext (paste0 ("N=", n[3]), side=1, line=2)

OLShist<-hist (betaOLS [1:1000, 4], breaks ="Scott", prob = T, xlim = c(mean(betaOLS [1:1000, 4])-1.5, 
                                                                        mean(betaOLS [1:1000,4])+0.5), 
               main = "", xlab = "", col=rgb (0.8, 0.8, 0.8, 0.5))
IVhist<- hist (betaIV [1:1000, 4], breaks = "Scott", prob = T, add = T, col=rgb (0.8, 0.8, 0.8, 0.5))
lines (density (betaIV [1:1000,4]))
lines (density (betaOLS [1:1000, 4]))
legend ("topleft", c("OLS","IV"), fill=c(rgb (0.8, 0.8, 0.8, 0.5), rgb(0.1, 0.1, 0.1, 0.5)), cex=0.5)
box ( )
mtext (paste0 ("N=", n[4]), side=1, line=2)

