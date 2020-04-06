##############################################################
# This file contains functions for calculating the number of #
# persons that could be included for a given test "budget"   #
# and the expected number of positiv cases idetified         #
# for two different pooling strategies.                      #
##############################################################


# Calculate the probability of positive pool for 
# prevalence p, pool size k and sensitivity s.
poolprob <- function(p,k,s)
  (1-(1-p)^k)*s


#####################################################
## Pool size 8. Test all in pool if pool positive.
#####################################################

sens8 <- 0.99 # Assumed sensitivity for pool size 8

# Number of persons that could be included with a 
# budget of Npcr tests and prevalence p. 
AntPersPool8 <- function(Npcr,p)
  8*Npcr/(1+poolprob(p,8,sens8)*8)

# Example, test capacity of 1000, prevalence of 0.5%
AntPersPool8(1000,0.005)

# Expected number of positiv cases idetified with a 
# budget of Npcr tests and prevalence p. 
EAntDetectionsPool8 <- function(Npcr,p)
  (poolprob(p,8,sens8)*Npcr/(1+poolprob(p,8,sens8)*8))*(8*p/(1-(1-p)^8))

# Example, test capacity of 1000, prevalence of 0.5%
EAntDetectionsPool8(1000,0.005)



##############################################################
## Pool size 32. If positive, test again in 4 pools of size 8. 
## Finally, test all 8 in the positive pools of size 8
##############################################################

sens32 <- 0.90 # Assumed sensitivity for pool size 8

# Expected number of postive pools of size 8 when the pool 
# of size 32 is positive.
ENPositivePools <- function(p)
  (dbinom(1,32,p)+(7/31+2*24/31)*dbinom(2,32,p)
   +(7*6/(31*30)+2*(7*24*3/(31*30))+3*(24*16/(31*30)))*dbinom(3,32,p)
   +4*pbinom(3,32,p, lower.tail=FALSE))/(1-dbinom(0,32,p))


# Number of persons that could be included with a 
# budget of Npcr tests and prevalence p. 
AntPersPool32 <- function(Npcr,p)
  32*Npcr/(1+poolprob(p,32,sens32)*(4+8*ENPositivePools(p)))

# Example, test capacity of 1000, prevalence of 0.5%
AntPersPool32(1000,0.005)


# Expected number of positiv cases idetified with a 
# budget of Npcr tests and prevalence p. 
EAntDetectionsPool32 <- function(Npcr,p)
  poolprob(p,32,0.90)*Npcr/(1+poolprob(p,32,0.90)*(4+8*ENPositivePools(p)))*(32*p/(1-(1-p)^(32)))

# Example, test capacity of 1000, prevalence of 0.5%
EAntDetectionsPool32(1000,0.005)





## Plots for the case with a test budget of 1000
Npcr <- 1000  #  Test budget
pvec <- seq(0,0.02,by=0.0001) # Considered range of prevalences


#pdf("ntested1000.pdf", pointsize = 16)
plot(pvec,AntPersPool32(Npcr,pvec), type="l", ylim=c(0,25000),
     xlab="prevalence",ylab="Number of persons tested",
     lwd=1.5,col="maroon", main=paste("Achieved with", Npcr, "PCR test"))
lines(pvec,AntPersPool8(Npcr,pvec), lty=2, lwd=1.5, col="blue")
lines(c(0,0.02),c(Npcr,Npcr), lty=3, lwd=2, col="black")
legend("topright",c("Pool 32", "Pool 8", "No pooling"),lty=1:3,
       col=c("maroon", "blue", "black"))
#dev.off()


#pdf("ndetected1000.pdf", pointsize = 16)
plot(pvec,EAntDetectionsPool32(Npcr,pvec), type="l", ylim = c(0,100),
     xlab="prevalence", ylab="Exptected number of infected persons detected",
     lwd=1.5,col="maroon", main=paste("Achieved with", Npcr, "PCR test"))
lines(pvec,EAntDetectionsPool8(Npcr,pvec), lty=2, lwd=1.5, col="blue")
lines(pvec,Npcr*pvec*0.99, lty=3, lwd=2, col="black")
legend("topleft",c("Pool 32", "Pool 8", "No pooling"),lty=1:3,
       col=c("maroon", "blue", "black"))
#dev.off()



