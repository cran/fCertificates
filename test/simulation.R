# Monte Carlo Valuations in order to check analytic formulas
#
# Use method simulatePricesAndMinimumFromGBB()
library(fCertificates)
library(fExpressCertificates)
#source("C:/Projects/Java/certificateserver/docs/modelle/GBB.R")
#source("F:/Promotion/Sweave/Expressstudie/GBB.R")

# Simulate probability for a Geometric Brownian Motion 
# to stay in some predefined range [a,b], i.e. P(m_t >= a & M_t <= b)
#
# Literatur:
#
# (1) Sutrick, Teall, Tucker, Wei (1997): The Range of Brownian Motion Processes, The Journal of Financial Engineering, Vol.6, 31-46
# (2) Hui (1996) : One-Touch Barrier Binary Option Values, Applied Financial Economics Vol. 6, 343-346
# (3) Haug (2007), p. 180/181
simProbInRange <- function(S0, a, b, mu, sigma, T, mc.loops, mc.steps)
{
  S     = matrix(NA, mc.loops, mc.steps+1)
  for (i in 1:mc.loops)
  {
    S[i,] = GBB(S0=S0, mu, sigma, T, N=mc.steps)
  }#

  # Minimum m_t
  m_t = apply(S, 1, min)
  M_t = apply(S, 1, max)
  # Wahrscheinlichkeit aus Monte Carlo, zwischen a und b zu bleiben
  p = mean(m_t >= a & M_t <= b)
  p
}

# Simulation of Bonus-Pro-Certificate
#
# There are two types of "B" options: 
# "B1" is defined such that only a barrier hit or crossed causes the option to be knocked out, 
# and a "B2" is defined such that a down-and-out-call is knocked out 
# as soon as the underlying price is below the barrier.
#
sim.BonusProCertificate(TypeFlag=c("poB1","pdoB2"), S, X, B, Time, time1=0, r, r_d, sigma, 
  ratio=1, barrierHit=FALSE, mc.loops=10000)
{
  TypeFlag = match.arg(TypeFlag)
  
  X1 <- simulatePricesAndMinimumFromGBB(n=mc.loops, S=16.66, T=c(time1,Time), mu=r-r_d, sigma=sigma, returnAllPeriodMinima=TRUE)
  head(X1)

  # Bewertung BonusPro
  # Type pdoB2 --> BonusPro Wertlos, sobald Schwelle B ab t1 unterschritten
  if (TypeFlag == "pdoB2") {
    barrierHit <- X1[,"S1"] <= 14 | X1[,"m12"] <= 14
    payoff <- exp(-r*Time) * X1[,"S2"] + ifelse(barrierHit, 0, pmax(X1[,"S2"] - X, 0))

    M  <- mean(payoff)
    # Standard Error
    SE <- sd(payoff)/sqrt(mc.loops)
    return(mean=M, se=SE)
  } else {
    # Type poB1 --> BonusPro nur wertlos, wenn die Schwelle B ab t1 gekreuzt wird
    # d.h. wenn S1 < B und M_{12} >= B
    #      oder S1 > B und m_{12} <= B
    # TODO: TO BE IMPLEMENTED
    return(mean=NA, se=NA)
  }
}


################################################################################

PTSingleAssetBarrierOption2(TypeFlag = "pdoB2", S=16.66, X=21, H = 14, time1 = .RLZ("15.09.2011"), 
  Time2 = .RLZ("22.12.2011"), r=0.01, b = 0, sigma=0.3)@price
PTSingleAssetBarrierOption(TypeFlag = "pdoB2", S=16.66, X=21, H = 14, time1 = .RLZ("15.09.2011"), 
  Time2 = .RLZ("22.12.2011"), r=0.01, b = 0, sigma=0.3)@price  
BonusProCertificate(S=16.66, X=21, B=14, time1=.RLZ("15.09.2011"), Time=.RLZ("22.12.2011"), r=0.01, r_d=0, sigma=0.3) 
  
# Simulation der Kurse und des Minimums m_t über direktem Weg	
mc.loops <- 1000000
time1    <- .RLZ("15.09.2011")
Time     <- .RLZ("22.12.2011")
sigma <- 0.3
r = 0.01
r_d <- 0
X <- 21
B <- 14
X1 <- simulatePricesAndMinimumFromGBB(n=mc.loops, S=16.66, T=c(time1,Time), mu=r-r_d, sigma=sigma, returnAllPeriodMinima=TRUE)
head(X1)

# Bewertung BonusPro
# Type pdoB2 --> BonusPro Wertlos, sobald Schwelle B ab t1 unterschritten
barrierHit <- X1[,"S1"] <= 14 | X1[,"m12"] <= 14
payoff <- exp(-r*Time) * X1[,"S2"] + ifelse(barrierHit, 0, pmax(X1[,"S2"] - X, 0))

M  <- mean(payoff)
# Standard Error
SE <- sd(payoff)/sqrt(mc.loops)

cat("[",M-SE,",",M+SE,"]\n")
  
BonusProCertificate(TypeFlag = "pdoB2", S=16.66, X=21, B=14, time1=.RLZ("15.09.2011"), Time=.RLZ("22.12.2011"), r=0.01, r_d=0, sigma=0.3)
BonusProCertificate(TypeFlag = "poB1", S=16.66, X=21, B=14, time1=.RLZ("15.09.2011"), Time=.RLZ("22.12.2011"), r=0.01, r_d=0, sigma=0.3)