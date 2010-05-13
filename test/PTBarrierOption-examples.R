library(fOptions)
library(fExoticOptions)
source("R/PartialTimeBarrierOption.R")

S=100
X=100
B=70
Time=1
sigma=0.3
r=0.01
r_d=0
time1=0.75

p            = GBSOption(TypeFlag="p", S, X, Time, r, b=r-r_d, sigma)@price
p_do         = StandardBarrierOption(TypeFlag="pdo", S, X, H=B, K=0, Time, r, b=r-r_d, sigma)@price

# Startzeitpunkt t1 der Barrierenbeobachtung : 
# für t1 --> 0 sollte sich der Preis eines normalen Down-And-Out-Puts ergeben, 
# für t1 --> 1 der Preis eines normalen Puts
time1        <- seq(0.01, 0.99, by=0.01)
p_do_partial_B1 <- numeric(length(time1))
p_do_partial_B2 <- numeric(length(time1))

p_do_partial_B1_SW <- numeric(length(time1)) # Meine Berechnung
p_do_partial_B2_SW <- numeric(length(time1)) # Meine Berechnung

for (i in seq(along=time1))
{
  p_do_partial_B1[i] = PTSingleAssetBarrierOption(TypeFlag="poB1", S, X, H=B, time1=time1[i], Time2=Time, r, b=r-r_d, sigma)@price 
  p_do_partial_B2[i] = PTSingleAssetBarrierOption(TypeFlag="pdoB2", S, X, H=B, time1=time1[i], Time2=Time, r, b=r-r_d, sigma)@price
  
  p_do_partial_B1_SW[i] = PTSingleAssetBarrierOption2(TypeFlag="poB1",  S, X, H=B, time1=time1[i], Time2=Time, r, b=r-r_d, sigma)@price 
  p_do_partial_B2_SW[i] = PTSingleAssetBarrierOption2(TypeFlag="pdoB2", S, X, H=B, time1=time1[i], Time2=Time, r, b=r-r_d, sigma)@price
}

p
p_do

plot(time1, p_do_partial_B1, type="l", ylim=c(0, 100), xlab="Start des Beobachtungszeitraums t1", ylab="Preis Put")
lines(time1, p_do_partial_B2, type="l", col="red")

lines(time1, p_do_partial_B1_SW, type="l", lwd=2, col="black", lty=2)
lines(time1, p_do_partial_B2_SW, type="l", lwd=2, col="red", lty=2)

# Preis eines Normalen Put bzw. Down-And-Out-Put einzeichnen als Grenze für t1 --> 0 und t1 --> 1
abline(h=c(p_do, p), lty=3)

p_do_partial_B1_SW
p_do_partial_B2_SW

undebug(PTSingleAssetBarrierOption2)
PTSingleAssetBarrierOption2(TypeFlag="coB1",  S, X, H=B, time1=0.02, Time2=Time, r, b=r-r_d, sigma)@price 
PTSingleAssetBarrierOption("coB1", S, X, H=B, time1=0.02, Time2=Time, r, b, sigma)@price
PTSingleAssetBarrierOption2(TypeFlag="poB1",  S, X, H=B, time1=0.02, Time2=Time, r, b=r-r_d, sigma)@price

S=100
X=100
B=70
time1=0.01
Time=1
sigma=0.3
r=0.01
b=0.01
#debug(PTSingleAssetBarrierOption)
PTSingleAssetBarrierOption(TypeFlag="poB1",  S, X, H=B, time1=time1, Time2=Time, r, b=b, sigma)@price
# --> 47.47563

StandardBarrierOption(TypeFlag="pdo", S, X, H=B, K=0, Time=Time, r, b=b, sigma)@price
# --> 3.394756

PTSingleAssetBarrierOption2(TypeFlag="poB1",  S, X, H=B, time1=time1, Time2=Time, r, b=b, sigma)@price
# --> 3.394756
