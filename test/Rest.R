#
# Ein Korridor Bonus Zertifikat partizipiert an der Entwicklung des Underlyings
# und gewährt dem Anleger unter gewissen Bedingungen den Erhalt einer
# Bonuszahlung. Solange weder die obere noch die untere Barriere durchbrochen
# wurden, erhält der Anleger den Maximalbetrag von 129,- EUR. Wurde zuerst die
# untere Barriere mindestens einmal erreicht oder unterschritten, so partizipiert
# der Anleger bis zu einem Cap von 125,- an der positiven Entwicklung des
# Underlyings. Wurde zuerst die obere Barriere mindestens einmal erreicht oder
# überschritten, so partizipiert der Anleger bis zu einem Cap von 125,-
# unterhalb des Initialkurses positiv an der negativen Wertentwicklung des
# Underlyings, wobei sich die Rückzahlung durch Bezugsverhältnis * (2 *
# Initialkurs - Schlusskurs) ergibt.
#
#  Duplication:
#
# (1) Zero-Strike-Call
# (2) Digit-Option
# (3) Call Up & In
# (4) Call Down & Out
# (5) Put Down & In
# (6) Put Up & Out
#
# @param S underlying price
# @param S0 (z.B. 125 EUR)
# @param Cap (z.B. 125 EUR)
# @params B1 lower barrier (B1 <= S0 <= B2)
# @params B2 upper barrier (B1 <= S0 <= B2)
# @params Time time to maturity in years
# @params r interest rate p.a. as 0.02 = 2%
# @params r_d continuous dividend yield p.a. as 0.02 = 2%
# @params sigma volatility p.a. as 0.18 = 18%
# @params ratio
KorridorBonusCertificate<-function(S, Cap, B1, B2, Time, r, r_d, sigma, ratio=1)
{
	# 1. Zero-Strike Call
	zero_strike_call = GBSOption(TypeFlag="c", S, X=0, Time, r, b=r-r_d, sigma) 
	price1=attr(zero_strike_call,"price")
	
	# 2. Digit-Option (Cash-Or-Nothing-Option)
	con_call<-CashOrNothingOption("p", S, X=Cap, Cap, Time, r, b=r-r_d, sigma)
	price2=attr(con_call,"price")
	
	# 3. Call Up & In
	up_in_call  = StandardBarrierOption(TypeFlag="cui", S, X=Cap, H=B1, K=0, Time, r, b=r-r_d, sigma) 
	price3=attr(up_in_call,"price")
	
	# 4. Call Down & Out
	down_out_call  = StandardBarrierOption(TypeFlag="cdo", S, X=Cap, H=B2, K=0, Time, r, b=r-r_d, sigma) 
	price4=attr(down_out_call,"price")
	
	# 5. Put Down & In
	down_in_put  = StandardBarrierOption(TypeFlag="pdi", S, X=Cap, H=B1, K=0, Time, r, b=r-r_d, sigma) 
	price5=attr(down_in_put,"price")
	
	# 6. Put Up & Out
	up_out_put  = StandardBarrierOption(TypeFlag="puo", S, X=Cap, H=B1, K=0, Time, r, b=r-r_d, sigma) 
	price6=attr(up_out_put,"price")
	
	(price1 - price2 - price3 - price4 - price5 - price6) * ratio
}

#KorridorBonusCertificate(S=100, Cap=25, B1=75, B2=125, Time=0, r=0.045, r_d=0, sigma=0.2, ratio=1)

# Express-Bonus-Zertifikat : Mehrere vorzeitige Bewertungstage, am letzten Bewertungstag wie ein normales Capped-Bonus-Zertifikat
#
# 1. Cap (greift nur am Letzten Bewertungstag)
# 2. Barriere B (=Pfadabhängigkeit greift nur am letzten Bewertungstag)
# 
# @param S aktueller Stand des Basiswerts
# @param S0 Startkurs Basiswert (wenn Grenzen prozentual angegeben sind)
# @param B Barriere am Laufzeitende
# @param X Strike-Preise für n Bewertungstage
# @param Cap
# @param T Bewertungstage
# @param Payoff Rückzahlungsvektor für vorzeitige Rückzahlungen (Länge (n-1))
ExpressBonusCertificate<-function(S, S0, B, X, Cap, T=c(6/12,18/12,30/12), Payoff=c(111.5, 123), r, r_d, sigma, ratio=1)
{
	# Anzahl der Bewertungstage
	n = length(T)
	
	# Fürs Debugging
	probs<-c()
	prices<-c()
	
	# Wahrscheinlichkeit, dass das Zertifikat noch einen Bewertungstag weiter läuft (es läuft immer bis zum 1. Bewertungstag)
	cum_prob=1
	cum_price=0
	
	for (i in 1:(n-1))
	{
		# Wie lange ist der Bewertungstag weg?
		
		# Cash-Or-Nothing-Option für Bewertungstag i
		con_call<-CashOrNothingOption("c", S, X=X[i], Payoff[i], Time=T[i], r, b=r-r_d, sigma)
		prices[i]=attr(con_call,"price")
		cum_price = cum_price + cum_prob * prices[i]
		
		# Wahrscheinlichkeit, am i-ten Bewertungstag, unter der Schwelle X[i] zu bleiben, also dass das Zertifikat in die nächste Runde geht
		probs[i] <- shortfall_risk("c", S, X=X[i], T[i], r, r_d, sigma)
		cum_prob = cum_prob * probs[i]
	}
	
	# letzter Bewertungstag
	prices[n]<-CappedBonusCertificate(S, X=X[n], Cap=Cap, B=B, T=T[n], r, r_d, sigma, ratio=1)
	cum_price = (cum_price + cum_prob * prices[n])*ratio
	
	#Fürs Debugging : result=list(price=cum_price, prices=prices, probs=probs)
	#result
	cum_price
}

# Pricing of Korridor-Easy-Express-Certificate
#
# Duplication:
# (1)
#
# @params S underlying price
# @params S0 Rückzahlungsbetrag (z.B. 120 EUR)
# @params B1 lower barrier (B1 <= S0 <= B2)
# @params B2 upper barrier (B1 <= S0 <= B2)
# @params Time time to maturity in years
# @params r interest rate p.a. as 0.02 = 2%
# @params r_d continuous dividend yield p.a. as 0.02 = 2%
# @params sigma volatility p.a. as 0.18 = 18%
# @params ratio
KorridorEasyExpressCertificate<-function(S, S0, B1, B2, Time, r, r_d, sigma, ratio=1)
{
	if (Time == 0 && (S == B1 || S == B2))
	{
		return(S0)
	}
	
	# 1. Long Cash-or-Nothing-Call
	con_long_call<-CashOrNothingOption("c", S, 0, S0, Time, r, b=r-r_d, sigma)
	price1=attr(con_long_call,"price")
	
	# 2. Short Cash-or-Nothing (short-put)
	con_short_put<-CashOrNothingOption("p", S, B1, S0-B1, Time, r, b=r-r_d, sigma)
	price2=attr(con_short_put,"price")
	
	# 3. Cash-or-Nothing (short-call)
	con_short_call<-CashOrNothingOption("c", S, B2, S0-B1, Time, r, b=r-r_d, sigma)
	price3=attr(con_short_call,"price")
	
	# 4. Plain-Vanilla-Short-Put
	plain_short_put<- GBSOption(TypeFlag="p", S, X=B1, Time, r, b=r-r_d, sigma)
	price4=attr(plain_short_put,"price")
	
	# 5. Plain-Vanilla-Short-Call  
	plain_short_call<- GBSOption(TypeFlag="c", S, X=B2, Time, r, b=r-r_d, sigma)
	price5=attr(plain_short_call,"price")
	
	(price1 - price2 - price3 - price4 - price5) * ratio
}

# Berechnung eines "Reverse Convertibles Bear" (Inverse Reverse Convertible) : ISIN CH0031093161
#
# @param S 
# @param Cap 
# @param Time
# @param r
# @param r_d   
# @param sigma   
# @param nominal 
# @param coupon
# 
InverseReverseConvertible<-function(S, Cap, Time, r, r_d, sigma, nominal, coupon)
{
	# TO BE DONE 
}

# Rainbow-Options
#
#
# Literatur:
# (1) Rubinstein M. (1991) Somewhere over the Rainbow, Risk Magazine 4, 10. 
# (2) Stulz R.M. (1982); Options on the Minimum or Maximum of Two Risky Assets, Journal of Financial Economics 10, 161–185.
# (3) Nelken, Israel (1996) : Handbook of Exotic Options
#
# The Valuation of American Call Options on the Minimum of Two Dividend-Paying Assets 
# Jerome Detemple; Shui Feng; Weidong Tian 
# The Annals of Applied Probability > Vol. 13, No. 3 (Aug., 2003), pp. 953-983

# Berechnung eines "Multi-Bonus-Zertifikats"
#
# Literatur/Stichworte:
# (1) Rainbow-Options "Best-Of", "Worst-Of"
# (2) http://www.global-derivatives.com/index.php?option=com_content&task=view&id=51&Itemid=31
# 
#
#
# @param S 
# @param X Strike price
# @param B = Absicherungslevel 
# @param Time
# @param r
# @param r_d   
# @param sigma   
# @param nominal 
# @param coupon
# 
MultiBonusCertificate<-function(S, S0, X, B, Time, r, r_d, sigma, participation, ratio=1)
{
	# TO BE DONE
}

# Beispiel: DE000TB07ED4 Multi-Capped-Bonus auf (1) Daimler, (2) E.ON, (3) Siemens
#
#
#MultiBonusCertificate(S=c(54.20,122.24,82.53), S0=c(53.35, 145.8, 92), X=116, B=c(26.68,72.90,46), Time=RLZ("26.06.2009"), r=0.045, r_d=c(0,0,0), sigma=c(0.27, 0.25, 0.34), ratio=1)