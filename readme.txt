Readme R-Pakete bauen
---------------------
0. Dokumentation lesen
- "Making R Packages Under Windows.pdf" lesen
- R-Dokumenation "Writing R Extensions”
- "Building MicrosoftWindows Versions of R and R packages under Intel Linux" : http://cran.r-project.org/doc/contrib/cross-build.pdf
- "R Pakete bauen in Windows und Mac" : statmath.wu-wien.ac.at/~dekic/R/

1. Download the set of “Rtools” or Unix utilities: http://www.murdoch-sutherland.com/Rtools/tools.zip
2. ActiveState Perl installieren

http://www.murdoch-sutherland.com/Rtools/installer.html

3. Rd-Dokumentation Files erzeugen mit prompt()
prompt(RTS, file="RTS.Rd")

4. Paket checken mit

Dos-Kommandozeile (cygwin geht nicht richtig)

cd C:\Projects\R

R CMD check fCertificates
bzw.
Rcmd check fCertificates

3. Paket bauen mit 

R CMD build fCertificates

--> baut ein fCertificates-<version>.tar.gz

4. Paket installieren mit

R CMD INSTALL <package>
R CMD INSTALL fCertificates

5. Installierbares .zip Paket für Windows aus dem tar.gz bauen

R CMD INSTALL --build fCertificates_0.1-1.tar.gz
R CMD INSTALL --build fCertificates_0.1-2.tar.gz
R CMD INSTALL --build fCertificates_0.2-1.tar.gz

6. Upload des Pakets zu CRAN
a) Upload als ftp://CRAN.R-project.org/incoming/
b) Email CRAN@R-project.org

Non-ASCII-Zeichen finden
------------------------

Funktion showNonASCII() in Paket tools

showNonASCII(readLines("Probabilities.R"))

71:     if (a < 0 ) stop("a muss gro<df>er sein als 0")
174:   if (a < 0 ) stop("a muss gro<df>er sein als 0")
Warnmeldung:
In readLines("R/Probabilities.R") :
  unvollständige letzte Zeile in 'R/Probabilities.R' gefunden


Hilfe testweise erstellen zum Prüfen:
-------------------------------------

Für Einzelfiles:
H:\R\fExpressCertificates>R CMD Rdconv -t html man/calculateProbabilityBrownianMotion.Rd

R CMD Rd2txt man/calculateProbabilityBrownianMotion.Rd
R CMD Rd2dvi man/calculateProbabilityBrownianMotion.Rd


