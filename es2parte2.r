library(readxl)
library(olsrr)
library(ggplot2)
library(DAAG)
library(car)

# Caricamento Dati
file_path_excel <- "C:/uniLM/Modelli statistici/Esercizio 2/Parte2/3_rice.xlsx" 
# Estrae i dati dal foglio "Data"
dati_rice <- read_excel(file_path_excel, sheet = "Data")

# Creazione variabili
firm = dati_rice$firm
year = dati_rice$year
prod = dati_rice$prod
area = dati_rice$area
labor = dati_rice$labor
fert = dati_rice$fert

# Matrice W che contiene, nelle colonne, le variabili di interesse appena definite
W <- cbind(firm, year, prod, area, labor, fert)

# Ispezione Dati
cat("--- Ispezione Dati (Head) ---\n")
print(head(W)) #print(head(dati_rice))
cat("\n--- Ispezione Dati (Summary) ---\n")
print(summary(W)) #print(summary(dati_rice))

# Istogramma della produzione
hist(prod, main="Istogramma della produzione (variabile dipendente)", xlab="Produzione", col="lightblue")
# NOTA: valori di produzione elevati sono poco frequenti

cat("\n--- Matrice di varianze e covarianze ---\n")
print(var(W))

cat("\n--- Matrice di correlazione ---\n")
print(cor(W))

scatterplotMatrix(~firm + year + prod + area + labor + fert,
                  col="black",
                  pch=20, regLine =
                    list(method=lm, lty=1, lwd=2, col="chartreuse3"),
                  smooth=FALSE,
                  diagonal=list(method="histogram", breaks="FD"),
                  main="Matrice di dispersione con rette di regressione",
                  data=dati_rice)

# NOTO CHE C'è GRANDE CORRELAZIONE FRA AREA, LABOR E FERT

# Stima del Modello Pieno (Full Model)
# Costruiamo il modello lineare con PROD come variabile dipendente
# e AREA, LABOR, FERT come regressori
modello_pieno <- lm(prod ~ firm + year + area + labor + fert, data = dati_rice)
cat("\n\n--- Analisi Modello Pieno (Tutti i Regressori) ---\n")
print(summary(modello_pieno))

# NOTO CHE FIRM E YEAR NON SONO SIGNIFICATIVI
# AREA LABOR E FERT SONO SIGNIFICATIVI, MA HO IL PROBLEMA DELLA CORRELAZIONE



