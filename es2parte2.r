library(readxl)
library(olsrr)
library(ggplot2)
library(DAAG)
library(car)
library(lmtest)

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
#NOTA: daL punto di vista empirico correlazioni maggiori di 0.8 
# possono indurci a pensare che vi sia multicollinearità
# NOTO CHE C'è GRANDE CORRELAZIONE FRA AREA, LABOR E FERT
# noto che le rette di regressione per queste 3 variabili non sono parallele all'asse x

# Stima del Modello Pieno (Full Model)
# Costruiamo il modello lineare con PROD come variabile dipendente
# e AREA, LABOR, FERT come regressori
modello_pieno <- lm(prod ~ firm + year + area + labor + fert, data = dati_rice)
cat("\n\n--- Analisi Modello Pieno (Tutti i Regressori) ---\n")
print(summary(modello_pieno))

# NOTO CHE FIRM E YEAR NON SONO STATISTICAMENTE SIGNIFICATIVI
# AREA LABOR E FERT SONO SIGNIFICATIVI, MA HO IL PROBLEMA DELLA CORRELAZIONE

# Stima con Modello 2
# Costruiamo il modello lineare con PROD come variabile dipendente
# e AREA, LABOR, FERT come regressori
modello2 <- lm(prod ~ area + labor + fert, data = dati_rice)
cat("\n\n--- Analisi Modello 2 (eliminando firm e year) ---\n")
print(summary(modello2))

# --- ANALISI DELLA MULTICOLLINEARITÀ ---

# Condition Number (Indice di Condizionamento)
# Formula: K = sqrt(lambda_max / lambda_min)
X <- cbind(area, labor, fert) # matrice dei regressori utilizzati
aut1<-eigen(t(X)%*%X) # autovalori di X(trasp)X
k<-sqrt(max(aut1$values)/min(aut1$values)) #condition number
cat("\n--- Condition Number (K) ---\n")
print(k)
cat("Interpretazione:
- K < 10: Collinearità debole/assente.
- 10 <= K < 30: Collinearità moderata.
- K >= 30: Collinearità FORTE (problematica)\n")

# VIF (Variance Inflation Factor)
# Formula: VIF_j = 1 / (1 - R_j^2)
# Indica di quanto aumenta la varianza del coefficiente a causa della collinearità.
vif_values <- vif(modello2)
cat("\n--- VIF (Variance Inflation Factor) ---\n")
print(vif_values)
# Interpretazione:
# - VIF > 10: Indica presenza di multicollinearità.
# - Valori vicini a 1 indicano assenza di multicollinearità.

# Tolerance
# Formula: Tol_j = 1 / VIF_j = 1 - R_j^2
# Rappresenta la quota di varianza del regressore non spiegata dagli altri regressori.
tolerance_values <- 1 / vif_values
cat("\n--- Tolerance (Tolleranza) ---\n")
print(tolerance_values)
# Interpretazione:
# Tolerance < 0.1: Indica una GRAVE multicollinearità (corrisponde a VIF > 10).

# rj0
rj0 <- (vif(modello2)-1)/vif(modello2)
cat("\n--- rj0 ---\n")
print(rj0)

# --- GESTIONE DELLA MULTICOLLINEARITÀ: TRASFORMAZIONE LOG-LOG ---

# Trasformazione delle variabili
dati_rice$l_prod  <- log(dati_rice$prod)
dati_rice$l_area  <- log(dati_rice$area)
dati_rice$l_labor <- log(dati_rice$labor)
dati_rice$l_fert  <- log(dati_rice$fert)

# Stima del Modello Log-Log (Cobb-Douglas)
modello_loglog <- lm(l_prod ~ l_area + l_labor + l_fert, data = dati_rice)

cat("\n\n--- Analisi Modello Log-Log ---\n")
print(summary(modello_loglog))

# Verifica miglioramento Condition Number
# Creiamo la matrice X con i logaritmi
X_log <- cbind(dati_rice$l_area, dati_rice$l_labor, dati_rice$l_fert)
aut_log <- eigen(t(X_log) %*% X_log)
k_log <- sqrt(max(aut_log$values) / min(aut_log$values))
cat("\n--- Nuovo Condition Number (Modello Log-Log) ---\n")
print(k_log)

# Verifica VIF sul nuovo modello
vif_values_loglog <- vif(modello_loglog)
cat("\n--- Nuovi VIF (Modello Log-Log) ---\n")
print(vif_values_loglog)

# Verifica tolerance sul nuovo modello
tolerance_values_loglog <- 1 / vif_values_loglog
cat("\n--- Tolerance (Tolleranza) ---\n")
print(tolerance_values_loglog)

# Verifica rj0 sul nuovo modello
rj0_loglog <- (vif(modello_loglog)-1)/vif(modello_loglog)
cat("\n--- rj0 ---\n")
print(rj0_loglog)

# Confronto Grafico
par(mfrow=c(1,2))
hist(dati_rice$prod, main="Produzione Originale", col="lightblue")
hist(dati_rice$l_prod, main="Log(Produzione)", col="lightgreen")

# --- VALIDAZIONE DEL MODELLO LOG-LOG (Analisi dei Residui) ---

# Costruiamo l’istogramma dei residui (standardizzati e non)
resstand <- rstandard(modello_loglog)
hist(resid(modello_loglog))
hist(resstand, freq=F, xlim=c(-4,4), ylim=c(0,0.6)); curve(dnorm(x),add=T)
plot(resstand)

# Il qq-plot è un grafico che mette a confronto i quantili empirici con 
# i quantili teorici della Normale Standardizzata
# Se i punti si dispongono tutti
# lungo una retta a 45° si può concludere che i residui standardizzati
# seguono una legge Normale. 
qqnorm(resstand, xlim=c(-4,4), ylim=c(-2,2)); qqline(resstand)
