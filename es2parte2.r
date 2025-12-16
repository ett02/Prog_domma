library(readxl)
library(olsrr)
library(ggplot2)
library(DAAG)
library(car)
library(lmtest)
library(MASS)

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

# --- ANALISI DELL'ETEROSCHEDASTICITÀ ---

cat("\n\n############################################################\n")
cat("### ANALISI ETEROSCHEDASTICITÀ SUL MODELLO LINEARE (Modello 3). Regressori: year, area, labor, fert###\n")
cat("############################################################\n")

modello3 <- lm(prod ~ year + area + labor + fert, data = dati_rice)

# 1. Analisi Grafica (Slide 28-30)
# Plot dei residui vs valori stimati (fitted values)
res2 <- resid(modello3)          # Residui epsilon cappuccio
fit2 <- fitted(modello3)         # Valori stimati y cappuccio

# Grafico
plot(fit2, res2, 
     main = "Plot Residui vs Valori Stimati (Modello 3)",
     xlab = "Valori Stimati (Fitted)", 
     ylab = "Residui",
     pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2) 
# NOTA: Se c'è una forma a "imbuto" (variabilità che cresce), c'è eteroschedasticità.

# 2. Test di Breusch-Pagan (Manuale come in Slide 38, 53)
# Ipotesi H0: Omoschedasticità
# Ipotesi H1: La varianza dipende linearmente dai regressori (area, labor, fert)

res2_sq <- res2^2  # Residui al quadrato (epsilon^2)

# Modello ausiliario: residui^2 ~ regressori
mod_bp_linear <- lm(res2_sq ~ year + area + labor + fert, data = dati_rice)

cat("\n--- Test Breusch-Pagan su Modello 3 ---\n")
summary_bp <- summary(mod_bp_linear)
print(summary_bp)

# Calcolo statistica Chi-Quadro
n <- nrow(dati_rice)
R2_bp <- summary_bp$r.squared
BP_stat <- n * R2_bp
p_value_BP <- 1 - pchisq(BP_stat, df = 4) # df = numero regressori (senza intercetta)

cat(paste("Statistica LM (n*R2):", round(BP_stat, 4), "\n"))
cat(paste("P-value Chi-Quadro:", format.pval(p_value_BP), "\n"))
if(p_value_BP < 0.05) cat("ESITO: Rifiuto H0. C'è eteroschedasticità.\n") else cat("ESITO: Accetto H0. Omoschedasticità plausibile.\n")

# 3. Test di White (Versione speciale con Y cappuccio)
# Modello ausiliario: residui^2 ~ y_stimata + y_stimata^2

fit2_sq <- fit2^2 # Valori stimati al quadrato

mod_white_linear <- lm(res2_sq ~ fit2 + fit2_sq)

cat("\n--- Test di White su Modello 3 ---\n")
summary_white <- summary(mod_white_linear)
print(summary_white)

# Calcolo statistica Chi-Quadro (n * R^2)
R2_white <- summary_white$r.squared
White_stat <- n * R2_white
p_value_White <- 1 - pchisq(White_stat, df = 2) # df = 2 (fit2 e fit2^2)

cat(paste("Statistica LM (n*R2):", round(White_stat, 4), "\n"))
cat(paste("P-value Chi-Quadro:", format.pval(p_value_White), "\n"))
if(p_value_White < 0.05) cat("ESITO: Rifiuto H0. C'è eteroschedasticità.\n") else cat("ESITO: Accetto H0. Omoschedasticità plausibile.\n")

cat("\n\n############################################################\n")
cat("### ANALISI ETEROSCHEDASTICITÀ SUL MODELLO LOG-LOG ###\n")
cat("############################################################\n")
# Il logaritmo spesso stabilizza la varianza.

# 1. Analisi Grafica
res_log <- resid(modello_loglog)
fit_log <- fitted(modello_loglog)

plot(fit_log, res_log, 
     main = "Plot Residui vs Valori Stimati (Modello Log-Log)",
     xlab = "Valori Stimati (Log)", 
     ylab = "Residui",
     pch = 20, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)

# 2. Test Breusch-Pagan su Log-Log
res_log_sq <- res_log^2

# Nota: uso i regressori logaritmici usati nel modello_loglog
mod_bp_log <- lm(res_log_sq ~ l_area + l_labor + l_fert, data = dati_rice)

cat("\n--- Test Breusch-Pagan su Modello Log-Log ---\n")
summary_bp_log <- summary(mod_bp_log)
print(summary_bp_log)

R2_bp_log <- summary_bp_log$r.squared
BP_stat_log <- n * R2_bp_log
p_value_BP_log <- 1 - pchisq(BP_stat_log, df = 3)

cat(paste("Statistica LM (n*R2):", round(BP_stat_log, 4), "\n"))
cat(paste("P-value Chi-Quadro:", format.pval(p_value_BP_log), "\n"))
if(p_value_BP_log < 0.05) cat("ESITO: Rifiuto H0. Eteroschedasticità persiste.\n") else cat("ESITO: Accetto H0. Omoschedasticità raggiunta.\n")

# 3. Test di White su Log-Log
fit_log_sq <- fit_log^2
mod_white_log <- lm(res_log_sq ~ fit_log + fit_log_sq)

cat("\n--- Test di White su Modello Log-Log ---\n")
summary_white_log <- summary(mod_white_log)
print(summary_white_log)

R2_white_log <- summary_white_log$r.squared
White_stat_log <- n * R2_white_log
p_value_White_log <- 1 - pchisq(White_stat_log, df = 2)

cat(paste("Statistica LM (n*R2):", round(White_stat_log, 4), "\n"))
cat(paste("P-value Chi-Quadro:", format.pval(p_value_White_log), "\n"))
if(p_value_White_log < 0.05) cat("ESITO: Rifiuto H0. Eteroschedasticità persiste.\n") else cat("ESITO: Accetto H0. Omoschedasticità raggiunta.\n")

############################################################
###       STIMA DEL MODELLO ROBUSTO (RLM)                ###
############################################################
# Visto che i test precedenti (BP/White) 
# segnalano problemi sui residui (eteroschedasticità/outliers), 
# procediamo con la stima robusta.

# Stima del Modello Robusto
# Utilizziamo le stesse variabili del modello Log-Log (che aveva risolto la multicollinearità)
mod_rlm <- rlm(l_prod ~ l_area + l_labor + l_fert, data = dati_rice)

cat("\n--- Summary del Modello Robusto (RLM) ---\n")
summary(mod_rlm)

# Confronto visivo dei residui
# Plot: Valori Previsti vs Residui

plot(fitted(mod_rlm), resid(mod_rlm),
     main = "Residui vs Fitted (Robust Linear Model)",
     xlab = "Valori Previsti (RLM)",
     ylab = "Residui (RLM)",
     col  = "blue",    # Colore punti come nell'immagine
     pch  = 20)        # Stile punti (pallino pieno)

# Aggiunta della linea rossa orizzontale allo zero
abline(h = 0, col = "red", lwd = 2)

# --- Confronto rapido dei coefficienti OLS vs RLM ---
cat("\n--- Confronto Coefficienti: OLS (Log-Log) vs RLM ---\n")
confronto <- compareCoefs(modello_loglog, mod_rlm)
print(confronto)
