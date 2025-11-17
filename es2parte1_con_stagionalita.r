library(readxl)

# --- LETTURA E PREPARAZIONE DATI ---

file_path <- "C:/uniLM/Modelli statistici/Esercizio 2/Parte1/Serie Storiche Mensili _ ISTAT.xlsx"
dati_istat <- read_excel(file_path)

# Prendo solo le righe contenenti dati (da riga 8 in poi)
dati_valori <- dati_istat[8:nrow(dati_istat), ]

# Estraggo periodo (data) e indici dei volumi di importazione
datazione <- as.character(dati_valori[[2]])
import_volumi <- as.numeric(dati_valori[[6]])

# Rimuove valori NA in entrambe le variabili
validi <- !(is.na(datazione) | is.na(import_volumi))
datazione <- datazione[validi]
import_volumi <- import_volumi[validi]

# Trasforma la variabile data in formato Date (il formato è tipo "1996-01")
datazione <- as.Date(paste0(datazione, "-01"), format = "%Y-%m-%d")

# GRAFICO SERIE ORIGINALE
plot(datazione, import_volumi, type = "l",
     main = "Importazioni - Indici dei volumi (ISTAT)",
     xlab = "Data", ylab = "Indice dei volumi importazioni",
     col = "blue")
grid()

# COSTRUZIONE SEQUENZA DEL TEMPO
t <- 1:length(import_volumi)

# --- SEGMENTAZIONE DELLA SERIE ---

# IMPOSTAZIONE BREAK
break1 <- as.Date("2008-01-01")
break2 <- as.Date("2012-01-01")

# DISTINZIONE DI 3 SEGMENTI DI TEMPO
seg <- ifelse(datazione<break1, 1, ifelse(datazione<break2, 2, 3))
# cioè seg vale 1 per le date minori di break1, vale 2 per le date fra break1 e break2, infine vale 3 per le date maggiori di break2

# --- COSTRUZIONE VARIABILI DUMMY ---
d1 <- ifelse(seg == 1, 1, 0) # Dummy per "pre-crisi"
d2 <- ifelse(seg == 2, 1, 0) # Dummy per "in-crisi"
d3 <- ifelse(seg == 3, 1, 0) # Dummy per "post-crisi"

# COSTRUZIONE DI UN DATAFRAME CON I 3 SEGEMENTI DI TEMPO
df <- data.frame(datazione = datazione, 
                 import_volumi=import_volumi, 
                 t = t, 
                 seg = factor(seg),
                 d1 = d1,
                 d2 = d2,
                 d3 = d3)

# --- COSTRUZIONE FORMULA SEGMENTATA (1, 1, 1) ---
pol__grado_111 <- import_volumi ~ d1 + d2 + d3 + I(t * d1) + I(t * d2) + I(t * d3) - 1
# 'seg' stima un'intercetta diversa per ogni segmento
# 'I(t * d1)' ecc. stima un trend diverso per ogni segmento
# La funzione I() assicura che R calcoli (t * d1) prima di inserirlo nel modello.

#--- STIMA TREND CON POLINOMI LINEARI PER OGNI SEGMENTO (1, 1, 1) ---
# Stima il modello lineare usando la formula segmentata
modello_stimato_grado_111 <- lm(pol__grado_111, data = df)
# Estrai i valori stimati (la linea del trend)
trend_pol1_seg111 <- fitted(modello_stimato_grado_111)

# --- STAMPA RISULTATI (1,1,1) ---
cat("\n--- Summary del Trend Lineare Segmentato (1, 1, 1) ---\n")
print(summary(modello_stimato_grado_111))

#-------------------------------------------------------------------------------

# --- COSTRUZIONE FORMULA SEGMENTATA (1, 2, 1) ---
pol__grado_121 <- import_volumi ~ d1 + d2 + d3 + I(t * d1) +
  I(t * d2) + I(t^2 * d2) + I(t * d3) -1

#--- STIMA TREND (1, 2, 1) ---
modello_stimato_grado_121 <- lm(pol__grado_121, data = df)
trend_pol_seg121 <- fitted(modello_stimato_grado_121)

# --- STAMPA RISULTATI (1, 2, 1) ---
cat("\n--- Summary del Trend Segmentato (1, 2, 1) ---\n")
print(summary(modello_stimato_grado_121))

#-------------------------------------------------------------------------------

# --- COSTRUZIONE FORMULA SEGMENTATA (2, 2, 2) ---
pol__grado_222 <- import_volumi ~ d1 + d2 + d3 + I(t * d1) + I(t^2 * d1) + 
  I(t * d2) + I(t^2 * d2) + I(t * d3) + I(t^2 * d3) - 1

#--- STIMA TREND (2, 2, 2) ---
modello_stimato_grado_222 <- lm(pol__grado_222, data = df)
trend_pol_seg222 <- fitted(modello_stimato_grado_222)

# --- STAMPA RISULTATI (2, 2, 2) ---
cat("\n--- Summary del Trend Quadratico Segmentato (2, 2, 2) ---\n")
print(summary(modello_stimato_grado_222))

#-------------------------------------------------------------------------------

# --- COSTRUZIONE FORMULA SEGMENTATA (2, 2, 1) ---
pol__grado_221 <- import_volumi ~ d1 + d2 + d3 + I(t * d1) + I(t^2 * d1) + 
  I(t * d2) + I(t^2 * d2) + I(t * d3) -1

#--- STIMA TREND (2, 2, 1) ---
modello_stimato_grado_221 <- lm(pol__grado_221, data = df)
trend_pol_seg221 <- fitted(modello_stimato_grado_221)

# --- STAMPA RISULTATI (2, 2, 1) ---
cat("\n--- Summary del Trend Segmentato (2, 2, 1) ---\n")
print(summary(modello_stimato_grado_221))

#-------------------------------------------------------------------------------

# --- COSTRUZIONE FORMULA SEGMENTATA (2, 3, 2) ---
pol__grado_232 <- import_volumi ~ d1 + d2 + d3 + I(t * d1) + I(t^2 * d1) + 
  I(t * d2) + I(t^2 * d2) + I(t^3 * d2) + I(t * d3) + I(t^2 * d3) - 1

#--- STIMA TREND (2, 3, 2) ---
modello_stimato_grado_232 <- lm(pol__grado_232, data = df)
trend_pol_seg232 <- fitted(modello_stimato_grado_232)

# --- STAMPA RISULTATI (2, 3, 2) ---
cat("\n--- Summary del Trend Segmentato (2, 3, 2) ---\n")
print(summary(modello_stimato_grado_232))

#-------------------------------------------------------------------------------

# --- COSTRUZIONE FORMULA SEGMENTATA (2, 3, 1) ---
pol__grado_231 <- import_volumi ~ d1 + d2 + d3 + I(t * d1) + I(t^2 * d1) + 
  I(t * d2) + I(t^2 * d2) + I(t^3 * d2) + I(t * d3) - 1

#--- STIMA TREND (2, 3, 1) ---
modello_stimato_grado_231 <- lm(pol__grado_231, data = df)
trend_pol_seg231 <- fitted(modello_stimato_grado_231)

# --- STAMPA RISULTATI (2, 3, 1) ---
cat("\n--- Summary del Trend Segmentato (2, 3, 1) ---\n")
print(summary(modello_stimato_grado_231))

#-------------------------------------------------------------------------------

# --- COSTRUZIONE FORMULA SEGMENTATA (3, 3, 3) ---
pol__grado_333 <- import_volumi ~ d1 + d2 + d3 + I(t * d1) + I(t^2 * d1) + I(t^3 * d1) +
  I(t * d2) + I(t^2 * d2) + I(t^3 * d2) + I(t * d3) + I(t^2 * d3) + I(t^3 * d3) - 1

#--- STIMA TREND (3, 3, 3) ---
modello_stimato_grado_333 <- lm(pol__grado_333, data = df)
trend_pol_seg333 <- fitted(modello_stimato_grado_333)

# --- STAMPA RISULTATI (3, 3, 3) ---
cat("\n--- Summary del Trend Segmentato (3, 3, 3) ---\n")
print(summary(modello_stimato_grado_333))

#-------------------------------------------------------------------------------

# --- GRAFICO DEI TREND ---

# Aggiungi i valori stimati dai modelli al grafico
lines(datazione, trend_pol1_seg111, col = "purple", lwd = 2)
lines(datazione, trend_pol_seg222, col = "red", lwd = 2)
lines(datazione, trend_pol_seg232, col = "orange", lwd = 2)
lines(datazione, trend_pol_seg231, col = "violet", lwd = 2)
lines(datazione, trend_pol_seg333, col = "green", lwd = 2)

# Aggiungi una legenda
legend("topleft",
       legend = c("Serie originale", 
                  "Trend segmentato 1, 1, 1",
                  "Trend segmentato 2, 2, 2",
                  "Trend segmentato 2, 3, 2",
                  "Trend segmentato 2, 3, 1",
                  "Trend segmentato 3, 3, 3"),
       col = c("blue", "purple","red", "orange", "violet", "green"),
       lwd = c(1, 2, 2, 2, 2, 2),
       bty = "n")

#-------------------------------------------------------------------------------
# --- STIMA DELLA STAGIONALITA' CON VARIABILI DUMMY ---
#-------------------------------------------------------------------------------

cat("\n--- Avvio Stima Componente Stagionale ---\n")

# 1. Creiamo 12 variabili dummy stagionali, una per ogni mese
df$mese <- as.numeric(format(df$datazione, "%m"))
df$d_gen <- ifelse(df$mese == 1, 1, 0)
df$d_feb <- ifelse(df$mese == 2, 1, 0)
df$d_mar <- ifelse(df$mese == 3, 1, 0)
df$d_apr <- ifelse(df$mese == 4, 1, 0)
df$d_mag <- ifelse(df$mese == 5, 1, 0)
df$d_giu <- ifelse(df$mese == 6, 1, 0)
df$d_lug <- ifelse(df$mese == 7, 1, 0)
df$d_ago <- ifelse(df$mese == 8, 1, 0)
df$d_set <- ifelse(df$mese == 9, 1, 0)
df$d_ott <- ifelse(df$mese == 10, 1, 0)
df$d_nov <- ifelse(df$mese == 11, 1, 0)
df$d_dic <- ifelse(df$mese == 12, 1, 0)

# 2. Costruiamo la formula stagionale
# e rimuoviamo l'intercetta (-1) per evitare la "trappola delle dummy"
formula_stagionale <- import_volumi ~ d_gen + d_feb + d_mar + d_apr + 
  d_mag + d_giu + d_lug + d_ago + 
  d_set + d_ott + d_nov + d_dic - 1

# 3. Stima del modello di regressione stagionale
modello_stagionale <- lm(formula_stagionale, data = df)

# 4. Stampiamo i risultati
cat("\n--- Summary del Modello Stagionale ---\n")
print(summary(modello_stagionale))

cat("\n--- Interpretazione Coefficienti Stagionali ---\n")
cat("I coefficienti 'Estimate' sono i 'coefficienti grezzi di stagionalità'.\n")
cat("Ognuno rappresenta il valore medio del fenomeno 'import_volumi' per quello specifico mese.\n")

# 5. Grafico della stagionalità
stag_coeffs <- coef(summary(modello_stagionale))[, "Estimate"]
plot(1:12, stag_coeffs, type = "b",
     main = "Coefficienti Grezzi di Stagionalità (Medie Mensili)",
     xlab = "Mese", ylab = "Coefficiente Stimato",
     xaxt = "n") # Disattiva l'asse x numerico
axis(1, at = 1:12, labels = c("Gen", "Feb", "Mar", "Apr", "Mag", "Giu", "Lug", "Ago", "Set", "Ott", "Nov", "Dic"))
grid()

# --- GRAFICO: CONFRONTO VALORI OSSERVATI E STAGIONALITA' STIMATA ---

# 1. Estrae i valori stimati (le ordinate stimate) dal modello stagionale
# Questo crea un vettore lungo quanto la serie originale, dove ogni
# osservazione è sostituita dal suo coefficiente stagionale grezzo.
valori_stagionali_stimati <- fitted(modello_stagionale)

# 2. Crea il grafico di confronto
plot(df$datazione, df$import_volumi, type = "l", 
     col = "blue", 
     main = "Valori Osservati vs. Stagionalità Stimata",
     xlab = "Data", 
     ylab = "Indice Volumi",
     lwd = 1)

# 3. Sovrappone la linea della stagionalità stimata
lines(df$datazione, valori_stagionali_stimati, col = "red", lwd = 2, lty = 2)
grid()

# 4. Aggiungi una legenda
legend("topleft",
       legend = c("Valori Osservati (import_volumi)", "Stagionalità Stimata (Medie Mensili)"),
       col = c("blue", "red"),
       lwd = c(1, 2),
       lty = c(1, 2),
       bty = "n")