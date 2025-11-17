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
cat("\n--- Summary del Trend Quadratico Segmentato (2, 2, 1) ---\n")
print(summary(modello_stimato_grado_221))

#-------------------------------------------------------------------------------

# --- COSTRUZIONE FORMULA SEGMENTATA (2, 3, 2) ---
pol__grado_232 <- import_volumi ~ d1 + d2 + d3 + I(t * d1) + I(t^2 * d1) + 
  I(t * d2) + I(t^2 * d2) + I(t^3 * d2) + I(t * d3) + I(t^2 * d3) - 1

#--- STIMA TREND (2, 3, 2) ---
modello_stimato_grado_232 <- lm(pol__grado_232, data = df)
trend_pol_seg232 <- fitted(modello_stimato_grado_232)

# --- STAMPA RISULTATI (2, 3, 2) ---
cat("\n--- Summary del Trend Quadratico Segmentato (2, 3, 2) ---\n")
print(summary(modello_stimato_grado_232))

#-------------------------------------------------------------------------------

# --- COSTRUZIONE FORMULA SEGMENTATA (3, 3, 3) ---
pol__grado_333 <- import_volumi ~ d1 + d2 + d3 + I(t * d1) + I(t^2 * d1) + I(t^3 * d1) +
  I(t * d2) + I(t^2 * d2) + I(t^3 * d2) + I(t * d3) + I(t^2 * d3) + I(t^3 * d3) - 1

#--- STIMA TREND (3, 3, 3) ---
modello_stimato_grado_333 <- lm(pol__grado_333, data = df)
trend_pol_seg333 <- fitted(modello_stimato_grado_333)

# --- STAMPA RISULTATI (3, 3, 3) ---
cat("\n--- Summary del Trend Quadratico Segmentato (3, 3, 3) ---\n")
print(summary(modello_stimato_grado_333))

#-------------------------------------------------------------------------------

# --- GRAFICO DEI TREND ---

# Aggiungi i valori stimati dai modelli al grafico
lines(datazione, trend_pol1_seg111, col = "purple", lwd = 2)
lines(datazione, trend_pol_seg222, col = "red", lwd = 2)
lines(datazione, trend_pol_seg232, col = "orange", lwd = 2)
lines(datazione, trend_pol_seg333, col = "green", lwd = 2)

# Aggiungi una legenda
legend("topleft",
       legend = c("Serie originale", 
                  "Trend segmentato 1, 1, 1",
                  "Trend segmentato 2, 2, 2",
                  "Trend segmentato 2, 3, 2",
                  "Trend segmentato 3, 3, 3"),
       col = c("blue", "purple","red", "orange", "green"),
       lwd = c(1, 2, 2, 2, 2),
       bty = "n")
