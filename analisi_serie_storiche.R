library(readxl)

# --- LETTURA DATI ---
file_path <- "C:/Users/Ettore/Prog_domma_2/Esercizio 2/1/Serie Storiche Mensili _ ISTAT.xlsx"
dati_istat <- read_excel(file_path)

# Prendi solo le righe contenenti dati (da riga 8 in poi)
dati_valori <- dati_istat[8:nrow(dati_istat), ]

# Estrai periodo (data) e indici dei volumi di importazione
data <- as.character(dati_valori[[2]])
import_volumi <- as.numeric(dati_valori[[6]])

# Rimuovi valori NA in entrambe le variabili
validi <- !(is.na(data) | is.na(import_volumi))
data <- data[validi]
import_volumi <- import_volumi[validi]

# Trasforma la variabile data in formato Date
data <- as.Date(paste0(data, "-01"), format = "%Y-%m-%d")

# --- MEDIA MOBILE ---
media_mobile <- stats::filter(import_volumi, rep(1/12, 12), sides = 2)

# --- STIMA TREND POLINOMIALI ---
t <- 1:length(import_volumi)

# Trend lineare (ordine 1)
pol1_import <- lm(import_volumi ~ poly(t, 1, raw = TRUE))
trend_pol1 <- fitted(pol1_import)
summary(pol1_import)

# Trend quadratico (ordine 2)
pol2_import <- lm(import_volumi ~ poly(t, 2, raw = TRUE))
trend_pol2 <- fitted(pol2_import)

# Trend cubico (ordine 3)
pol3_import <- lm(import_volumi ~ poly(t, 3, raw = TRUE)) # Era 108, sicuramente è un errore: deve essere 3
trend_pol3 <- fitted(pol3_import)

# Mostra i risultati dei modelli
cat("\n--- Trend lineare ---\n")
print(summary(pol1_import))
cat("\n--- Trend quadratico ---\n")
print(summary(pol2_import))
cat("\n--- Trend cubico ---\n")
print(summary(pol3_import))

# --- AUMENTA MARGINI a destra per la legenda ---
par(mar = c(5, 4, 4, 10)) # margine destro largo

# --- GRAFICO SERIE ORIGINALE ---
plot(data, import_volumi, type = "l",
     main = "Importazioni - Indici dei volumi (ISTAT)",
     xlab = "Data", ylab = "Indice dei volumi importazioni",
     col = "blue")

points(data, import_volumi, pch = 16, col = "red", cex = 0.7)
grid()

# --- TREND E MEDIA MOBILE ---
lines(data, media_mobile, col = "darkred", lwd = 2)
lines(data, trend_pol1, col = "orange", lwd = 2)   # Ordine 1
lines(data, trend_pol2, col = "purple", lwd = 2)   # Ordine 2
lines(data, trend_pol3, col = "green", lwd = 2)    # Ordine 3

# --- LEGENDA FUORI DAL GRAFICO (a destra, non coperta) ---
par(xpd=TRUE)  # disegna fuori dal plot box
legend("topright",
       inset = c(-0.35, 0),   # sposta la legenda fuori a destra
       legend = c("Serie originale", 
                  "Media mobile (12 mesi)",
                  "Trend lineare (ordine 1)",
                  "Trend quadratico (ordine 2)",
                  "Trend cubico (ordine 3)"),
       col = c("blue", "darkred", "orange", "purple", "green"),
       lwd = c(1, 2, 2, 2, 2),
       bty = "n",             # niente bordo
       cex = 0.7)             # dimensione piccola della legenda
par(xpd=FALSE) # torna al comportamento normale

# (Opzionale) Ripristina margini predefiniti per eventuali altri plot dopo
par(mar = c(5, 4, 4, 2))
