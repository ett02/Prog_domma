library(readxl)

# --- PUNTO 1: LETTURA E PREPARAZIONE DATI ---

file_path <- "C:/Users/Ettore/Prog_domma_2/Esercizio 2/1/Serie Storiche Mensili _ ISTAT.xlsx"
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

# --- PUNTO 2: SEGMENTAZIONE DELLA SERIE ---

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

# --- PUNTO 3: FUNZIONE PER COSTRUIRE LA FORMULA ---
# Questa funzione automatizza la creazione di termini di interazione
# tra polinomi (t, t^2, ...) e le variabili dummy (d1, d2, d3).

build_formula <- function(o1, o2, o3) {
  terms <- c()
  
  # Intercetta diversa per segmento. 
  # Questo è un modo standard per gestire le dummy [cite: 288, 515]
  # 'seg' è un factor, R crea automaticamente le intercette per i livelli 2 e 3
  # (il livello 1 è l'intercetta di base)
  terms <- c(terms, "seg") 
  
  # Termini polinomiali per segmento 1 (interagiti con dummy d1)
  if (o1 >= 1) for (p in 1:o1) terms <- c(terms, sprintf("I((t^%d) * d1)", p))
  
  # Termini polinomiali per segmento 2 (interagiti con dummy d2)
  if (o2 >= 1) for (p in 1:o2) terms <- c(terms, sprintf("I((t^%d) * d2)", p))
  
  # Termini polinomiali per segmento 3 (interagiti con dummy d3)
  if (o3 >= 1) for (p in 1:o3) terms <- c(terms, sprintf("I((t^%d) * d3)", p))
  
  # Combina tutti i termini in una formula 'lm'
  as.formula(paste("import_volumi ~", paste(terms, collapse = " + ")))
}

# --- PUNTO 4: SCELTA DEGLI ORDINI DA TESTARE ---
orders1 <- c(1, 2) # Segmento 1: prova lineare (1) e quadratico (2)
orders2 <- c(1, 2) # Segmento 2: prova lineare (1) e quadratico (2)
orders3 <- c(2, 3) # Segmento 3: prova quadratico (2) e cubico (3)

results <- list() # Lista vuota per salvare i risultati
idx <- 1

cat("Inizio stima di", length(orders1) * length(orders2) * length(orders3), "modelli segmentati...\n")

# --- PUNTO 5: CICLO PER LA STIMA E VALUTAZIONE ---
for (o1 in orders1) for (o2 in orders2) for (o3 in orders3) {
  
  # Costruisci la formula per questa combinazione
  f <- build_formula(o1, o2, o3)
  
  # Stima il modello di regressione lineare (come da PDF)
  m <- lm(f, data = df)
  
  # Estrai il summary del modello
  s <- summary(m)
  
  # Estrai l'Adjusted R-squared [cite: 188]
  adjr <- s$adj.r.squared
  
  # Calcolo proporzione di termini significativi (per robustezza)
  coefs <- coef(summary(m))
  term_names <- rownames(coefs)
  trend_idx <- grepl("I\\(\\(t", term_names) # Trova solo i termini polinomiali
  
  if (any(trend_idx)) {
    pvals_trend <- coefs[trend_idx, "Pr(>|t|)"]
    prop_signif <- mean(pvals_trend < 0.05, na.rm = TRUE)
  } else {
    prop_signif <- NA # Nessun termine polinomiale
  }
  
  # Score combinato (premia adjR2 alto e alta significatività)
  score <- adjr * (0.5 + 0.5 * ifelse(is.na(prop_signif), 0, prop_signif))
  
  # Salva i risultati di questa combinazione
  results[[idx]] <- list(
    o1 = o1, o2 = o2, o3 = o3,
    model = m,
    adjr = adjr,
    prop_signif = prop_signif,
    score = score,
    aic = AIC(m), # Criterio AIC [cite: 1383]
    bic = BIC(m)  # Criterio BIC [cite: 1384]
  )
  
  idx <- idx + 1
}

cat("Stima completata.\n\n")

# --- PUNTO 6: RIEPILOGO E SELEZIONE MODELLO MIGLIORE ---

# Converti la lista in un data.frame per una facile lettura
df_results <- do.call(rbind, lapply(results, function(x) 
  data.frame(o1=x$o1, o2=x$o2, o3=x$o3, adjr=x$adjr, 
             prop_signif=x$prop_signif, score=x$score, AIC=x$aic, BIC=x$bic)))

# Ordina i risultati per 'score' decrescente (il migliore prima)
df_results <- df_results[order(-df_results$score, -df_results$adjr), ]

# Stampa la tabella di riepilogo
print("Risultati riepilogo (ordinati per score):")
print(df_results)

# Seleziona il modello con lo score più alto
best <- results[[which.max(sapply(results, function(x) x$score))]]

# Stampa il risultato scelto
cat(sprintf("
--- Modello Scelto ---
Ordini: (%d, %d, %d)
Adj. R-squared: %.4f
Prop. Termini Significativi: %.2f
Score: %.4f
", 
  best$o1, best$o2, best$o3, best$adjr, best$prop_signif, best$score))

# Stampa il summary completo del modello migliore
print(summary(best$model))

# --- PUNTO 7: GRAFICO DEL MODELLO MIGLIORE ---

# Aggiungi i valori stimati dal modello MIGLIORE al grafico
# fitted(best$model) estrae i valori predetti (le ordinate stimate [cite: 165])
lines(df$datazione, fitted(best$model), col = "red", lwd = 2)

# Aggiungi una legenda
legend("topleft",
       legend = c("Serie originale", 
                  paste("Trend segmentato", best$o1, best$o2, best$o3)),
       col = c("blue", "red"),
       lwd = c(1, 2),
       bty = "n")

# --- STIMA TREND POLINOMIALI ---

# Trend lineare (ordine 1)
pol1_import <- lm(import_volumi ~ poly(t, 1, raw = TRUE))
trend_pol1 <- fitted(pol1_import)

# Trend quadratico (ordine 2)
pol2_import <- lm(import_volumi ~ poly(t, 2, raw = TRUE))
trend_pol2 <- fitted(pol2_import)

# Trend cubico (ordine 3)
pol3_import <- lm(import_volumi ~ poly(t, 3, raw = TRUE))
trend_pol3 <- fitted(pol3_import)

# Mostra i risultati dei modelli

cat("\nAdjusted R-squared Trend lineare: ")
cat(summary(pol1_import)$adj.r.squared)
cat("\n")

cat("\nAdjusted R-squared Trend quadratico: ")
cat(summary(pol2_import)$adj.r.squared)
cat("\n")

cat("\nAdjusted R-squared Trend cubico: ")
cat(summary(pol3_import)$adj.r.squared)
cat("\n")

# --- AGGIUNGI TREND AL GRAFICO ---

lines(datazione, trend_pol1, col = "orange", lwd = 2)   # Ordine 1
lines(datazione, trend_pol2, col = "purple", lwd = 2)   # Ordine 2
lines(datazione, trend_pol3, col = "green", lwd = 2)    # Ordine 3

# --- LEGENDA FUORI DAL GRAFICO (a destra, non coperta) ---
par(xpd=TRUE)  # disegna fuori dal plot box
legend("topright",
       inset = c(-0.35, 0),   # sposta la legenda fuori a destra
       legend = c("Serie originale",
                  "Trend lineare (ordine 1)",
                  "Trend quadratico (ordine 2)",
                  "Trend cubico (ordine 3)"),
       col = c("blue", "orange", "purple", "green"),
       lwd = c(1, 2, 2, 2),
       bty = "n",             # niente bordo
       cex = 0.7)             # dimensione piccola della legenda
par(xpd=FALSE) # torna al comportamento normale

# (Opzionale) Ripristina margini predefiniti per eventuali altri plot dopo
par(mar = c(5, 4, 4, 2))

# --- TEST DA GRADO 1 A GRADO 10 ---

# Definiamo il numero massimo di gradi da testare
max_degree <- 10
# Creiamo un data.frame vuoto per memorizzare i risultati
results_df <- data.frame(
  Grado = 1:max_degree, # colonna di valori da 1 a max_degree, etichettata come "Grado"
  Adj_R_Squared = numeric(max_degree) # vettore numerico di n=max_degree elementi (inizialmente uguali a 0), assegnato alla colonna "Adj_R_Squared"
)
cat("Inizio stima di", max_degree, "modelli polinomiali...\n")
# Ciclo for per calcolare l'R-quadro corretto per ogni grado
for (i in 1:max_degree) {
  # Stima il modello lineare per il grado 'i'
  # usiamo 'try' per evitare che il codice si blocchi in caso di errori con polinomi di grado molto alto
  model <- try(
    lm(import_volumi ~ poly(t, i, raw = TRUE)),
    silent = TRUE
  )
  # Se la stima del modello ha funzionato...
  if (!inherits(model, "try-error")) {
    # ...estraiamo l'R-quadro corretto e lo salviamo nel data.frame
    results_df$Adj_R_Squared[i] <- summary(model)$adj.r.squared
  } else {
    # ...altrimenti salviamo NA e andiamo avanti
    results_df$Adj_R_Squared[i] <- NA
  }
}
cat("Stima completata.\n\n")