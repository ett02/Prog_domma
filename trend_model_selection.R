## Selezione di trend segmentati con polinomi
## Obiettivo: testare combinazioni di ordini polinomiali sui 3 segmenti
## (break indicati: 2008 e 2012) e scegliere modello che massimizza
## adjusted R^2 tenendo conto della significatività dei coefficienti.

library(readxl)
library(ggplot2)
library(dplyr)

# --- Parametri ---
file_path <- "C:/Users/Ettore/Prog_domma_2/Esercizio 2/1/Serie Storiche Mensili _ ISTAT.xlsx"
break1 <- as.Date("2008-01-01")
break2 <- as.Date("2012-01-01")

# Legge i dati (adatta le colonne se necessario)
dati_raw <- read_excel(file_path)
# la riga di inizio dati e la colonna con i valori potrebbero dover essere adattate
dati_valori <- dati_raw[8:nrow(dati_raw), ]
date_col <- as.character(dati_valori[[2]])
val_col <- as.numeric(dati_valori[[6]])

# Filtra valori validi
validi <- !(is.na(date_col) | is.na(val_col))
date_col <- as.Date(paste0(date_col[validi], "-01"), format = "%Y-%m-%d")
y <- val_col[validi]

# Indice temporale in mesi (1,2,3,...)
t <- seq_along(y)

# Segment indicator basati sulle date fornite
seg <- ifelse(date_col < break1, 1, ifelse(date_col < break2, 2, 3))

# Costruisco un dataframe base
df <- data.frame(Date = date_col, y = y, t = t, seg = seg)
df$seg1 <- as.numeric(df$seg == 1)
df$seg2 <- as.numeric(df$seg == 2)
df$seg3 <- as.numeric(df$seg == 3)

# Funzione per costruire la formula dati ordini per segmento
build_formula <- function(o1, o2, o3) {
  terms <- c()
  # aggiungo dummy intercepts per i segmenti (factor)
  terms <- c(terms, "factor(seg)")
  # per ogni segmento aggiungo i termini polinomiali
  if (o1 >= 1) for (p in 1:o1) terms <- c(terms, sprintf("I((t^%d) * seg1)", p))
  if (o2 >= 1) for (p in 1:o2) terms <- c(terms, sprintf("I((t^%d) * seg2)", p))
  if (o3 >= 1) for (p in 1:o3) terms <- c(terms, sprintf("I((t^%d) * seg3)", p))
  as.formula(paste("y ~", paste(terms, collapse = " + ")))
}

# Range di ordinamenti da testare (suggeriti: primi due quadratici, terzo quadr o cubico)
orders1 <- c(1,2)  # per segmento 1
orders2 <- c(1,2)  # per segmento 2
orders3 <- c(2,3)  # per segmento 3 (secondo indicazione può essere 2 o 3)

# Lista risultati
results <- list()
idx <- 1

for (o1 in orders1) for (o2 in orders2) for (o3 in orders3) {
  f <- build_formula(o1,o2,o3)
  m <- lm(f, data = df)
  s <- summary(m)
  adjr <- s$adj.r.squared
  # estraggo p-value dei termini polinomiali
  coefs <- coef(summary(m))
  term_names <- rownames(coefs)
  trend_idx <- grepl("I\\(\\(t", term_names)
  if (any(trend_idx)) {
    pvals_trend <- coefs[trend_idx, "Pr(>|t|)"]
    prop_signif <- mean(pvals_trend < 0.05, na.rm = TRUE)
  } else {
    prop_signif <- NA
  }
  score <- adjr * (0.5 + 0.5 * ifelse(is.na(prop_signif), 0, prop_signif))

  results[[idx]] <- list(o1=o1,o2=o2,o3=o3, model=m, adjr=adjr, prop_signif=prop_signif, score=score, aic=AIC(m), bic=BIC(m))
  idx <- idx + 1
}

# Converto i risultati in dataframe di riepilogo
df_results <- do.call(rbind, lapply(results, function(x) data.frame(o1=x$o1, o2=x$o2, o3=x$o3, adjr=x$adjr, prop_signif=x$prop_signif, score=x$score, AIC=x$aic, BIC=x$bic)))
df_results <- df_results[order(-df_results$score, -df_results$adjr), ]

print("Risultati riepilogo (ordinati per score):")
print(df_results)

# Seleziono il modello migliore secondo lo score
best <- results[[which.max(sapply(results, function(x) x$score))]]
cat(sprintf("\nModello scelto: ordini (%d, %d, %d) - adjR2=%.4f - prop_signif=%.2f\n", best$o1, best$o2, best$o3, best$adjr, best$prop_signif))

# Stampo summary del modello scelto
print(summary(best$model))

# Plot confronto: osservati vs fitted
df$fit <- fitted(best$model)

# 1) Mostro il grafico nella device attiva (RStudio Plots)
plot(df$Date, df$y, type = 'l', col = 'black',
  main = 'Osservati vs Fitted (modello segmentato)', xlab = 'Date', ylab = 'Valore')
lines(df$Date, df$fit, col = 'red', lwd = 2)
legend('topleft', legend = c('Osservati', 'Fitted'), col = c('black', 'red'), lty = 1, lwd = c(1, 2))

# 2) Salvo lo stesso grafico su file PNG (copia dalla device corrente)
# Questo permette di vedere il plot in RStudio e contemporaneamente avere il file su disco.
dev.copy(png, filename = "trend_segmented_fit.png", width = 1000, height = 600)
dev.off()

# Salvo risultati dettagliati su file CSV
write.csv(df_results, file='trend_model_comparison.csv', row.names = FALSE)

cat('\nScript completato. Controllare trend_model_comparison.csv e trend_segmented_fit.png\n')

# NOTE / step successivi:
# - Se vuoi stimare i break automaticamente usa il pacchetto `strucchange` (breakpoints())
# - Per una selezione più rigorosa valuta AIC/BIC o test di Chow per break
# - La score usata (adjR2 * funzione(prop_signif)) è solo una heuristica: possiamo modificarla
#   (es. penalizzare modelli con p-value medi alti o usare un criterio basato su BIC)
