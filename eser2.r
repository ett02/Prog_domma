# Carico le librerie necessarie
library(dplyr)
library(ggplot2)

# Lettura dei dati
dati_spesa <- read.table("Esercizio 2/2/1_spesa.txt", header = TRUE, sep = "\t")

# 1. Visualizzazione delle prime righe dei dati
head(dati_spesa)

# 2. Statistiche descrittive base
summary(dati_spesa)

# 3. Visualizzazione della distribuzione della spesa
ggplot(dati_spesa, aes(x = spesa)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribuzione della Spesa Alimentare",
       x = "Spesa", y = "Frequenza") +
  theme_minimal()

# 4. Relazione tra reddito e spesa
ggplot(dati_spesa, aes(x = reddito, y = spesa)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relazione tra Reddito e Spesa",
       x = "Reddito", y = "Spesa") +
  theme_minimal()

# 5. Box plot della spesa per numero di figli
ggplot(dati_spesa, aes(x = factor(figli), y = spesa)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Spesa per Numero di Figli",
       x = "Numero di Figli", y = "Spesa") +
  theme_minimal()

# 6. Modello di regressione base
modello <- lm(spesa ~ reddito + figli + metri + sesso + genitori + zona, data = dati_spesa)
summary(modello)

# 7. Correlazioni tra variabili numeriche
cor(dati_spesa[c("spesa", "reddito", "figli", "metri")])

# 8. Analisi per gruppi di famiglie
spesa_per_figli <- dati_spesa %>%
  group_by(figli) %>%
  summarise(
    spesa_media = mean(spesa),
    spesa_sd = sd(spesa)
  )
print(spesa_per_figli)

# 9. Test statistici
# Test di normalità per la spesa
shapiro.test(dati_spesa$spesa)

# ANOVA per differenze tra gruppi di figli
anova_result <- aov(spesa ~ factor(figli), data = dati_spesa)
summary(anova_result)

# 10. Identificazione outliers nella spesa
outliers <- boxplot.stats(dati_spesa$spesa)$out
print("Valori outlier nella spesa:")
print(outliers)

# 11. Statistiche per gruppi di reddito
dati_spesa$gruppo_reddito <- cut(dati_spesa$reddito, 
                                breaks = quantile(dati_spesa$reddito, probs = seq(0, 1, 0.25)),
                                labels = c("Basso", "Medio-Basso", "Medio-Alto", "Alto"),
                                include.lowest = TRUE)

spesa_per_reddito <- dati_spesa %>%
  group_by(gruppo_reddito) %>%
  summarise(
    spesa_media = mean(spesa),
    spesa_sd = sd(spesa),
    n = n()
  )
print(spesa_per_reddito)


# 12. Analisi dei pattern nei dati

# Ordiniamo i dati per reddito per vedere se ci sono pattern
dati_ordinati <- dati_spesa %>%
  arrange(reddito)

# Grafico del trend della spesa rispetto al reddito (ordinato)
ggplot(dati_ordinati, aes(x = 1:nrow(dati_ordinati), y = spesa)) +
  geom_line(color = "blue", alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Trend della Spesa (ordinato per reddito)",
       x = "Osservazioni (ordinate per reddito)",
       y = "Spesa") +
  theme_minimal()

# Analisi dei pattern per zona di abitazione
ggplot(dati_spesa, aes(x = reddito, y = spesa, color = factor(zona))) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Pattern della Spesa per Zona di Abitazione",
       x = "Reddito",
       y = "Spesa",
       color = "Zona (1=Urbana)") +
  theme_minimal()

# Analisi dei pattern per composizione familiare
ggplot(dati_spesa, aes(x = reddito, y = spesa, color = factor(figli))) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Pattern della Spesa per Numero di Figli",
       x = "Reddito",
       y = "Spesa",
       color = "Numero di Figli") +
  theme_minimal()

# Box plot della spesa per zona e numero di figli
ggplot(dati_spesa, aes(x = factor(figli), y = spesa, fill = factor(zona))) +
  geom_boxplot() +
  labs(title = "Distribuzione della Spesa per Figli e Zona",
       x = "Numero di Figli",
       y = "Spesa",
       fill = "Zona (1=Urbana)") +
  theme_minimal()

# Analisi della variazione della spesa
# Calcoliamo la variazione della spesa rispetto alla media per gruppo di reddito
dati_spesa <- dati_spesa %>%
  group_by(gruppo_reddito) %>%
  mutate(
    spesa_media_gruppo = mean(spesa),
    variazione_spesa = spesa - spesa_media_gruppo
  ) %>%
  ungroup()

# Grafico della variazione della spesa
ggplot(dati_spesa, aes(x = reddito, y = variazione_spesa)) +
  geom_point(aes(color = factor(figli))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Variazione della Spesa rispetto alla Media del Gruppo di Reddito",
       x = "Reddito",
       y = "Variazione della Spesa",
       color = "Numero di Figli") +
  theme_minimal()

# Analisi delle proporzioni di spesa rispetto al reddito
dati_spesa$proporzione_spesa <- dati_spesa$spesa / dati_spesa$reddito * 100

# Grafico della proporzione di spesa
ggplot(dati_spesa, aes(x = reddito, y = proporzione_spesa)) +
  geom_point(aes(color = factor(figli))) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Proporzione della Spesa sul Reddito",
       x = "Reddito",
       y = "Percentuale Spesa sul Reddito",
       color = "Numero di Figli") +
  theme_minimal()

# Statistiche sulla proporzione di spesa
summary_proporzioni <- dati_spesa %>%
  group_by(gruppo_reddito) %>%
  summarise(
    proporzione_media = mean(proporzione_spesa),
    proporzione_sd = sd(proporzione_spesa)
  )
print("Proporzione media di spesa sul reddito per gruppo:")
print(summary_proporzioni)

# Nuovi plot aggiuntivi

# 13. Analisi multivariata avanzata

# Installazione e caricamento pacchetti aggiuntivi
if(!require(plotly)) install.packages("plotly")
if(!require(viridis)) install.packages("viridis")
library(plotly)
library(viridis)

# Plot 3D interattivo
plot_ly(dati_spesa, 
        x = ~reddito, 
        y = ~metri, 
        z = ~spesa,
        color = ~factor(figli),
        type = "scatter3d",
        mode = "markers") %>%
  layout(title = "Relazione 3D tra Reddito, Metri quadri e Spesa",
         scene = list(
           xaxis = list(title = "Reddito"),
           yaxis = list(title = "Metri quadri"),
           zaxis = list(title = "Spesa")
         ))

# Heatmap della correlazione con valori più dettagliati
cor_matrix <- cor(dati_spesa[c("spesa", "reddito", "figli", "metri", "proporzione_spesa")])
plot_ly(
  x = colnames(cor_matrix),
  y = colnames(cor_matrix),
  z = cor_matrix,
  type = "heatmap",
  colors = viridis(100)
) %>%
  layout(title = "Heatmap delle Correlazioni")

# Violin plot per confrontare distribuzioni
ggplot(dati_spesa, aes(x = factor(figli), y = spesa, fill = factor(zona))) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, alpha = 0.8) +
  scale_fill_viridis_d() +
  labs(title = "Distribuzione della Spesa per Numero di Figli e Zona",
       x = "Numero di Figli",
       y = "Spesa",
       fill = "Zona") +
  theme_minimal()

# Grafico radar per caratteristiche medie per gruppo di reddito
library(fmsb)

# Preparazione dati per il grafico radar
radar_data <- dati_spesa %>%
  group_by(gruppo_reddito) %>%
  summarise(
    Spesa_media = mean(spesa),
    Metri_medi = mean(metri),
    Figli_medi = mean(figli),
    Prop_spesa = mean(proporzione_spesa)
  ) %>%
  select(-gruppo_reddito) %>%
  as.data.frame()

# Normalizzazione dei dati
radar_data_norm <- data.frame(lapply(radar_data, scales::rescale))

# Aggiunta di max e min per il grafico radar
radar_data_plot <- rbind(rep(1,ncol(radar_data_norm)), 
                        rep(0,ncol(radar_data_norm)), 
                        radar_data_norm)

# Creazione del grafico radar
par(mar = c(1, 1, 1, 1))
radarchart(radar_data_plot,
           pcol = rainbow(4),
           pfcol = rainbow(4, alpha = 0.3),
           plwd = 2,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0, 1, 0.2),
           title = "Caratteristiche per Gruppo di Reddito")
legend("topright", 
       legend = unique(dati_spesa$gruppo_reddito),
       col = rainbow(4),
       lty = 1,
       lwd = 2,
       bty = "n")

# Salvataggio automatico di tutti i plot
# Crea una directory per i plot se non esiste
if (!dir.exists("plots")) dir.create("plots")

# Funzione per salvare i plot
save_all_plots <- function() {
  # Lista di tutti i plot ggplot
  plot_list <- mget(ls(pattern = "^plot"), envir = .GlobalEnv)
  
  # Salva ogni plot
  for (i in seq_along(plot_list)) {
    if (inherits(plot_list[[i]], "ggplot")) {
      ggsave(
        filename = paste0("plots/plot_", i, ".png"),
        plot = plot_list[[i]],
        width = 10,
        height = 6,
        dpi = 300
      )
    }
  }
}

# Esegui il salvataggio
save_all_plots()