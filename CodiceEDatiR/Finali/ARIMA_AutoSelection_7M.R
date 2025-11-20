# ================================================================
# MODELLO MENSILE ROBUSTO - PREVISIONI ACCURATE
# ================================================================

# COSA FA:
# Confronto sistematico di modelli standard, selezione per AIC minimo

# Basato sui dati di MargineCommessa.xlsx

# Il progetto prende i dati mensili di produzione (quantità prodotta per commessa)
# e genera previsioni accurate per i mesi futuri, considerando diversi modelli
# e variabili aggiuntive come il numero di commesse o stagionalità.

# ================================================================
# Flusso generale
# ================================================================

# 1. Caricamento e preparazione dati
# - Il file Excel viene selezionato e caricato in R.
# - I nomi delle colonne vengono puliti.
# - I mesi italiani vengono mappati in numeri.
# - Viene creata la colonna data_mese (primo giorno del mese) e convertita in numerica.
# - Viene filtrata la quantità prodotta positiva e valida.

# 2. Aggregazione mensile
# - I dati vengono raggruppati per mese e anno.
# - Calcolo:
#     • qta_prodotta_tot = totale quantità prodotta per mese
#     • n_commesse = numero di commesse per mese
#     • commessa_tipo_A/C = conteggio commesse di tipo A e C
#     • perc_tipo_A/C = percentuale commesse di tipo A e C
# - I dati vengono ordinati cronologicamente.

# 3. Conversione in tsibble (time series tibble)
# - Vengono aggiunte variabili ausiliarie:
#     • trimestre e stagione
#     • sin_mese / cos_mese per modellare stagionalità
#     • trend = tempo trascorso dal primo mese in anni
# - La serie viene convertita in tsibble per il forecasting.

# 4. Analisi esplorativa
# - Calcolo statistiche descrittive: media, mediana, deviazione standard, min, max, coefficiente di variazione, numero osservazioni.
# - Visualizzazione della serie storica mensile con trend smoothed (LOESS).
# - Decomposizione STL per separare trend, stagionalità e residuo.

# 5. Training e selezione modelli
# - Cutoff a febbraio 2025 per creare il training set.
# - Numero mesi da prevedere: 7 (marzo → settembre 2025).
# - Viene verificata la presenza di stagionalità (almeno 24 mesi di dati).
# - Modelli addestrati:
#     • media storica (MEAN)
#     • naive, drift
#     • ETS automatico
#     • ARIMA automatico
#     • SNAIVE e ARIMA con sin/cos se stagionalità
#     • ARIMA con regressore n_commesse se disponibile
# - Selezione del miglior modello in base ad AIC più basso.

# 6. Previsione futura con regressore stimato
# - Creazione tibble con date future da prevedere.
# - Stima n_commesse future come media storica dello stesso mese.
# - Forecast sul modello selezionato.

# 7. Output e visualizzazione
# - Grafico della serie storica con linee di previsione.
# - Viene mostrata la linea di cutoff (febbraio 2025).
# - Tabella dettagliata delle previsioni:
#     • data_mese, mese_nome, anno
#     • previsione arrotondata
#     • intervallo di confidenza stimato ±15%
# - Tutto stampato in console con messaggi di avanzamento.

# 8. Conclusioni
# - Analisi completata con successo.
# - Il progetto fornisce sia la serie storica pulita che le previsioni future,
#   includendo informazioni aggiuntive sul numero e tipo di commesse,
#   trend e stagionalità, pronte per l’uso operativo o decisionale.


library(readxl)
library(fpp3)
library(dplyr)
library(lubridate)
library(janitor)
library(ggplot2)
library(forecast)
library(purrr)

# 1. CARICAMENTO E PREPARAZIONE DATI
# ================================================================
cat("📂 Caricamento dati mensili...\n")

file_path <- file.choose()
dati_raw <- read_excel(file_path, na = c("-", "NA", "")) %>%
  clean_names()

mesi_it <- c("gen", "feb", "mar", "apr", "mag", "giu", 
             "lug", "ago", "set", "ott", "nov", "dic")

dati_clean <- dati_raw %>%
  mutate(
    mese_num = match(tolower(mese), mesi_it),
    anno = as.numeric(anno),
    data_mese = as.Date(paste(anno, sprintf("%02d", mese_num), "01", sep = "-")),
    qta_prodotta = as.numeric(qta_prodotta)
  ) %>%
  filter(!is.na(data_mese), !is.na(qta_prodotta), qta_prodotta > 0)

serie_mensile <- dati_clean %>%
  group_by(data_mese, anno, mese_num) %>%
  summarise(
    qta_prodotta_tot = sum(qta_prodotta, na.rm = TRUE),
    n_commesse = n(),
    commessa_tipo_A = sum(substr(as.character(commessa), 1, 1) == "A", na.rm = TRUE),
    commessa_tipo_C = sum(substr(as.character(commessa), 1, 1) == "C", na.rm = TRUE),
    perc_tipo_A = mean(substr(as.character(commessa), 1, 1) == "A", na.rm = TRUE),
    perc_tipo_C = mean(substr(as.character(commessa), 1, 1) == "C", na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(data_mese)

# 2. CONVERSIONE IN TSIBBLE
# ================================================================
cat("🔧 Conversione in formato time series...\n")

ts_data <- serie_mensile %>%
  mutate(
    year_month = yearmonth(data_mese),
    trimestre = quarter(data_mese),
    stagione = case_when(
      mese_num %in% c(12, 1, 2) ~ "Inverno",
      mese_num %in% c(3, 4, 5) ~ "Primavera", 
      mese_num %in% c(6, 7, 8) ~ "Estate",
      TRUE ~ "Autunno"
    ),
    sin_mese = sin(2 * pi * mese_num / 12),
    cos_mese = cos(2 * pi * mese_num / 12),
    trend = as.numeric(data_mese - min(data_mese)) / 365.25
  ) %>%
  as_tsibble(index = year_month)

# 3. ANALISI ESPLORATIVA
# ================================================================
cat("📊 Analisi esplorativa dati...\n")

cat("\n=== STATISTICHE DESCRITTIVE ===\n")
summary_stats <- ts_data %>%
  summarise(
    Media = mean(qta_prodotta_tot, na.rm = TRUE),
    Mediana = median(qta_prodotta_tot, na.rm = TRUE),
    Dev_Std = sd(qta_prodotta_tot, na.rm = TRUE),
    Min = min(qta_prodotta_tot, na.rm = TRUE),
    Max = max(qta_prodotta_tot, na.rm = TRUE),
    CV = sd(qta_prodotta_tot, na.rm = TRUE) / mean(qta_prodotta_tot, na.rm = TRUE),
    N_Osservazioni = n()
  )
print(summary_stats)

p1 <- ts_data %>%
  ggplot(aes(x = year_month, y = qta_prodotta_tot)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_smooth(method = "loess", color = "red", se = FALSE, linetype = "dashed") +
  labs(title = "Serie Storica Mensile - Quantità Prodotta",
       subtitle = "Con trend smoothed (LOESS)",
       x = "Mese", y = "Quantità Prodotta") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p1)

decomp <- ts_data %>%
  model(STL(qta_prodotta_tot)) %>%
  components()
autoplot(decomp) + labs(title = "Decomposizione STL - Serie Mensile") +
  theme_minimal()

# 4. TRAINING + MODEL SELECTION
# ================================================================
cat("🤖 Training modelli predittivi...\n")

# Cutoff a febbraio
data_cutoff <- yearmonth("2025 Feb")
train_data <- ts_data %>% filter(year_month <= data_cutoff)

# Numero mesi da prevedere (marzo -> settembre = 7)
h_mesi <- 7

# Verifica stagionalità
has_seasonality <- nrow(train_data) >= 24

# Definizione modelli
modelli <- train_data %>%
  model(
    media = MEAN(qta_prodotta_tot),
    naive = NAIVE(qta_prodotta_tot),
    drift = RW(qta_prodotta_tot ~ drift()),
    ets_auto = ETS(qta_prodotta_tot),
    arima_auto = ARIMA(qta_prodotta_tot),
    !!!if(has_seasonality) {
      list(
        naive_seas = SNAIVE(qta_prodotta_tot),
        arima_features = ARIMA(qta_prodotta_tot ~ sin_mese + cos_mese)
      )
    } else {
      list()
    },
    !!!if("n_commesse" %in% colnames(train_data) && !any(is.na(train_data$n_commesse))) {
      list(arima_regressori = ARIMA(qta_prodotta_tot ~ n_commesse))
    } else {
      list()
    }
  )

# Valutazione: scegliamo AIC (più basso = migliore)
accuracy_results <- glance(modelli) %>%
  select(.model, AIC, BIC) %>%
  arrange(AIC)
print(accuracy_results)

best_model_name <- accuracy_results$.model[1]
cat(sprintf("\n🏆 Miglior modello selezionato: %s\n", best_model_name))

# Ricostruzione modello selezionato
modello_finale <- train_data %>%
  model(
    modello_selezionato = switch(best_model_name,
                                 "media" = MEAN(qta_prodotta_tot),
                                 "naive" = NAIVE(qta_prodotta_tot),
                                 "drift" = RW(qta_prodotta_tot ~ drift()),
                                 "ets_auto" = ETS(qta_prodotta_tot),
                                 "arima_auto" = ARIMA(qta_prodotta_tot),
                                 "naive_seas" = SNAIVE(qta_prodotta_tot),
                                 "arima_features" = ARIMA(qta_prodotta_tot ~ sin_mese + cos_mese),
                                 "arima_regressori" = ARIMA(qta_prodotta_tot ~ n_commesse),
                                 ETS(qta_prodotta_tot)
    )
  )

# 5. FORECAST FUTURO CON REGRESSORE STIMATO
# ================================================================
cat("📅 Preparazione dati futuri con n_commesse stimata...\n")

# Date future da prevedere
future_dates <- seq(as.Date("2025-03-01"), as.Date("2025-09-01"), by = "month")
future_data <- tibble(
  data_mese = future_dates,
  mese_num = month(future_dates)
)

# Stima n_commesse future come media storica dello stesso mese
media_commesse_mese <- train_data %>%
  mutate(mese_num = month(year_month)) %>%
  group_by(mese_num) %>%
  summarise(n_commesse_media = mean(n_commesse, na.rm = TRUE), .groups = "drop")

future_data <- future_data %>%
  left_join(media_commesse_mese, by = "mese_num") %>%
  mutate(
    year_month = yearmonth(data_mese),
    n_commesse = n_commesse_media
  ) %>%
  select(year_month, n_commesse) %>%
  as_tsibble(index = year_month)  # ✅ fondamentale: convertiamo in tsibble

# Forecast
previsioni_future <- modello_finale %>%
  forecast(new_data = future_data)

# Plot previsioni
plot_previsioni <- ts_data %>%
  autoplot(qta_prodotta_tot, color = "black") +
  autolayer(previsioni_future, alpha = 0.7) +
  geom_vline(xintercept = as.Date("2025-02-01"), linetype = "dashed", color = "red") +
  labs(title = sprintf("Previsioni %s - Marzo → Settembre", best_model_name),
       subtitle = "Cutoff a Febbraio 2025",
       x = "Mese", y = "Quantità Prodotta") +
  theme_minimal()
print(plot_previsioni)

# Tabella previsioni dettagliata
tabella_previsioni <- previsioni_future %>%
  as_tibble() %>%
  mutate(
    data_mese = as.Date(year_month),
    mese_nome = month(data_mese, label = TRUE, abbr = FALSE),
    anno = year(data_mese),
    previsione = round(.mean, 0),
    limite_inf = round(previsione * 0.85, 0),
    limite_sup = round(previsione * 1.15, 0),
    intervallo_conf = sprintf("[%d - %d] (stimato)", limite_inf, limite_sup)
  ) %>%
  select(data_mese, mese_nome, anno, previsione, intervallo_conf)

cat("\n=== PREVISIONI DETTAGLIATE ===\n")
print(tabella_previsioni)

# ================================================================
# 6. SALVATAGGIO RISULTATI IN CARTELLA DEDICATA
# ================================================================

cat("\n💾 Salvataggio risultati in cartella dedicata...\n")

# Nome del file dello script
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getActiveDocumentContext()$path
  file_name <- tools::file_path_sans_ext(basename(script_path))
} else {
  file_name <- "script_output"  # fallback
}

# Timestamp
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Creazione cartella di output
output_folder <- paste0(file_name, "_output_", timestamp)
if(!dir.exists(output_folder)) dir.create(output_folder)
cat("📂 Cartella creata:", output_folder, "\n")

# 1️⃣ Salvataggio CSV previsioni
write.csv(tabella_previsioni, file.path(output_folder, "previsioni_mensili.csv"), row.names = FALSE)
cat("✓ Previsioni salvate:", file.path(output_folder, "previsioni_mensili.csv"), "\n")

# 2️⃣ Salvataggio grafico
tryCatch({
  ggsave(file.path(output_folder, "plot_previsioni.png"), plot_previsioni, width = 14, height = 8, dpi = 300)
  cat("✓ Grafico salvato:", file.path(output_folder, "plot_previsioni.png"), "\n")
}, error = function(e) {
  cat("⚠ Errore nel salvataggio grafico:", e$message, "\n")
  
  # Salva un grafico semplificato
  p_simple <- ts_data %>%
    autoplot(qta_prodotta_tot, color = "steelblue") +
    labs(title = "Serie Storica Quantità Prodotta", x = "Mese", y = "Quantità") +
    theme_minimal()
  
  ggsave(file.path(output_folder, "serie_storica.png"), p_simple, width = 12, height = 6)
  cat("✓ Grafico semplificato salvato:", file.path(output_folder, "serie_storica.png"), "\n")
})

cat("\n✅ Tutti i risultati sono stati salvati nella cartella:", output_folder, "\n")


cat("\n✅ Analisi completata con successo!\n")
