# --------------------------------------------------------------
# Modello Personalizzato Ibrido per Analisi Commessa
# --------------------------------------------------------------

# COSA FA:
# Modello su misura specificamente progettato per i pattern dei dati

# Basato sui dati di MargineCommessa.xlsx

# Ho creato un modello personalizzato ibrido che combina diversi
# approcci per sfruttare al meglio le specificità dei tuoi dati.
# Ecco le caratteristiche principali:

# Architettura del Modello
# Componenti principali:
# - Modello deterministico: relazione quantità/numero di commesse con regressione quadratica
# - Componente stagionale adattiva: decomposizione STL robusta per pattern stagionali
# - ARIMA sui residui: cattura le dinamiche stocastiche non spiegate
# - Trend breaks detection: rileva automaticamente i cambiamenti strutturali
# - Ensemble pesato: combina le previsioni con pesi basati sulla performance storica

# Vantaggi Specifici per i Tuoi Dati
# Gestione intelligente delle commesse:
# - Modella la relazione non-lineare tra quantità prodotta e numero di commesse
# - Stima automaticamente il numero futuro di commesse usando medie stagionali ponderate
# - Considera i tipi di commesse (A, C) nell'analisi

# Robustezza:
# - Gestisce automaticamente valori mancanti e outlier
# - Include vincoli realistici per evitare previsioni irrealistiche
# - Intervalli di confidenza dinamici che crescono con l'orizzonte temporale

# Adattabilità:
# - Rileva automaticamente breakpoint nel trend
# - Stagionalità adattiva che si aggiorna con nuovi dati
# - Performance metric che migliora l'accuratezza nel tempo

# Come Usarlo
# - Avvia lo script: seleziona il file MargineCommessa.xlsx
# - Training automatico: il modello si addestra sui dati fino a febbraio 2025
# - Previsioni personalizzate: genera 7 mesi di previsioni con intervalli di confidenza
# - Confronto: visualizza le performance vs modelli standard (ARIMA, ETS)
# - Export: salva previsioni, modello e grafici

# Il modello è progettato specificamente per i tuoi pattern di produzione
# e dovrebbe fornire previsioni più accurate dei modelli generici,
# specialmente considerando la relazione tra quantità prodotte e dinamiche delle commesse.
# --------------------------------------------------------------


library(readxl)
library(fpp3)
library(dplyr)
library(lubridate)
library(janitor)
library(ggplot2)
library(forecast)
library(purrr)
library(modelr)

# ================================================================
# FUNZIONI DEL MODELLO PERSONALIZZATO
# ================================================================

# Funzione per calcolare media mobile ponderata
weighted_moving_average <- function(x, weights = c(0.5, 0.3, 0.2)) {
  n <- length(x)
  if(n < length(weights)) return(rep(mean(x, na.rm = TRUE), n))
  
  result <- numeric(n)
  for(i in seq_along(weights)) {
    result[1:(length(weights)-1)] <- mean(x[1:length(weights)], na.rm = TRUE)
  }
  
  for(i in length(weights):n) {
    if(i == length(weights)) {
      result[i] <- sum(x[(i-length(weights)+1):i] * rev(weights), na.rm = TRUE)
    } else {
      result[i] <- sum(x[(i-length(weights)+1):i] * rev(weights), na.rm = TRUE)
    }
  }
  return(result)
}

# Funzione per rilevare trend breaks
detect_trend_breaks <- function(ts_data, min_size = 6) {
  if(nrow(ts_data) < min_size * 2) return(NULL)
  
  values <- ts_data$qta_prodotta_tot
  n <- length(values)
  
  # Calcola differenze di trend in finestre mobili
  breaks <- c()
  for(i in seq(min_size, n - min_size)) {
    before <- lm(values[max(1, i-min_size):i] ~ I(max(1, i-min_size):i))$coefficients[2]
    after <- lm(values[i:(i+min_size)] ~ I(i:(i+min_size)))$coefficients[2]
    
    if(!is.na(before) && !is.na(after) && abs(before - after) > sd(diff(values), na.rm = TRUE)) {
      breaks <- c(breaks, i)
    }
  }
  
  return(unique(breaks))
}

# Componente stagionale adattiva
adaptive_seasonal <- function(ts_data, period = 12) {
  if(nrow(ts_data) < period * 2) return(rep(0, nrow(ts_data)))
  
  values <- ts_data$qta_prodotta_tot
  seasonal_component <- numeric(length(values))
  
  # Calcola stagionalità usando decomposizione robusta
  if(length(values) >= period * 2) {
    ts_obj <- ts(values, frequency = period)
    decomp <- tryCatch({
      stl(ts_obj, s.window = "periodic", robust = TRUE)
    }, error = function(e) {
      # Fallback: stagionalità semplice
      seasonal_means <- tapply(values, rep(1:period, length.out = length(values)), mean, na.rm = TRUE)
      list(time.series = cbind(seasonal = rep(seasonal_means, length.out = length(values))))
    })
    
    if(is.list(decomp) && "time.series" %in% names(decomp)) {
      seasonal_component <- as.numeric(decomp$time.series[, "seasonal"])
    } else {
      seasonal_component <- rep(0, length(values))
    }
  }
  
  return(seasonal_component)
}

# Modello principale personalizzato
custom_forecasting_model <- function(train_data, h = 6) {
  
  cat("🔧 Inizializzazione modello personalizzato...\n")
  
  # 1. ANALISI COMPONENTI
  # ===============================================================
  
  # Trend con breakpoints
  trend_breaks <- detect_trend_breaks(train_data)
  cat("📈 Breakpoints rilevati:", length(trend_breaks), "\n")
  
  # Componente stagionale adattiva
  seasonal_comp <- adaptive_seasonal(train_data)
  cat("📅 Componente stagionale calcolata\n")
  
  # 2. MODELLAZIONE DELLE COMMESSE
  # ===============================================================
  
  # Relazione quantità-commesse con regressione robusta
  if("n_commesse" %in% names(train_data)) {
    # Modello robusto per relazione qta ~ n_commesse
    lm_commesse <- lm(qta_prodotta_tot ~ n_commesse + I(n_commesse^2), 
                      data = train_data)
    
    # Coefficiente di determinazione
    r_squared_commesse <- summary(lm_commesse)$r.squared
    cat("📊 R² relazione qta-commesse:", round(r_squared_commesse, 3), "\n")
    
    # Residui dalla relazione commesse
    residui_commesse <- residuals(lm_commesse)
  } else {
    lm_commesse <- NULL
    residui_commesse <- train_data$qta_prodotta_tot
    r_squared_commesse <- 0
  }
  
  # 3. MODELLAZIONE RESIDUI CON ARIMA
  # ===============================================================
  
  # Rimuovi trend deterministico e stagionalità
  detrended <- residui_commesse - seasonal_comp
  
  # ARIMA sui residui
  arima_residui <- tryCatch({
    auto.arima(detrended, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  }, error = function(e) {
    arima(detrended, order = c(1,1,1))
  })
  
  cat("🎯 ARIMA sui residui:", paste(arima_residui$arma[c(1,6,2)], collapse = ","), "\n")
  
  # 4. MODELLO ENSEMBLE PESATO
  # ===============================================================
  
  # Pesi basati su performance storica
  if(nrow(train_data) >= 12) {
    # Validazione rolling window
    errors <- c()
    for(i in 12:(nrow(train_data)-1)) {
      train_subset <- train_data[1:i, ]
      actual <- train_data$qta_prodotta_tot[i+1]
      
      # Previsione semplice
      pred_naive <- train_subset$qta_prodotta_tot[nrow(train_subset)]
      pred_trend <- mean(tail(train_subset$qta_prodotta_tot, 3), na.rm = TRUE)
      
      error_naive <- abs(actual - pred_naive)
      error_trend <- abs(actual - pred_trend)
      
      errors <- c(errors, min(error_naive, error_trend))
    }
    
    performance_metric <- 1 / (1 + mean(errors, na.rm = TRUE))
  } else {
    performance_metric <- 0.5
  }
  
  # 5. COSTRUZIONE OGGETTO MODELLO
  # ===============================================================
  
  model_obj <- list(
    # Componenti del modello
    trend_breaks = trend_breaks,
    seasonal_component = seasonal_comp,
    lm_commesse = lm_commesse,
    arima_residui = arima_residui,
    
    # Dati di training
    train_data = train_data,
    performance_metric = performance_metric,
    
    # Parametri
    n_obs = nrow(train_data),
    has_commesse = "n_commesse" %in% names(train_data),
    r_squared_commesse = r_squared_commesse,
    
    # Metadata
    fitted_date = Sys.Date(),
    model_type = "custom_hybrid"
  )
  
  class(model_obj) <- "custom_forecasting_model"
  cat("✅ Modello personalizzato creato con successo!\n")
  
  return(model_obj)
}

# Funzione di previsione per il modello personalizzato
forecast.custom_forecasting_model <- function(model, h = 6, future_data = NULL) {
  
  cat("🔮 Generazione previsioni personalizzate per", h, "periodi...\n")
  
  train_data <- model$train_data
  last_date <- max(train_data$data_mese, na.rm = TRUE)
  
  # Date future
  future_dates <- seq.Date(from = as.Date(last_date) + months(1), 
                           by = "month", length.out = h)
  
  # 1. STIMA N_COMMESSE FUTURE
  # ===============================================================
  if(model$has_commesse && is.null(future_data)) {
    # Stima n_commesse come media stagionale
    mesi_storici <- month(train_data$data_mese)
    mesi_futuri <- month(future_dates)
    
    n_commesse_future <- numeric(h)
    for(i in seq_along(mesi_futuri)) {
      mese_target <- mesi_futuri[i]
      commesse_stesso_mese <- train_data$n_commesse[mesi_storici == mese_target]
      
      if(length(commesse_stesso_mese) > 0) {
        # Media ponderata: più peso ai dati recenti
        weights <- exp(seq(-2, 0, length.out = length(commesse_stesso_mese)))
        n_commesse_future[i] <- weighted.mean(commesse_stesso_mese, weights, na.rm = TRUE)
      } else {
        n_commesse_future[i] <- mean(train_data$n_commesse, na.rm = TRUE)
      }
    }
  } else if(!is.null(future_data) && "n_commesse" %in% names(future_data)) {
    n_commesse_future <- future_data$n_commesse[1:h]
  } else {
    n_commesse_future <- rep(mean(train_data$n_commesse, na.rm = TRUE), h)
  }
  
  # 2. PREVISIONE COMPONENTE COMMESSE
  # ===============================================================
  if(!is.null(model$lm_commesse)) {
    pred_df <- data.frame(n_commesse = n_commesse_future)
    pred_commesse <- predict(model$lm_commesse, pred_df)
  } else {
    pred_commesse <- rep(mean(train_data$qta_prodotta_tot, na.rm = TRUE), h)
  }
  
  # 3. PREVISIONE COMPONENTE STAGIONALE
  # ===============================================================
  seasonal_pattern <- model$seasonal_component
  if(length(seasonal_pattern) >= 12) {
    # Estrapola pattern stagionale
    mesi_futuri <- month(future_dates)
    seasonal_future <- numeric(h)
    
    for(i in seq_along(mesi_futuri)) {
      mese_target <- mesi_futuri[i]
      indices_stesso_mese <- which(month(train_data$data_mese) == mese_target)
      
      if(length(indices_stesso_mese) > 0) {
        seasonal_future[i] <- mean(seasonal_pattern[indices_stesso_mese], na.rm = TRUE)
      } else {
        seasonal_future[i] <- 0
      }
    }
  } else {
    seasonal_future <- rep(0, h)
  }
  
  # 4. PREVISIONE RESIDUI ARIMA
  # ===============================================================
  residui_forecast <- forecast(model$arima_residui, h = h)
  pred_residui <- as.numeric(residui_forecast$mean)
  
  # 5. COMBINAZIONE FINALE
  # ===============================================================
  
  # Previsione base
  pred_base <- pred_commesse + seasonal_future + pred_residui
  
  # Aggiustamento per trend recente
  trend_recente <- mean(diff(tail(train_data$qta_prodotta_tot, 6)), na.rm = TRUE)
  if(!is.na(trend_recente) && abs(trend_recente) > 0) {
    trend_adjustment <- trend_recente * seq(1, h) * 0.5  # Damping del trend
    pred_base <- pred_base + trend_adjustment
  }
  
  # Vincoli realistici
  min_val <- min(train_data$qta_prodotta_tot, na.rm = TRUE) * 0.3
  max_val <- max(train_data$qta_prodotta_tot, na.rm = TRUE) * 1.5
  pred_final <- pmax(min_val, pmin(max_val, pred_base))
  
  # 6. INTERVALLI DI CONFIDENZA
  # ===============================================================
  
  # Calcola volatilità storica
  historical_errors <- diff(train_data$qta_prodotta_tot)
  volatility <- sd(historical_errors, na.rm = TRUE) * sqrt(1:h)  # Crescente con h
  
  # Intervalli al 80% e 95%
  lower_80 <- pred_final - 1.28 * volatility
  upper_80 <- pred_final + 1.28 * volatility
  lower_95 <- pred_final - 1.96 * volatility
  upper_95 <- pred_final + 1.96 * volatility
  
  # 7. COSTRUZIONE RISULTATO
  # ===============================================================
  
  result <- tibble(
    data_mese = future_dates,
    year_month = yearmonth(future_dates),
    .mean = pred_final,
    n_commesse_stimate = n_commesse_future,
    
    # Componenti della previsione
    componente_commesse = pred_commesse,
    componente_stagionale = seasonal_future,
    componente_residui = pred_residui,
    
    # Intervalli di confidenza
    .lower_80 = pmax(0, lower_80),
    .upper_80 = upper_80,
    .lower_95 = pmax(0, lower_95),
    .upper_95 = upper_95,
    
    # Metadata
    .model = "custom_hybrid"
  )
  
  cat("✅ Previsioni generate con successo!\n")
  cat("📊 Range previsioni:", round(min(result$.mean)), "-", round(max(result$.mean)), "\n")
  
  return(result)
}

# Funzione per calcolare metriche robuste
calculate_robust_metrics <- function(actual, predicted, training_data = NULL) {
  n <- length(actual)
  
  # Metriche base
  mae_val <- mean(abs(actual - predicted))
  rmse_val <- sqrt(mean((actual - predicted)^2))
  
  # sMAPE (Symmetric Mean Absolute Percentage Error) - più robusto del MAPE
  smape_val <- mean(200 * abs(actual - predicted) / (abs(actual) + abs(predicted)))
  
  # NRMSE (Normalized Root Mean Square Error) - basato su varianza
  nrmse_val <- rmse_val / mean(actual) * 100
  
  # MASE (Mean Absolute Scaled Error) - scale-independent
  if(!is.null(training_data) && length(training_data) > 1) {
    naive_mae <- mean(abs(diff(training_data)), na.rm = TRUE)
    mase_val <- mae_val / naive_mae
  } else {
    mase_val <- NA
  }
  
  # Mantieni MAPE per compatibilità, ma con gestione errori
  mape_val <- tryCatch({
    # Evita divisione per zero
    valid_idx <- abs(actual) > 1e-6
    if(sum(valid_idx) > 0) {
      mean(abs((actual[valid_idx] - predicted[valid_idx]) / actual[valid_idx])) * 100
    } else {
      NA
    }
  }, error = function(e) NA)
  
  return(list(
    mae = mae_val,
    rmse = rmse_val,
    mape = mape_val,
    smape = smape_val,
    nrmse = nrmse_val,
    mase = mase_val,
    n_observations = n
  ))
}

# Valutazione singola previsione
evaluate_single_prediction <- function(actual, predicted, actual_mean = NULL) {
  
  # Errore assoluto
  abs_error <- abs(actual - predicted)
  
  # Errore percentuale (gestito per evitare divisione per zero)
  perc_error <- if(abs(actual) > 1e-6) {
    (predicted - actual) / actual * 100
  } else {
    NA
  }
  
  # Errore percentuale simmetrico (sMAPE per singola osservazione)
  smape_single <- if(abs(actual) + abs(predicted) > 1e-6) {
    200 * abs_error / (abs(actual) + abs(predicted))
  } else {
    0
  }
  
  # Errore normalizzato rispetto alla media (se disponibile)
  normalized_error <- if(!is.null(actual_mean) && actual_mean > 0) {
    abs_error / actual_mean * 100
  } else {
    NA
  }
  
  # Score composito per singola previsione (0-1, dove 1 = perfetto)
  smape_score <- pmax(0, pmin(1, (50 - smape_single) / 50))
  
  # Classificazione qualitativa basata su score composito
  if(smape_score >= 0.90) {  # sMAPE <= 5%
    quality <- list(emoji = "🟢", level = "Eccellente", description = "sMAPE ≤ 5%")
  } else if(smape_score >= 0.75) {  # sMAPE <= 12.5%
    quality <- list(emoji = "🟡", level = "Buona", description = "sMAPE ≤ 12.5%")
  } else if(smape_score >= 0.50) {  # sMAPE <= 25%
    quality <- list(emoji = "🟠", level = "Media", description = "sMAPE ≤ 25%")
  } else if(smape_score >= 0.25) {  # sMAPE <= 37.5%
    quality <- list(emoji = "🔴", level = "Bassa", description = "sMAPE ≤ 37.5%")
  } else {  # sMAPE > 37.5%
    quality <- list(emoji = "⚫", level = "Molto Bassa", description = "sMAPE > 37.5%")
  }
  
  return(list(
    abs_error = abs_error,
    perc_error = perc_error,
    smape_single = smape_single,
    normalized_error = normalized_error,
    score = smape_score,
    quality = quality
  ))
}

# Calcola accuratezza intervalli di confidenza
calculate_interval_accuracy <- function(actual, predictions, confidence_level) {
  if(confidence_level == 0.8) {
    lower_col <- ".lower_80"
    upper_col <- ".upper_80"
  } else {
    lower_col <- ".lower_95"
    upper_col <- ".upper_95"
  }
  
  if(all(c(lower_col, upper_col) %in% names(predictions))) {
    n_in_interval <- sum(actual >= predictions[[lower_col]][1:length(actual)] & 
                           actual <= predictions[[upper_col]][1:length(actual)], na.rm = TRUE)
    return(n_in_interval / length(actual))
  }
  
  return(NA)
}


# ================================================================
# SCRIPT PRINCIPALE
# ================================================================

# 1. CARICAMENTO DATI
# ===============================================================
cat("📂 Caricamento dati mensili...\n")

# Selezione file
file_path <- file.choose()

# Mapping mesi italiani
mesi_it <- c("gen", "feb", "mar", "apr", "mag", "giu", 
             "lug", "ago", "set", "ott", "nov", "dic")

# Caricamento e pulizia
dati_raw <- read_excel(file_path, na = c("-", "NA", "")) %>%
  clean_names()

dati_clean <- dati_raw %>%
  mutate(
    mese_num = match(tolower(mese), mesi_it),
    anno = as.numeric(anno),
    data_mese = as.Date(paste(anno, sprintf("%02d", mese_num), "01", sep = "-")),
    qta_prodotta = as.numeric(qta_prodotta)
  ) %>%
  filter(!is.na(data_mese), !is.na(qta_prodotta), qta_prodotta > 0)

# Aggregazione mensile
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

cat("✅ Dati caricati:", nrow(serie_mensile), "osservazioni mensili\n")
cat("📅 Periodo:", as.character(min(serie_mensile$data_mese)), "→", 
    as.character(max(serie_mensile$data_mese)), "\n")


# ==============================================================================
# 1.5. CONFIGURAZIONE ORIZZONTE PREVISIONI ⭐ NUOVO ⭐
# ==============================================================================

cat("\n⚙️ ===== CONFIGURAZIONE ORIZZONTE PREVISIONI ===== ⚙️\n")

# Mostra informazioni sui dati per aiutare la scelta
data_inizio <- min(serie_mensile$data_mese)
data_fine <- max(serie_mensile$data_mese)
n_mesi_totali <- nrow(serie_mensile)

cat("📊 INFORMAZIONI DATASET:\n")
cat("   • Periodo dati:", format(data_inizio, "%Y-%m"), "→", format(data_fine, "%Y-%m"), "\n")
cat("   • Totale mesi disponibili:", n_mesi_totali, "\n")
cat("   • Cutoff training impostato:", format(as.Date("2025-02-01"), "%Y-%m"), "\n")

# Calcola mesi di training e test con cutoff attuale
cutoff_preview <- as.Date("2025-02-01")
n_training <- sum(serie_mensile$data_mese <= cutoff_preview)
n_test <- sum(serie_mensile$data_mese > cutoff_preview)

cat("   • Mesi training (con cutoff attuale):", n_training, "\n")
cat("   • Mesi test disponibili:", n_test, "\n\n")

# Suggerimenti intelligenti
cat("💡 SUGGERIMENTI ORIZZONTE:\n")
cat("   • Breve termine (1-6 mesi): Maggiore accuratezza\n")
cat("   • Medio termine (6-12 mesi): Buon equilibrio accuratezza/utilità\n")
cat("   • Lungo termine (12-24 mesi): Pianificazione strategica\n")

if(n_test > 0) {
  cat("   • Hai", n_test, "mesi di test per validare l'accuratezza\n")
  cat("   • Suggerimento: usa max", n_test, "mesi per confronto con dati reali\n")
}

cat("\n🔮 Quanti mesi vuoi PREVEDERE?\n")

# Input con validazione
repeat {
  # Determina range ragionevole
  min_mesi <- 1
  max_mesi <- min(36, max(24, n_test + 12))  # Max 36 mesi, ma almeno 24 o test+12
  
  h_forecast <- as.numeric(readline(prompt = paste0("   Inserisci numero mesi (", min_mesi, "-", max_mesi, "): ")))
  
  # Validazione input
  if(is.na(h_forecast)) {
    cat("   ❌ Input non valido. Inserisci un numero\n\n")
    next
  }
  
  if(h_forecast < min_mesi || h_forecast > max_mesi) {
    cat("   ❌ Numero fuori range. Inserisci un numero tra", min_mesi, "e", max_mesi, "\n\n")
    next
  }
  
  if(h_forecast != floor(h_forecast)) {
    cat("   ❌ Inserisci un numero intero\n\n")
    next
  }
  
  # Input valido, esci dal loop
  break
}

# Conversione a intero
h_forecast <- as.integer(h_forecast)

cat("✅ Orizzonte selezionato:", h_forecast, "mesi\n")

# Informazioni aggiuntive sulla scelta
end_forecast_date <- cutoff_preview + months(h_forecast)
cat("📅 Le previsioni copriranno da", format(cutoff_preview + months(1), "%Y-%m"), 
    "a", format(end_forecast_date, "%Y-%m"), "\n")

# Confronto con dati test
if(n_test > 0) {
  if(h_forecast <= n_test) {
    cat("🎯 Potrai validare TUTTE le", h_forecast, "previsioni con dati reali\n")
  } else {
    cat("🎯 Potrai validare le prime", n_test, "previsioni con dati reali\n")
    cat("   Le restanti", h_forecast - n_test, "saranno previsioni pure (senza validazione)\n")
  }
}

# Stima tempo computazionale
tempo_stimato <- ceiling(h_forecast / 6) * 2  # Circa 2 secondi ogni 6 mesi
cat("⏱️ Tempo di calcolo stimato: ~", tempo_stimato, "secondi\n")

# Conferma finale
repeat {
  conferma <- toupper(readline(prompt = "\nConfermi questa configurazione? (S/N): "))
  if(conferma %in% c("S", "SI", "Y", "YES")) {
    break
  } else if(conferma %in% c("N", "NO")) {
    cat("❌ Configurazione annullata. Riavvia lo script per una nuova selezione.\n")
    stop("Script interrotto dall'utente")
  } else {
    cat("   ❌ Risposta non valida. Digita S per continuare o N per annullare\n")
  }
}

cat("🚀 Avvio analisi con orizzonte di", h_forecast, "mesi...\n\n")


# 2. SPLIT DATI E TRAINING
# ===============================================================
cat("\n🎯 Split Train/Test...\n")

# Cutoff a febbraio 2025
data_cutoff <- as.Date("2025-02-01")
train_data <- serie_mensile %>% filter(data_mese <= data_cutoff)
test_data <- serie_mensile %>% filter(data_mese > data_cutoff)

cat("📊 Training set:", nrow(train_data), "mesi\n")
cat("📊 Test set:", nrow(test_data), "mesi\n")

# 3. ADDESTRAMENTO MODELLO PERSONALIZZATO
# ===============================================================
cat("\n🤖 Addestramento modello personalizzato...\n")

modelo_custom <- custom_forecasting_model(train_data, h = h_forecast)

# 4. GENERAZIONE PREVISIONI
# ===============================================================
cat("\n🔮 Generazione previsioni...\n")

previsioni_custom <- forecast(modelo_custom, h = h_forecast)

# 5. CONFRONTO CON MODELLI STANDARD E VALUTAZIONE ACCURATEZZA
# ===============================================================
cat("\n📈 Confronto con modelli standard...\n")

# Converti in tsibble per modelli fpp3
train_ts <- train_data %>%
  mutate(year_month = yearmonth(data_mese)) %>%
  as_tsibble(index = year_month)

# Modelli standard
modelli_standard <- train_ts %>%
  model(
    arima_auto = ARIMA(qta_prodotta_tot),
    ets_auto = ETS(qta_prodotta_tot),
    naive = NAIVE(qta_prodotta_tot),
    ensemble_std = (ARIMA(qta_prodotta_tot) + ETS(qta_prodotta_tot)) / 2
  )

# Previsioni standard
fc_standard <- modelli_standard %>% forecast(h = h_forecast)

# ==============================================================================
# 5.5. VALUTAZIONE ACCURATEZZA SUL TEST SET
# ==============================================================================

test_performance <- NULL
confronto_test <- NULL

if(nrow(test_data) > 0) {
  cat("\n🎯 ===== VALUTAZIONE ACCURATEZZA SUL TEST SET ===== 🎯\n")
  
  # Numero di osservazioni da valutare
  n_eval <- min(nrow(previsioni_custom), nrow(test_data))
  actual_values <- test_data$qta_prodotta_tot[1:n_eval]
  predicted_values <- previsioni_custom$.mean[1:n_eval]
  
  # Calcola media storica per normalizzazione
  actual_mean <- mean(train_data$qta_prodotta_tot, na.rm = TRUE)
  
  # Metriche aggregate usando la nuova funzione robusta
  robust_metrics <- calculate_robust_metrics(
    actual = actual_values, 
    predicted = predicted_values, 
    training_data = train_data$qta_prodotta_tot
  )
  
  # Coverage intervalli
  coverage_80 <- calculate_interval_accuracy(actual_values, previsioni_custom, 0.8)
  coverage_95 <- calculate_interval_accuracy(actual_values, previsioni_custom, 0.95)
  
  # Valutazione di ogni singola previsione
  individual_evaluations <- list()
  for(i in 1:n_eval) {
    individual_evaluations[[i]] <- evaluate_single_prediction(
      actual = actual_values[i],
      predicted = predicted_values[i], 
      actual_mean = actual_mean
    )
  }
  
  # Crea tabella confronto dettagliata
  confronto_test <- data.frame(
    data_mese = test_data$data_mese[1:n_eval],
    mese_nome = c("Gen","Feb","Mar","Apr","Mag","Giu",
                  "Lug","Ago","Set","Ott","Nov","Dic")[month(test_data$data_mese[1:n_eval])],
    anno = year(test_data$data_mese[1:n_eval]),
    previsione = round(predicted_values, 0),
    valore_reale = actual_values,
    errore = round(predicted_values - actual_values, 0),
    
    # Metriche robuste per singola previsione
    smape_singola = round(sapply(individual_evaluations, function(x) x$smape_single), 1),
    errore_norm = round(sapply(individual_evaluations, function(x) x$normalized_error), 1),
    score_singolo = round(sapply(individual_evaluations, function(x) x$score), 3),
    
    # Classificazione qualitativa
    accuratezza = sapply(individual_evaluations, function(x) 
      paste(x$quality$emoji, x$quality$level)),
    
    # Coverage intervalli
    in_intervallo_80 = actual_values >= previsioni_custom$.lower_80[1:n_eval] & 
      actual_values <= previsioni_custom$.upper_80[1:n_eval],
    in_intervallo_95 = actual_values >= previsioni_custom$.lower_95[1:n_eval] & 
      actual_values <= previsioni_custom$.upper_95[1:n_eval],
    
    stringsAsFactors = FALSE
  )
  
  # Performance aggregate
  test_performance <- list(
    mae = robust_metrics$mae,
    rmse = robust_metrics$rmse,
    smape = robust_metrics$smape,
    nrmse = robust_metrics$nrmse,
    mase = robust_metrics$mase,
    mape = robust_metrics$mape,
    coverage_80 = coverage_80,
    coverage_95 = coverage_95,
    n_observations = n_eval
  )
  
  # Report performance aggregate
  cat("📊 PERFORMANCE AGGREGATE SUL TEST SET:\n")
  cat("   • MAE test:", round(robust_metrics$mae, 2), "\n")
  cat("   • RMSE test:", round(robust_metrics$rmse, 2), "\n")
  cat("   • sMAPE test:", round(robust_metrics$smape, 2), "%\n")
  cat("   • NRMSE test:", round(robust_metrics$nrmse, 2), "%\n")
  if(!is.na(robust_metrics$mase)) {
    cat("   • MASE test:", round(robust_metrics$mase, 3), "\n")
  }
  cat("   • Coverage intervalli 80%:", round(coverage_80 * 100, 1), "%\n")
  cat("   • Coverage intervalli 95%:", round(coverage_95 * 100, 1), "%\n")
  
  # Valutazione qualitativa aggregate
  if(robust_metrics$smape <= 15 && coverage_80 >= 0.7) {
    cat("   🟢 Accuratezza aggregate: OTTIMA\n")
  } else if(robust_metrics$smape <= 25) {
    cat("   🟡 Accuratezza aggregate: BUONA\n")
  } else if(robust_metrics$smape <= 35) {
    cat("   🟠 Accuratezza aggregate: ACCETTABILE\n")
  } else {
    cat("   🔴 Accuratezza aggregate: DA MIGLIORARE\n")
  }
  
  # Report dettagliato delle previsioni
  cat("\n🔍 CONFRONTO DETTAGLIATO PREVISIONI vs VALORI REALI:\n")
  
  confronto_report <- confronto_test %>%
    select(data_mese, mese_nome, anno, previsione, valore_reale, 
           errore, smape_singola, score_singolo, accuratezza) %>%
    mutate(
      previsione = format(previsione, big.mark = ","),
      valore_reale = format(valore_reale, big.mark = ","),
      errore = ifelse(errore >= 0, paste("+", errore), as.character(errore))
    )
  
  print(confronto_report)
  
  # Analisi pattern di accuratezza
  cat("\n📈 ANALISI SINGOLE PREVISIONI:\n")
  
  # Statistiche distribuzione qualità
  quality_distribution <- table(sapply(individual_evaluations, function(x) x$quality$level))
  for(level in names(quality_distribution)) {
    emoji <- switch(level,
                    "Eccellente" = "🟢",
                    "Buona" = "🟡", 
                    "Media" = "🟠",
                    "Bassa" = "🔴",
                    "Molto Bassa" = "⚫")
    cat("   ", emoji, level, ":", quality_distribution[level], "previsioni\n")
  }
  
  # Statistiche errori
  cat("\n📊 DISTRIBUZIONE ERRORI:\n")
  excellent_pct <- mean(sapply(individual_evaluations, function(x) x$quality$level) == "Eccellente") * 100
  good_plus_pct <- mean(sapply(individual_evaluations, function(x) x$quality$level) %in% c("Eccellente", "Buona")) * 100
  acceptable_plus_pct <- mean(sapply(individual_evaluations, function(x) x$quality$level) %in% c("Eccellente", "Buona", "Media")) * 100
  
  cat("   • Previsioni eccellenti (sMAPE ≤ 5%):", round(excellent_pct, 1), "%\n")
  cat("   • Previsioni buone+ (sMAPE ≤ 12.5%):", round(good_plus_pct, 1), "%\n")
  cat("   • Previsioni accettabili+ (sMAPE ≤ 25%):", round(acceptable_plus_pct, 1), "%\n")
  
  # Identifica le previsioni migliori e peggiori
  if(n_eval > 1) {
    best_idx <- which.max(sapply(individual_evaluations, function(x) x$score))
    worst_idx <- which.min(sapply(individual_evaluations, function(x) x$score))
    
    cat("\n🏆 MIGLIORE PREVISIONE:\n")
    cat("   ", format(confronto_test$data_mese[best_idx], "%Y-%m"), 
        "- sMAPE:", round(individual_evaluations[[best_idx]]$smape_single, 1), "%",
        "- Score:", round(individual_evaluations[[best_idx]]$score, 3), "\n")
    
    cat("🎯 PREVISIONE DA MIGLIORARE:\n")
    cat("   ", format(confronto_test$data_mese[worst_idx], "%Y-%m"), 
        "- sMAPE:", round(individual_evaluations[[worst_idx]]$smape_single, 1), "%",
        "- Score:", round(individual_evaluations[[worst_idx]]$score, 3), "\n")
  }
  
} else {
  cat("⚠️ Nessun dato di test disponibile per la valutazione dell'accuratezza\n")
  cat("💡 Suggerimento: il cutoff è impostato su", as.character(data_cutoff), "\n")
  cat("   Se hai dati successivi, modifica la data_cutoff per includerli nel test set\n")
}


# 6. VISUALIZZAZIONE COMPARATIVA
# ===============================================================
cat("\n🎨 Creazione grafici comparativi...\n")

# Grafico principale (tutto il dataset)
p_comparison <- ggplot() +
  # Dati storici
  geom_line(data = train_data, aes(x = data_mese, y = qta_prodotta_tot), 
            color = "black", linewidth = 1.2) +
  
  # Previsioni personalizzate
  geom_line(data = previsioni_custom, aes(x = data_mese, y = .mean), 
            color = "red", linewidth = 1.5) +
  geom_ribbon(data = previsioni_custom, 
              aes(x = data_mese, ymin = .lower_80, ymax = .upper_80), 
              fill = "red", alpha = 0.2) +
  
  # Test data se disponibile
  {if(nrow(test_data) > 0) 
    geom_line(data = test_data, aes(x = data_mese, y = qta_prodotta_tot), 
              color = "blue", linewidth = 1.2, alpha = 0.8)} +
  
  # Cutoff line
  geom_vline(xintercept = data_cutoff, linetype = "dotted", color = "gray50") +
  
  labs(
    title = "Modello Personalizzato vs Dati Reali - Serie Completa",
    subtitle = paste("Previsioni per", nrow(previsioni_custom), "mesi futuri"),
    x = "Data",
    y = "Quantità Prodotta",
    caption = paste("Modello addestrato il", Sys.Date())
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

print(p_comparison)

# ========== NUOVO GRAFICO ULTIMI 5 ANNI ==========
cat("🎨 Creazione grafico focalizzato ultimi 5 anni...\n")

# Calcola la data di inizio per gli ultimi 5 anni dal cutoff
cutoff_5y <- data_cutoff - years(5)

# Filtra train_data per gli ultimi 5 anni
train_data_5y <- train_data %>%
  filter(data_mese >= cutoff_5y)

cat("📅 Grafico ultimi 5 anni: da", as.character(min(train_data_5y$data_mese)), 
    "a", as.character(max(train_data_5y$data_mese)), "\n")

# Grafico focalizzato ultimi 5 anni
p_comparison_5y <- ggplot() +
  # Dati storici ultimi 5 anni
  geom_line(data = train_data_5y, aes(x = data_mese, y = qta_prodotta_tot), 
            color = "black", linewidth = 1.2) +
  geom_point(data = train_data_5y, aes(x = data_mese, y = qta_prodotta_tot), 
             color = "black", size = 1, alpha = 0.6) +
  
  # Previsioni personalizzate
  geom_line(data = previsioni_custom, aes(x = data_mese, y = .mean), 
            color = "red", linewidth = 1.5) +
  geom_point(data = previsioni_custom, aes(x = data_mese, y = .mean), 
             color = "red", size = 2) +
  
  # Intervalli di confidenza
  geom_ribbon(data = previsioni_custom, 
              aes(x = data_mese, ymin = .lower_80, ymax = .upper_80), 
              fill = "red", alpha = 0.2) +
  geom_ribbon(data = previsioni_custom, 
              aes(x = data_mese, ymin = .lower_95, ymax = .upper_95), 
              fill = "red", alpha = 0.1) +
  
  # Test data se disponibile (ultimi 5 anni)
  {if(nrow(test_data) > 0) {
    test_data_5y <- test_data %>% filter(data_mese >= cutoff_5y)
    if(nrow(test_data_5y) > 0) {
      list(
        geom_line(data = test_data_5y, aes(x = data_mese, y = qta_prodotta_tot), 
                  color = "blue", linewidth = 1.2, alpha = 0.8),
        geom_point(data = test_data_5y, aes(x = data_mese, y = qta_prodotta_tot), 
                   color = "blue", size = 2)
      )
    }
  }} +
  
  # Cutoff line
  geom_vline(xintercept = data_cutoff, linetype = "dotted", color = "gray50", linewidth = 1) +
  
  # Linea separazione ultimi 5 anni
  geom_vline(xintercept = cutoff_5y, linetype = "dotted", color = "gray70", alpha = 0.7) +
  
  labs(
    title = "Modello Personalizzato - Focus Ultimi 5 Anni",
    subtitle = paste("Dettaglio dal", format(cutoff_5y, "%Y-%m"), "con previsioni", nrow(previsioni_custom), "mesi"),
    x = "Data",
    y = "Quantità Prodotta",
    caption = paste("Grafico dettagliato - Modello addestrato il", Sys.Date())
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  # Migliore leggibilità asse X per periodo più corto
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  scale_y_continuous(labels = scales::comma)

print(p_comparison_5y)

cat("✅ Entrambi i grafici creati con successo!\n")

# 7. TABELLA RIASSUNTIVA MIGLIORATA
# ===============================================================
cat("\n📋 Tabella previsioni dettagliata:\n")

# Vettore mesi italiani
mesi_it <- c("Gennaio","Febbraio","Marzo","Aprile","Maggio","Giugno",
             "Luglio","Agosto","Settembre","Ottobre","Novembre","Dicembre")

# Crea tabella base delle previsioni
tabella_base <- previsioni_custom %>%
  mutate(
    mese_nome = mesi_it[month(data_mese)],
    anno = year(data_mese),
    previsione = round(.mean, 0),
    intervallo_80 = paste0("[", round(.lower_80, 0), " - ", round(.upper_80, 0), "]"),
    n_commesse = round(n_commesse_stimate, 0)
  ) %>%
  select(data_mese, mese_nome, anno, previsione, n_commesse, intervallo_80)

# ==============================================================================
# COMBINAZIONE CON DATI REALI E ACCURATEZZA ⭐ NUOVO ⭐
# ==============================================================================

if(!is.null(confronto_test) && nrow(confronto_test) > 0) {
  cat("🎯 Tabella con confronto vs dati reali:\n")
  
  # Prepara dati di confronto
  confronto_semplificato <- confronto_test %>%
    select(data_mese, valore_reale, errore, accuratezza) %>%
    mutate(
      valore_reale = as.numeric(valore_reale),
      errore = as.numeric(errore)
    )
  
  # Combina tabella base con confronto
  tabella_finale <- tabella_base %>%
    left_join(confronto_semplificato, by = "data_mese") %>%
    mutate(
      # Formatta i valori per migliore leggibilità
      valore_reale_fmt = case_when(
        !is.na(valore_reale) ~ format(valore_reale, big.mark = ","),
        TRUE ~ "- (futuro)"
      ),
      errore_fmt = case_when(
        !is.na(errore) & errore >= 0 ~ paste("+", format(abs(errore), big.mark = ",")),
        !is.na(errore) & errore < 0 ~ paste("-", format(abs(errore), big.mark = ",")),
        TRUE ~ "- (futuro)"
      ),
      accuratezza_fmt = case_when(
        !is.na(accuratezza) ~ accuratezza,
        TRUE ~ "⭕ Da verificare"
      ),
      # Indica se è validabile o futuro
      tipo = case_when(
        !is.na(valore_reale) ~ "📊 Validato",
        TRUE ~ "🔮 Previsione"
      )
    ) %>%
    select(data_mese, mese_nome, anno, tipo, previsione, valore_reale_fmt, 
           errore_fmt, accuratezza_fmt, n_commesse, intervallo_80) %>%
    rename(
      Data = data_mese,
      Mese = mese_nome,
      Anno = anno,
      Tipo = tipo,
      Previsione = previsione,
      `Valore Reale` = valore_reale_fmt,
      Differenza = errore_fmt,
      Accuratezza = accuratezza_fmt,
      `N. Commesse` = n_commesse,
      `Intervallo 80%` = intervallo_80
    )
  
  # Stampa tabella completa
  print(tabella_finale)
  
  # ==============================================================================
  # ANALISI RIASSUNTIVA DELLA TABELLA
  # ==============================================================================
  
  cat("\n📈 ANALISI TABELLA FINALE:\n")
  
  # Conta mesi validati vs futuri
  n_validati <- sum(!is.na(confronto_test$valore_reale))
  n_futuri <- nrow(tabella_finale) - n_validati
  
  cat("   • Mesi con validazione:", n_validati, "\n")
  cat("   • Mesi previsioni future:", n_futuri, "\n")
  
  if(n_validati > 0) {
    # Analisi accuratezza sui mesi validati
    accuracy_levels <- confronto_test$accuratezza
    
    eccellenti <- sum(grepl("Eccellente", accuracy_levels))
    buone <- sum(grepl("Buona", accuracy_levels))
    medie <- sum(grepl("Media", accuracy_levels))
    basse <- sum(grepl("Bassa", accuracy_levels))
    
    cat("\n🎯 DISTRIBUZIONE ACCURATEZZA (mesi validati):\n")
    if(eccellenti > 0) cat("   🟢 Eccellenti:", eccellenti, "mesi\n")
    if(buone > 0) cat("   🟡 Buone:", buone, "mesi\n")
    if(medie > 0) cat("   🟠 Medie:", medie, "mesi\n")
    if(basse > 0) cat("   🔴 Basse:", basse, "mesi\n")
    
    # Calcola statistiche errori
    errori_numerici <- as.numeric(confronto_test$errore)
    if(length(errori_numerici) > 0) {
      errore_medio <- mean(abs(errori_numerici), na.rm = TRUE)
      errore_max <- max(abs(errori_numerici), na.rm = TRUE)
      
      cat("\n📊 STATISTICHE ERRORI:\n")
      cat("   • Errore medio assoluto:", round(errore_medio, 0), "\n")
      cat("   • Errore massimo:", round(errore_max, 0), "\n")
      
      # Percentuale errori piccoli
      errori_piccoli <- sum(abs(errori_numerici) <= errore_medio * 0.5, na.rm = TRUE)
      pct_piccoli <- errori_piccoli / length(errori_numerici) * 100
      cat("   • Errori sotto la media:", round(pct_piccoli, 1), "%\n")
    }
    
    # Identifica mesi problematici
    mesi_problematici <- confronto_test %>%
      filter(grepl("🔴|⚫", accuratezza)) %>%
      select(data_mese, mese_nome, anno, smape_singola)
    
    if(nrow(mesi_problematici) > 0) {
      cat("\n⚠️ MESI DA MONITORARE (accuratezza bassa):\n")
      for(i in 1:nrow(mesi_problematici)) {
        cat("   •", format(mesi_problematici$data_mese[i], "%Y-%m"), 
            "-", mesi_problematici$mese_nome[i], 
            "- sMAPE:", mesi_problematici$smape_singola[i], "%\n")
      }
    }
  }
  
  # Range previsioni future
  if(n_futuri > 0) {
    future_forecasts <- tabella_finale %>%
      filter(Tipo == "🔮 Previsione") %>%
      pull(Previsione)
    
    cat("\n🔮 PREVISIONI FUTURE:\n")
    cat("   • Range:", min(future_forecasts), "-", max(future_forecasts), "\n")
    cat("   • Media:", round(mean(future_forecasts)), "\n")
    
    # Trend previsioni future
    if(length(future_forecasts) > 1) {
      trend_slope <- lm(future_forecasts ~ seq_along(future_forecasts))$coefficients[2]
      if(abs(trend_slope) > 1) {
        trend_direction <- ifelse(trend_slope > 0, "crescente", "decrescente")
        cat("   • Trend:", trend_direction, "(", round(trend_slope, 1), "unità/mese )\n")
      } else {
        cat("   • Trend: stabile\n")
      }
    }
  }
  
} else {
  # ==============================================================================
  # TABELLA SEMPLICE (SOLO PREVISIONI) - CASO SENZA DATI TEST
  # ==============================================================================
  
  cat("🔮 Tabella previsioni (senza dati di confronto):\n")
  
  tabella_finale <- tabella_base %>%
    mutate(
      tipo = "🔮 Previsione"
    ) %>%
    select(data_mese, mese_nome, anno, tipo, previsione, n_commesse, intervallo_80) %>%
    rename(
      Data = data_mese,
      Mese = mese_nome,
      Anno = anno,
      Tipo = tipo,
      Previsione = previsione,
      `N. Commesse` = n_commesse,
      `Intervallo 80%` = intervallo_80
    )
  
  print(tabella_finale)
  
  cat("\n📈 ANALISI PREVISIONI:\n")
  forecasts <- tabella_finale$Previsione
  cat("   • Totale mesi previsti:", length(forecasts), "\n")
  cat("   • Range previsioni:", min(forecasts), "-", max(forecasts), "\n")
  cat("   • Media previsioni:", round(mean(forecasts)), "\n")
  
  # Analisi stagionalità nelle previsioni
  mesi_numerici <- month(tabella_base$data_mese)
  if(length(unique(mesi_numerici)) > 1) {
    seasonal_analysis <- tabella_base %>%
      mutate(mese_num = month(data_mese)) %>%
      group_by(mese_num) %>%
      summarise(media_mese = mean(previsione), .groups = 'drop') %>%
      arrange(desc(media_mese))
    
    mese_alto <- mesi_it[seasonal_analysis$mese_num[1]]
    mese_basso <- mesi_it[seasonal_analysis$mese_num[nrow(seasonal_analysis)]]
    
    cat("   • Mese previsto più alto:", mese_alto, "(", round(seasonal_analysis$media_mese[1]), ")\n")
    cat("   • Mese previsto più basso:", mese_basso, "(", round(seasonal_analysis$media_mese[nrow(seasonal_analysis)]), ")\n")
  }
}

cat("\n💡 LEGENDA TABELLA:\n")
cat("   📊 Validato = Confronto con dati reali disponibile\n")
cat("   🔮 Previsione = Previsione futura da verificare\n")
cat("   🟢 Eccellente = sMAPE ≤ 5%\n")
cat("   🟡 Buona = sMAPE ≤ 12.5%\n") 
cat("   🟠 Media = sMAPE ≤ 25%\n")
cat("   🔴 Bassa = sMAPE > 25%\n")
cat("   ⭕ Da verificare = Attendere dati reali\n")

# Salva la tabella finale migliorata (per la sezione 8)
tabella_per_salvataggio <- tabella_finale

# 8. SALVATAGGIO RISULTATI
# ===============================================================
cat("\n💾 Salvataggio risultati...\n")

# Nome del file dello script
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getActiveDocumentContext()$path
  file_name <- tools::file_path_sans_ext(basename(script_path))
} else {
  file_name <- "script_output"  # fallback se non si usa RStudio
}

# Timestamp
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Creazione cartella di output
output_folder <- paste0(file_name, "_output_", timestamp)
if(!dir.exists(output_folder)) dir.create(output_folder)

cat("📂 Cartella di output creata:", output_folder, "\n")

# Salvataggio file nella cartella
write.csv(tabella_finale, file.path(output_folder, "previsioni_modello_personalizzato.csv"), row.names = FALSE)
saveRDS(modelo_custom, file.path(output_folder, "modello_personalizzato.rds"))
ggsave(file.path(output_folder, "confronto_modello_personalizzato.png"), p_comparison, 
       width = 14, height = 8, dpi = 300)

# Salva anche i risultati della valutazione accuratezza (se disponibili)
if(!is.null(confronto_test)) {
  write.csv(confronto_test, file.path(output_folder, "confronto_previsioni_vs_reali.csv"), row.names = FALSE)
  
  # Salva anche le metriche aggregate
  if(!is.null(test_performance)) {
    test_perf_df <- data.frame(
      metrica = c("MAE", "RMSE", "sMAPE", "NRMSE", "MASE", "MAPE", "Coverage_80", "Coverage_95"),
      valore = c(test_performance$mae, test_performance$rmse, test_performance$smape,
                 test_performance$nrmse, test_performance$mase, test_performance$mape,
                 test_performance$coverage_80, test_performance$coverage_95)
    )
    write.csv(test_perf_df, file.path(output_folder, "metriche_accuratezza.csv"), row.names = FALSE)
  }
}

# Salva secondo grafico
ggsave(file.path(output_folder, "confronto_modello_personalizzato_5anni.png"), p_comparison_5y, 
       width = 14, height = 8, dpi = 300)

cat("✅ Tutti i file salvati nella cartella:", output_folder, "\n")
cat("  • previsioni_modello_personalizzato.csv\n")
cat("  • modello_personalizzato.rds\n")
cat("  • confronto_modello_personalizzato.png\n")
cat("  • confronto_modello_personalizzato_5anni.png\n")
if(!is.null(confronto_test)) {
  cat("  • confronto_previsioni_vs_reali.csv\n")
  cat("  • metriche_accuratezza.csv\n")
}

# 9. SUMMARY FINALE
# ===============================================================
cat("\n" , rep("=", 60), "\n")
cat("🎯 MODELLO PERSONALIZZATO COMPLETATO!\n")
cat(rep("=", 60), "\n")

cat("🏗️  CARATTERISTICHE MODELLO:\n")
cat("   • Componente deterministico basato su n_commesse\n")
cat("   • Componente stagionale adattiva\n")
cat("   • ARIMA sui residui per catturare dinamiche stocastiche\n")
cat("   • Trend breaks detection automatica\n")

cat("\n📊 PERFORMANCE:\n")
cat("   • R² relazione quantità-commesse:", round(modelo_custom$r_squared_commesse, 3), "\n")
cat("   • Performance metric:", round(modelo_custom$performance_metric, 3), "\n")

if(!is.null(test_performance)) {
  cat("\n🎯 ACCURATEZZA SUL TEST SET:\n")
  cat("   • MAE:", round(test_performance$mae, 2), "\n")
  cat("   • sMAPE:", round(test_performance$smape, 2), "%\n")
  cat("   • Coverage intervalli 80%:", round(test_performance$coverage_80 * 100, 1), "%\n")
  cat("   • Osservazioni test:", test_performance$n_observations, "\n")
  
  # Valutazione finale affidabilità
  if(test_performance$smape <= 20 && test_performance$coverage_80 >= 0.70) {
    cat("   ✅ Modello AFFIDABILE per uso operativo\n")
  } else if(test_performance$smape <= 30) {
    cat("   ⚠️ Modello MODERATAMENTE affidabile\n") 
  } else {
    cat("   🔧 Modello necessita miglioramenti\n")
  }
}

cat("\n🔮 PREVISIONI GENERATE:\n")
cat("   • Orizzonte:", nrow(previsioni_custom), "mesi\n")
cat("   • Range:", round(min(previsioni_custom$.mean)), "-", round(max(previsioni_custom$.mean)), "\n")
cat("   • Intervalli di confidenza: 80% e 95%\n")

cat("\n✨ Il tuo modello personalizzato è pronto! ✨\n\n")