# ================================================================
# ANALISI TEMPORALE AVANZATA - GRANULARITÀ GIORNALIERA
# ================================================================
# Estrazione di informazioni temporali precise dai codici commessa
# ancora da sistemare, non funziona la modalita giornaliera

library(readxl)
library(dplyr)
library(lubridate)
library(janitor)
library(ggplot2)
library(fpp3)

# 1. FUNZIONE PER DECODIFICARE COMMESSE
# ================================================================

decode_commessa <- function(commessa_code) {
  # Converte codice commessa in informazioni temporali
  # Formato: [TIPO][ANNO][MESE][PROGRESSIVO]
  
  if(is.na(commessa_code) || nchar(as.character(commessa_code)) != 11) {
    return(list(
      tipo = NA,
      anno = NA, 
      mese = NA,
      progressivo = NA,
      data_stimata = as.Date(NA)
    ))
  }
  
  code_str <- as.character(commessa_code)
  
  # Estrazione componenti con controllo errori
  tryCatch({
    tipo <- substr(code_str, 1, 1)
    anno <- 2000 + as.numeric(substr(code_str, 2, 3))
    mese <- as.numeric(substr(code_str, 4, 5))
    progressivo <- as.numeric(substr(code_str, 6, 11))
    
    # Validazioni base
    if(is.na(anno) || is.na(mese) || is.na(progressivo)) {
      return(list(tipo = tipo, anno = anno, mese = mese, progressivo = progressivo, data_stimata = as.Date(NA)))
    }
    
    if(anno < 2020 || anno > 2030 || mese < 1 || mese > 12) {
      return(list(tipo = tipo, anno = anno, mese = mese, progressivo = progressivo, data_stimata = as.Date(NA)))
    }
    
    # Stima data con gestione errori
    data_stimata <- tryCatch({
      # Ipotesi 1: Progressivo = giorno giuliano (1-365)
      if(!is.na(progressivo) && progressivo >= 1 && progressivo <= 366) {
        data_base <- as.Date(paste(anno, "01", "01", sep = "-"))
        data_candidate <- data_base + days(progressivo - 1)
        
        # Verifica che la data sia valida
        if(!is.na(data_candidate) && year(data_candidate) == anno) {
          return(data_candidate)
        }
      }
      
      # Ipotesi 2: Progressivo = ordine nel mese, usa metà mese
      data_fallback <- as.Date(paste(anno, sprintf("%02d", mese), "15", sep = "-"))
      return(data_fallback)
      
    }, error = function(e) {
      # Fallback finale: primo del mese
      return(as.Date(paste(anno, sprintf("%02d", mese), "01", sep = "-")))
    })
    
    return(list(
      tipo = tipo,
      anno = anno,
      mese = mese, 
      progressivo = progressivo,
      data_stimata = data_stimata
    ))
    
  }, error = function(e) {
    return(list(
      tipo = NA,
      anno = NA,
      mese = NA,
      progressivo = NA,
      data_stimata = as.Date(NA)
    ))
  })
}

# 2. CARICAMENTO E DECODIFICA DATI
# ================================================================

cat("📂 Caricamento dati con decodifica commesse...\n")

# Carica file
file_path <- file.choose()  # O specificare path diretto
dati_raw <- read_excel(file_path, na = c("-", "NA", "")) %>%
  clean_names()

# Decodifica commesse con gestione errori migliorata
cat("🔓 Decodifica codici commessa...\n")

# Prima, vediamo quanti record abbiamo e la struttura
cat("Record totali:", nrow(dati_raw), "\n")
cat("Colonne principali:", paste(names(dati_raw)[1:min(5, ncol(dati_raw))], collapse = ", "), "\n")

# Decodifica con gestione errori e progress tracking
dati_decoded <- tryCatch({
  
  # Processa in lotti per debugging
  batch_size <- 1000
  n_batches <- ceiling(nrow(dati_raw) / batch_size)
  results_list <- list()
  
  for(i in 1:n_batches) {
    start_row <- (i-1) * batch_size + 1
    end_row <- min(i * batch_size, nrow(dati_raw))
    
    cat("Processando batch", i, "di", n_batches, "(righe", start_row, "-", end_row, ")\n")
    
    batch_data <- dati_raw[start_row:end_row, ]
    
    batch_result <- batch_data %>%
      rowwise() %>%
      mutate(
        # Decodifica sicura
        decoded_info = tryCatch({
          decode_commessa(commessa)
        }, error = function(e) {
          list(tipo = NA, anno = NA, mese = NA, progressivo = NA, data_stimata = as.Date(NA))
        }),
        
        # Estrazione componenti
        commessa_tipo = tryCatch(decoded_info$tipo, error = function(e) NA),
        commessa_anno = tryCatch(decoded_info$anno, error = function(e) NA),
        commessa_mese = tryCatch(decoded_info$mese, error = function(e) NA),
        commessa_progressivo = tryCatch(decoded_info$progressivo, error = function(e) NA),
        data_stimata = tryCatch(decoded_info$data_stimata, error = function(e) as.Date(NA)),
        
        # Variabili originali pulite
        qta_prodotta = tryCatch(as.numeric(qta_prodotta), error = function(e) NA),
        
        # Data dal file originale
        mese_num = match(tolower(mese), c("gen", "feb", "mar", "apr", "mag", "giu", 
                                          "lug", "ago", "set", "ott", "nov", "dic")),
        data_originale = tryCatch({
          if(!is.na(mese_num) && !is.na(anno)) {
            as.Date(paste(anno, sprintf("%02d", mese_num), "01", sep = "-"))
          } else {
            as.Date(NA)
          }
        }, error = function(e) as.Date(NA))
      ) %>%
      select(-decoded_info) %>%
      ungroup()
    
    results_list[[i]] <- batch_result
  }
  
  # Combina tutti i risultati
  combined_result <- bind_rows(results_list)
  
  # Filtra record validi
  combined_result %>%
    filter(!is.na(qta_prodotta), qta_prodotta > 0)
  
}, error = function(e) {
  cat("ERRORE nella decodifica completa:", e$message, "\n")
  cat("Provo approccio semplificato...\n")
  
  # Fallback: decodifica semplice senza rowwise
  dati_raw %>%
    mutate(
      qta_prodotta = as.numeric(qta_prodotta),
      mese_num = match(tolower(mese), c("gen", "feb", "mar", "apr", "mag", "giu", 
                                        "lug", "ago", "set", "ott", "nov", "dic")),
      data_originale = as.Date(paste(anno, sprintf("%02d", mese_num), "01", sep = "-")),
      
      # Decodifica semplificata commessa
      commessa_str = as.character(commessa),
      commessa_valida = nchar(commessa_str) == 11,
      commessa_tipo = ifelse(commessa_valida, substr(commessa_str, 1, 1), NA),
      data_stimata = data_originale  # Usa data originale come fallback
    ) %>%
    filter(!is.na(qta_prodotta), qta_prodotta > 0)
})

cat("✓ Decodifica completata:", nrow(dati_decoded), "record processati\n")

# 3. ANALISI COERENZA TEMPORALE
# ================================================================

cat("\n=== ANALISI COERENZA TEMPORALE ===\n")

# Confronta date originali vs date stimate
coerenza_check <- dati_decoded %>%
  filter(!is.na(data_originale), !is.na(data_stimata)) %>%
  mutate(
    diff_giorni = as.numeric(data_stimata - data_originale),
    stesso_mese = month(data_originale) == month(data_stimata),
    stesso_anno = year(data_originale) == year(data_stimata)
  )

cat("Controlli coerenza:\n")
cat("- Record con stessa anno:", sum(coerenza_check$stesso_anno, na.rm = TRUE), "/", nrow(coerenza_check), "\n")
cat("- Record con stesso mese:", sum(coerenza_check$stesso_mese, na.rm = TRUE), "/", nrow(coerenza_check), "\n")
cat("- Differenza media giorni:", round(mean(coerenza_check$diff_giorni, na.rm = TRUE), 1), "\n")

# CORREZIONE - Verifica e ridecodifica se necessario
cat("Verifico struttura dati decodificati...\n")
print(names(dati_decoded))

# Se mancano le colonne dei progressivi, ridecodifica
if(!"commessa_progressivo" %in% names(dati_decoded)) {
  cat("Colonne progressivi mancanti, ridecodifico...\n")
  
  dati_decoded <- dati_decoded %>%
    mutate(
      commessa_str = as.character(commessa),
      commessa_valida = !is.na(commessa_str) & nchar(commessa_str) == 11,
      
      # Estrai componenti manualmente
      commessa_tipo = ifelse(commessa_valida, substr(commessa_str, 1, 1), NA),
      commessa_anno_code = ifelse(commessa_valida, as.numeric(substr(commessa_str, 2, 3)), NA),
      commessa_mese_code = ifelse(commessa_valida, as.numeric(substr(commessa_str, 4, 5)), NA),
      commessa_progressivo = ifelse(commessa_valida, as.numeric(substr(commessa_str, 6, 11)), NA),
      
      # Ricalcola data stimata con progressivo
      data_stimata = case_when(
        !commessa_valida ~ data_originale,
        is.na(commessa_progressivo) ~ data_originale,
        commessa_progressivo >= 1 & commessa_progressivo <= 366 ~ {
          data_base <- as.Date(paste(2000 + commessa_anno_code, "01", "01", sep = "-"))
          data_base + days(commessa_progressivo - 1)
        },
        TRUE ~ data_originale
      )
    )
  
  cat("✓ Ridecodifica completata\n")
}

# Ora continua con l'analisi progressivi (versione corretta)
progressivi_analysis <- dati_decoded %>%
  filter(!is.na(commessa_progressivo), !is.na(data_originale)) %>%
  group_by(anno, mese) %>%
  summarise(
    n_commesse = n(),
    progressivo_min = min(commessa_progressivo, na.rm = TRUE),
    progressivo_max = max(commessa_progressivo, na.rm = TRUE),
    progressivo_range = progressivo_max - progressivo_min,
    possibili_giorni = sum(commessa_progressivo >= 1 & commessa_progressivo <= 31),
    possibili_giorni_anno = sum(commessa_progressivo >= 1 & commessa_progressivo <= 366),
    .groups = "drop"
  )

print("Analisi progressivi per mese:")
print(head(progressivi_analysis, 10))

# Verifica campione
progressivi_sample <- dati_decoded %>%
  filter(!is.na(commessa_progressivo)) %>%
  select(commessa, commessa_progressivo, data_originale, data_stimata) %>%
  head(20)

print("Campione progressivi:")
print(progressivi_sample)

# 4. CREAZIONE SERIE TEMPORALI GIORNALIERE
# ================================================================

cat("\n📈 Creazione serie temporali giornaliere...\n")

# Ipotesi: se i progressivi sono ragionevolmente distribuiti come giorni
use_daily_granularity <- mean(progressivi_analysis$possibili_giorni_anno / progressivi_analysis$n_commesse, na.rm = TRUE) > 0.8

if(use_daily_granularity) {
  cat("✓ Usando granularità giornaliera basata sui progressivi\n")
  
  # Serie giornaliera
  serie_giornaliera <- dati_decoded %>%
    filter(!is.na(data_stimata), !is.na(qta_prodotta)) %>%
    group_by(data_stimata, commessa_tipo) %>%
    summarise(
      qta_prodotta_tot = sum(qta_prodotta, na.rm = TRUE),
      n_commesse = n(),
      .groups = "drop"
    ) %>%
    # Completa serie temporale
    complete(data_stimata = seq.Date(min(data_stimata), max(data_stimata), by = "day"),
             commessa_tipo,
             fill = list(qta_prodotta_tot = 0, n_commesse = 0)) %>%
    arrange(data_stimata)
  
  cat("✓ Serie giornaliera creata:", nrow(serie_giornaliera), "osservazioni\n")
  
  # Aggregazione settimanale per modelling
  serie_settimanale <- serie_giornaliera %>%
    mutate(settimana = floor_date(data_stimata, "week")) %>%
    group_by(settimana, commessa_tipo) %>%
    summarise(
      qta_prodotta_tot = sum(qta_prodotta_tot, na.rm = TRUE),
      n_commesse = sum(n_commesse, na.rm = TRUE),
      giorni_attivi = sum(qta_prodotta_tot > 0),
      .groups = "drop"
    )
  
  working_data <- serie_settimanale %>%
    group_by(settimana) %>%
    summarise(
      qta_prodotta_tot = sum(qta_prodotta_tot, na.rm = TRUE),
      n_commesse = sum(n_commesse, na.rm = TRUE),
      n_tipi = n_distinct(commessa_tipo[qta_prodotta_tot > 0]),
      .groups = "drop"
    ) %>%
    filter(qta_prodotta_tot > 0) %>%
    as_tsibble(index = settimana)
  
  granularity <- "settimanale"
  
} else {
  cat("⚠ Progressivi non sembrano giorni, uso granularità mensile migliorata\n")
  
  # Serie mensile migliorata con info dai progressivi
  working_data <- dati_decoded %>%
    filter(!is.na(data_originale), !is.na(qta_prodotta)) %>%
    group_by(data_originale) %>%
    summarise(
      qta_prodotta_tot = sum(qta_prodotta, na.rm = TRUE),
      n_commesse = n(),
      
      # Info aggiuntive dai progressivi
      progressivo_medio = mean(commessa_progressivo, na.rm = TRUE),
      spread_temporale = max(commessa_progressivo, na.rm = TRUE) - min(commessa_progressivo, na.rm = TRUE),
      n_tipi = n_distinct(commessa_tipo),
      perc_tipo_A = mean(commessa_tipo == "A", na.rm = TRUE),
      perc_tipo_C = mean(commessa_tipo == "C", na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    arrange(data_originale) %>%
    as_tsibble(index = data_originale)
  
  granularity <- "mensile"
}

cat("✓ Dataset finale:", nrow(working_data), "osservazioni", granularity, "\n")

# 5. VISUALIZZAZIONE SERIE TEMPORALI
# ================================================================

cat("\n🎨 Creazione visualizzazioni...\n")

# Grafico principale
p_main <- working_data %>%
  autoplot(qta_prodotta_tot) +
  labs(
    title = paste("Serie Temporale", stringr::str_to_title(granularity), "- Quantità Prodotta"),
    subtitle = "Basata su decodifica codici commessa",
    x = ifelse(granularity == "settimanale", "Settimana", "Mese"),
    y = "Quantità Prodotta Totale"
  ) +
  theme_minimal()

print(p_main)

# Grafico distribuzione per tipo (se disponibile)
if("commessa_tipo" %in% names(working_data) || exists("serie_giornaliera")) {
  
  if(exists("serie_giornaliera")) {
    p_tipi <- serie_giornaliera %>%
      filter(qta_prodotta_tot > 0) %>%
      ggplot(aes(x = data_stimata, y = qta_prodotta_tot, color = commessa_tipo)) +
      geom_line(alpha = 0.7) +
      facet_wrap(~commessa_tipo, scales = "free_y") +
      labs(title = "Serie Temporali per Tipo Commessa",
           x = "Data", y = "Quantità Prodotta") +
      theme_minimal()
    
    print(p_tipi)
  }
}

# 6. MODELLAZIONE AVANZATA CON GRANULARITÀ MIGLIORATA
# ================================================================

cat("\n🤖 Modellazione con granularità", granularity, "...\n")

# Split temporale
n_total <- nrow(working_data)
n_train <- floor(n_total * 0.8)
n_test <- n_total - n_train

train_data <- working_data %>% slice_head(n = n_train)
test_data <- working_data %>% slice_tail(n = n_test)

cat("Training:", n_train, "osservazioni\n")
cat("Test:", n_test, "osservazioni\n")

# Modelli appropriati per la granularità
if(granularity == "settimanale") {
  modelli <- train_data %>%
    model(
      arima_auto = ARIMA(qta_prodotta_tot),
      ets_auto = ETS(qta_prodotta_tot),
      # Modelli settimanali specifici
      tbats = TBATS(qta_prodotta_tot),
      ensemble = (ARIMA(qta_prodotta_tot) + ETS(qta_prodotta_tot)) / 2
    )
  
  freq_info <- "frequency = 52 (settimanale)"
  
} else {
  modelli <- train_data %>%
    model(
      arima_auto = ARIMA(qta_prodotta_tot),
      ets_auto = ETS(qta_prodotta_tot),
      
      # Sfrutta info aggiuntive dai progressivi se disponibili
      arima_reg = ARIMA(qta_prodotta_tot ~ progressivo_medio + spread_temporale + n_tipi),
      
      ensemble = (ARIMA(qta_prodotta_tot) + ETS(qta_prodotta_tot)) / 2
    )
  
  freq_info <- "frequency = 12 (mensile)"
}

# Previsioni
fc <- modelli %>% forecast(h = n_test)

# Accuracy
if(n_test > 0) {
  acc_results <- accuracy(fc, test_data) %>%
    arrange(MAE) %>%
    select(.model, MAE, RMSE, MAPE) %>%
    mutate_if(is.numeric, round, 2)
  
  cat("\n📊 RISULTATI ACCURACY:\n")
  print(acc_results)
  
  best_model <- acc_results$.model[1]
  cat("\n🏆 MIGLIOR MODELLO:", best_model, "\n")
}

# Grafico finale
grafico_finale <- train_data %>%
  autoplot(qta_prodotta_tot, color = "black", size = 1) +
  autolayer(fc, alpha = 0.8) +
  {if(n_test > 0) autolayer(test_data, qta_prodotta_tot, color = "red", size = 1.2)} +
  labs(
    title = paste("Previsioni con Granularità", stringr::str_to_title(granularity)),
    subtitle = paste("Basate su decodifica commesse -", freq_info),
    x = "Tempo",
    y = "Quantità Prodotta",
    caption = "Nero: Training | Colori: Previsioni | Rosso: Test"
  ) +
  theme_minimal()

print(grafico_finale)

# 7. EXPORT RISULTATI
# ================================================================

cat("\n💾 Export risultati...\n")

# Salva dataset decodificato
write.csv(dati_decoded, "dataset_commesse_decodificate.csv", row.names = FALSE)

# Salva serie temporale finale
write.csv(working_data, paste0("serie_", granularity, ".csv"), row.names = FALSE)

# Salva previsioni
if(exists("fc")) {
  previsioni <- fc %>%
    as_tibble() %>%
    select(-any_of(c(".distribution"))) %>%
    write.csv(paste0("previsioni_", granularity, ".csv"), row.names = FALSE)
}

# Salva grafici
ggsave("serie_temporale_decodificata.png", p_main, width = 14, height = 8)
ggsave("previsioni_decodificate.png", grafico_finale, width = 16, height = 10)

cat("\n✅ ANALISI COMPLETATA!\n")
cat("==========================================\n")
cat("📁 FILE GENERATI:\n")
cat("- dataset_commesse_decodificate.csv\n")
cat("- serie_", granularity, ".csv\n") 
cat("- previsioni_", granularity, ".csv\n")
cat("- serie_temporale_decodificata.png\n")
cat("- previsioni_decodificate.png\n")

cat("\n🎯 VANTAGGI OTTENUTI:\n")
if(granularity == "settimanale") {
  cat("✓ Granularità SETTIMANALE invece che mensile\n")
  cat("✓ 4x più punti dati per il training\n")
  cat("✓ Cattura pattern intra-mese\n")
  cat("✓ Previsioni più precise e tempestive\n")
} else {
  cat("✓ Serie mensile ARRICCHITA con info temporali\n")
  cat("✓ Features aggiuntive dai codici commessa\n") 
  cat("✓ Migliore comprensione pattern produttivi\n")
  cat("✓ Possibilità di segmentazione per tipo\n")
}

cat("\n🚀 PROSSIMI PASSI:\n")
cat("1. Valida le ipotesi sui progressivi con il team\n")
cat("2. Se confermata granularità giornaliera, rerun con daily data\n") 
cat("3. Integra con altri sistemi aziendali per validazione\n")
cat("4. Usa per pianificazione operativa settimanale\n")