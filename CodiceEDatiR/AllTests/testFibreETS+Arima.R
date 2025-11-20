## Script R che importa un file Excel, trasforma i dati in una tsibble annuale per ‘fibre’, divide train (≤1980) e test (>1980), stima un modello 
## ETS additivo e quattro ARIMA con pdq specifici più un ARIMA automatico, seleziona il migliore via AIC/BIC, genera le previsioni ETS e del miglior 
## ARIMA, le confronta graficamente con i dati reali, calcola le metriche di accuratezza e stampa infine i report dettagliati dei modelli.

library(readxl)
library(fpp3)

# Scegli il file Excel manualmente
file <- file.choose()
dati <- read_excel(file)

# Prepara i dati: seleziona 'anno' e 'fibre'
dati_fibre <- dati %>%
  select(anno, fibre) %>%
  filter(!is.na(fibre)) %>%
  mutate(anno = as.numeric(anno), fibre = as.numeric(fibre)) %>%
  as_tsibble(index = anno)

# Suddivisione in train e test
train_fibre <- dati_fibre %>% filter(anno <= 1980)
test_fibre  <- dati_fibre %>% filter(anno > 1980)

# Modello ETS con trend additivo
mod_ets_fibre <- train_fibre %>%
  model(ETS(fibre ~ error("A") + trend("A") + season("N")))

# Diversi modelli ARIMA con parametri espliciti
mod_arima_fibre <- train_fibre %>%
  model(
    ARIMA_111   = ARIMA(fibre ~ pdq(1,1,1)),
    ARIMA_211   = ARIMA(fibre ~ pdq(2,1,1)),
    ARIMA_011   = ARIMA(fibre ~ pdq(0,1,1)),
    ARIMA_auto  = ARIMA(fibre)
  )

# Visualizza AIC, BIC, ecc. per scegliere il migliore
glance(mod_arima_fibre)

# Seleziona il miglior modello manualmente
mod_best_arima <- mod_arima_fibre %>% select(ARIMA_211)
fc_best_arima <- forecast(mod_best_arima, h = nrow(test_fibre))

# Grafico finale
grafico_finale <- autoplot(train_fibre, fibre, color = "black") +
  autolayer(fc_best_arima, level = NULL, color = "green", linetype = "dotted", size = 1.2) +
  autolayer(test_fibre, fibre, color = "red", size = 1.2) +
  labs(title = "Migliori modelli – produzione fibre", x = "Anno", y = "Produzione") +
  theme_minimal()

print(grafico_finale)


# Accuratezza dei modelli
accuracy(fc_arima_fibre, test_fibre)


# Report dettagliati
report(mod_ets_fibre)
report(mod_arima_fibre)

