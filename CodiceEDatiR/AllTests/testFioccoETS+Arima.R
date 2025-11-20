## Script R che importa un file Excel, trasforma i dati di “fiocco” in una tsibble annuale, suddivide train (≤1980) e test (>1980), stima modelli 
## ETS e ARIMA, genera le previsioni, le confronta visivamente con i dati reali, calcola le metriche di accuratezza e stampa i report dei modelli.

library(readxl)
library(fpp3)

# Seleziona il file
file <- file.choose()
dati <- read_excel(file)

# Prepara i dati per "fiocco"
dati_fiocco <- dati %>%
  select(anno, fiocco) %>%
  filter(!is.na(fiocco)) %>%  
  mutate(anno = as.numeric(anno), fiocco = as.numeric(fiocco)) %>%
  as_tsibble(index = anno)

# Split
train_fiocco <- dati_fiocco %>% filter(anno <= 1980)
test_fiocco  <- dati_fiocco %>% filter(anno > 1980)

# ETS con trend
mod_ets_fiocco <- train_fiocco %>%
  model(ETS(fiocco ~ error("A") + trend("A") + season("N")))

# ARIMA automatico
mod_arima_fiocco <- train_fiocco %>%
  model(ARIMA(fiocco))

# Previsioni
fc_ets_fiocco <- mod_ets_fiocco %>% forecast(h = nrow(test_fiocco))
fc_arima_fiocco <- mod_arima_fiocco %>% forecast(h = nrow(test_fiocco))

grafico_fiocco <- autoplot(train_fiocco, fiocco, color = "black") +
  autolayer(fc_arima_fiocco, level = NULL, color = "purple", linetype = "dashed", size = 1.2) +
  autolayer(fc_ets_fiocco, level = NULL, color = "green", linetype = "dotted", size = 1.2) +
  autolayer(test_fiocco, fiocco, color = "red", size = 1.2) +
  labs(title = "Confronto ETS vs ARIMA – produzione fiocco", x = "Anno", y = "Produzione") +
  theme_minimal()


print(grafico_fiocco)

# Accuracy
accuracy(fc_ets_fiocco, test_fiocco)
accuracy(fc_arima_fiocco, test_fiocco)

bind_rows(
  accuracy(fc_ets_fiocco, test_fiocco),
  accuracy(fc_arima_fiocco, test_fiocco)
)

# Report modelli
report(mod_ets_fiocco)
report(mod_arima_fiocco)

