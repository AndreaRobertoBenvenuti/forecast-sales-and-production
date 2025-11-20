## Script R che importa un file Excel, trasforma i dati in una tsibble annuale, divide train (≤1980) e test (>1980), stima modelli ETS e ARIMA, 
## genera previsioni, le confronta graficamente con i dati reali e calcola le metriche di accuratezza, stampando infine i report dei modelli.

library(readxl)
library(fpp3)

# Scegli il file manualmente
file <- file.choose()

# Ti apre la finestra per scegliere il nuovo file Excel
dati <- read_excel(file)


dati_cotone <- dati %>%
  select(anno, cotone) %>%
  filter(!is.na(cotone)) %>%  
  mutate(anno = as.numeric(anno), cotone = as.numeric(cotone)) %>%
  as_tsibble(index = anno)

train <- dati_cotone %>% filter(anno <= 1980)
test  <- dati_cotone %>% filter(anno > 1980)

# ETS con trend additivo
mod_ets <- train %>%
  model(ETS(cotone ~ error("A") + trend("A") + season("N")))

# ARIMA automatico
mod_arima <- train %>%
  model(ARIMA(cotone))


# Previsioni per entrambi
fc_ets <- mod_ets %>% forecast(h = nrow(test))
fc_arima <- mod_arima %>% forecast(h = nrow(test))

# Puoi anche salvare direttamente questo grafico
grafico <- autoplot(train, cotone, color = "black") +
  autolayer(fc_ets, level = NULL, color = "blue", linetype = "dashed", size = 1.2) +
  autolayer(fc_arima, level = NULL, color = "green", linetype = "dotted", size = 1.2) +
  autolayer(test, cotone, color = "red", size = 1.2) +
  labs(title = "Confronto ETS vs ARIMA – produzione cotone", x = "Anno", y = "Produzione") +
  theme_minimal()

print(grafico)


# Accuracy
accuracy(fc_ets, test)
accuracy(fc_arima, test)

bind_rows(
  accuracy(fc_ets, test),
  accuracy(fc_arima, test)
)


report(mod_arima)
report(mod_ets)


