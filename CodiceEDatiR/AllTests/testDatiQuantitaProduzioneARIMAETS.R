# scegliere documento excel MargineCommessa

library(readxl)
library(fpp3)
library(prophet)
library(lubridate)
library(janitor)
library(dplyr)
library(tsibble)

# 1) Caricamento e pulizia dati
file <- file.choose()
mesi_it <- c("gen", "feb", "mar", "apr", "mag", "giu", 
             "lug", "ago", "set", "ott", "nov", "dic")

dati <- read_excel(file, na = "-") %>%
  clean_names() %>%
  mutate(
    mese = match(tolower(mese), mesi_it),
    anno = as.numeric(anno),
    qta_prodotta = as.numeric(qta_prodotta)
  )

# 2) Aggregazione e trasformazione in tsibble mensile
dati_ts <- dati %>%
  filter(!is.na(qta_prodotta), !is.na(anno), !is.na(mese)) %>%
  group_by(anno, mese) %>%
  summarise(qta_prodotta = sum(qta_prodotta, na.rm = TRUE), .groups = "drop") %>%
  mutate(mese = yearmonth(paste(anno, sprintf("%02d", mese), sep = "-"))) %>%
  as_tsibble(index = mese) %>%
  fill_gaps()

# Rimuove eventuali righe con NA (create da fill_gaps)
dati_ts <- dati_ts %>%
  filter(!is.na(qta_prodotta))

# Verifica che i dati siano regolari (senza buchi)
has_gaps(dati_ts)

# 3) Train/Test split
train <- dati_ts %>% filter(year(mese) == 2024)
test  <- dati_ts %>% filter(year(mese) == 2025 & month(mese) != 3)


# 4) Modellazione
mod_ets   <- train %>% model(ETS(qta_prodotta ~ error("A") + trend("A") + season("N")))
mod_arima <- train %>% model(ARIMA(qta_prodotta))

fc_ets   <- forecast(mod_ets, h = nrow(test))
fc_arima <- forecast(mod_arima, h = nrow(test))

# 5) Grafico
grafico <- autoplot(train, qta_prodotta, color = "black") +
  autolayer(fc_ets, level = NULL, color = "blue", linetype = "dashed") +
  autolayer(fc_arima, level = NULL, color = "green", linetype = "dotted") +
  autolayer(test, qta_prodotta, color = "red") +
  labs(title = "Confronto ETS vs ARIMA – Produzione", x = "Data", y = "Q.tà prodotta") +
  theme_minimal()

print(grafico)

# 6) Accuracy + report
bind_rows(
  accuracy(fc_ets, test),
  accuracy(fc_arima, test)
)

report(mod_arima)
report(mod_ets)
