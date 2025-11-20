# === Caricamento pacchetti (solo la prima volta serve installarli) ===


library(fpp3)

# Crea una serie temporale fittizia e trasformala in tsibble
set.seed(123)
dati <- tibble(
  mese = yearmonth(seq.Date(from = as.Date("2020-01-01"), 
                            to = as.Date("2023-12-01"), by = "month")),
  vendite = 100 + 10*sin(2*pi*1:48/12) + rnorm(48, 0, 5)
) %>% 
  as_tsibble(index = mese)

# Controllo
print(dati)
autoplot(dati)


# Modello ETS
modello_ets <- dati %>%
  model(ETS(vendite))

# Previsione
previsione <- modello_ets %>%
  forecast(h = "12 months")

# Visualizzazione
autoplot(previsione, dati)

# Accuratezza
accuracy(modello_ets)


