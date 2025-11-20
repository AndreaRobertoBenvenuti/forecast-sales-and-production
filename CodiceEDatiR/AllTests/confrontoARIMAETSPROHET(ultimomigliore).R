# Dopo aver fatto partire testQuantitaProduzioneARIMAETS e 
# testDatiQuantitaProduzioneProphet puoi far partire questo confronto

# ─────────────────────────────────────────────
# Prophet: estrai previsioni come tsibble
# ─────────────────────────────────────────────
forecast_prophet <- forecast %>%
  select(ds, yhat) %>%
  rename(mese = ds, .mean = yhat) %>%
  mutate(mese = yearmonth(mese)) %>%
  as_tsibble(index = mese)

# Filtro le date del test
forecast_prophet <- forecast_prophet %>%
  filter(mese %in% test$mese)

# ─────────────────────────────────────────────
# Estrai ultimi/primissimi valori per collegare le linee
# ─────────────────────────────────────────────

# Ultimo punto del training
ultimo_mese <- max(train$mese)
ultimo_val <- train %>%
  filter(mese == ultimo_mese) %>%
  pull(qta_prodotta)

# Primo mese del test
primo_mese_test <- min(test$mese)

# Valori al primo mese del test per ogni modello
valore_test <- test %>% filter(mese == primo_mese_test) %>% pull(qta_prodotta)

valore_ets <- fc_ets %>%
  as_tsibble() %>%
  filter(mese == primo_mese_test) %>%
  pull(.mean)

valore_arima <- fc_arima %>%
  as_tsibble() %>%
  filter(mese == primo_mese_test) %>%
  pull(.mean)

valore_prophet <- forecast_prophet %>%
  filter(mese == primo_mese_test) %>%
  pull(.mean)

# ─────────────────────────────────────────────
# Grafico finale con tutte le previsioni + linee collegate
# ─────────────────────────────────────────────
# Crea un dataframe fittizio solo per la legenda
legenda_df <- tibble::tibble(
  mese = rep(ultimo_mese, 5),
  .mean = rep(NA_real_, 5),
  modello = c("Train", "Test", "ETS", "ARIMA", "Prophet")
)

grafico_completo <- autoplot(train, qta_prodotta, color = "black", show.legend = TRUE) +
  autolayer(fc_ets, level = NULL, color = "blue", linetype = "dashed", show.legend = TRUE) +
  autolayer(fc_arima, level = NULL, color = "green", linetype = "dotted", show.legend = TRUE) +
  autolayer(forecast_prophet, .mean, color = "purple", linetype = "dotdash", show.legend = TRUE) +
  autolayer(test, qta_prodotta, color = "red", show.legend = TRUE) +
  geom_segment(data = data.frame(x = ultimo_mese, y = ultimo_val, xend = primo_mese_test, yend = valore_test),
               aes(x = x, y = y, xend = xend, yend = yend), color = "red", linewidth = 1, inherit.aes = FALSE) +
  geom_segment(data = data.frame(x = ultimo_mese, y = ultimo_val, xend = primo_mese_test, yend = valore_ets),
               aes(x = x, y = y, xend = xend, yend = yend), color = "blue", linewidth = 1, inherit.aes = FALSE) +
  geom_segment(data = data.frame(x = ultimo_mese, y = ultimo_val, xend = primo_mese_test, yend = valore_arima),
               aes(x = x, y = y, xend = xend, yend = yend), color = "green", linewidth = 1, inherit.aes = FALSE) +
  geom_segment(data = data.frame(x = ultimo_mese, y = ultimo_val, xend = primo_mese_test, yend = valore_prophet),
               aes(x = x, y = y, xend = xend, yend = yend), color = "purple", linewidth = 1, inherit.aes = FALSE) +
  # Aggiunta leggenda fittizia  
  geom_line(data = legenda_df, aes(x = mese, y = .mean, color = modello), size = 1, show.legend = TRUE) +
  scale_color_manual(values = c(
    "Train" = "black",
    "Test" = "red",
    "ETS" = "blue",
    "ARIMA" = "green",
    "Prophet" = "purple"
  )) +
  labs(title = "Confronto ETS vs ARIMA vs Prophet – Produzione",
       x = "Data",
       y = "Q.tà prodotta",
       color = "Fonte") +
  theme_minimal()

print(grafico_completo)


