## Script R che importa dati da Excel, li trasforma in tsibble annuale, li suddivide in train (≤1980) e test (>1980), stima modelli ETS e ARIMA, 
## addestra un Prophet tarato (log-fit, pochi changepoint, regressori fiocco/fibre, forecast mensile aggregato annuo), traccia tutte le previsioni a 
## confronto con i reali e calcola le relative metriche di accuratezza.

library(readxl)
library(fpp3)
library(prophet)
library(lubridate)

# ───────────────────────────────────────────────────────────────────────────────
# 1) Caricamento dati
# ───────────────────────────────────────────────────────────────────────────────
file <- file.choose()
dati <- read_excel(file)

dati_cotone <- dati %>%
  select(anno, cotone) %>%
  filter(!is.na(cotone)) %>%
  mutate(
    anno   = as.numeric(anno),
    cotone = as.numeric(cotone)
  ) %>%
  as_tsibble(index = anno)

# ───────────────────────────────────────────────────────────────────────────────
# 2) Train / Test split
# ───────────────────────────────────────────────────────────────────────────────
train <- dati_cotone %>% filter(anno <= 1980)
test  <- dati_cotone %>% filter(anno  > 1980)

# ───────────────────────────────────────────────────────────────────────────────
# 3) ETS & ARIMA (stesso di prima)
# ───────────────────────────────────────────────────────────────────────────────
mod_ets   <- train   %>% model(ETS(cotone ~ error("A") + trend("A") + season("N")))
mod_arima <- train   %>% model(ARIMA(cotone))
fc_ets     <- forecast(mod_ets,   h = nrow(test))
fc_arima   <- forecast(mod_arima, h = nrow(test))

# ───────────────────────────────────────────────────────────────────────────────
# 4) Prophet TUNED
# ───────────────────────────────────────────────────────────────────────────────

# 4.1 Log-trasformazione e preparazione dei dati
# ───────────────────────────────────────────────────────────────────────────────
# 4) Prophet con regressori fiocco e fibre
# ───────────────────────────────────────────────────────────────────────────────

# 4.1 Prepara i dati per Prophet, includendo fiocco e fibre
train_prophet <- train %>%
  as_tibble() %>%
  transmute(
    ds     = as.Date(paste0(anno, "-01-01")),
    y      = log(cotone),
    fiocco = fiocco,
    fibre  = fibre
  )

# 4.2 Fit di Prophet con regressori e tuning dei changepoint
m_prophet <- prophet(
  train_prophet,
  growth                  = "linear",
  n.changepoints          = 5,
  changepoint.prior.scale = 0.5,
  yearly.seasonality      = FALSE,
  weekly.seasonality      = FALSE,
  daily.seasonality       = FALSE,
  verbose                 = FALSE
) %>%
  add_regressor("fiocco") %>%
  add_regressor("fibre")

# 4.3 Genera future: DS + fiocco + fibre
#    (usiamo i valori del test set per fiocco/fibre annuali)
future_prophet <- train_prophet %>%
  # Estendi fino alla fine del test a intervalli annuali
  bind_rows(
    test %>%
      as_tibble() %>%
      transmute(
        ds     = as.Date(paste0(anno, "-01-01")),
        y      = NA,           # placeholder
        fiocco = fiocco,
        fibre  = fibre
      )
  ) %>%
  # Mantieni solo ds, fiocco, fibre
  select(ds, fiocco, fibre)

# 4.4 Predict
forecast_prophet <- predict(m_prophet, future_prophet)

# 4.5 Ri-aggregazione ad anno e back-transform
fc_prophet <- forecast_prophet %>%
  transmute(
    anno  = year(ds),
    yhat  = exp(yhat)
  ) %>%
  filter(anno > max(train$anno)) %>%
  group_by(anno) %>%
  summarise(.mean = mean(yhat), .groups = "drop") %>%
  as_tsibble(index = anno)

# ───────────────────────────────────────────────────────────────────────────────
# 5) Grafico di confronto
# ───────────────────────────────────────────────────────────────────────────────
grafico <- autoplot(train, cotone) +
  autolayer(fc_ets,     cotone,    level = NULL, color = "blue",   linetype = "dashed") +
  autolayer(fc_arima,   cotone,    level = NULL, color = "green",  linetype = "dotted") +
  autolayer(test,       cotone,    color = "red") +
  autolayer(fc_prophet, .mean,     color = "purple", linetype = "dotdash") +
  labs(
    title = "ETS vs ARIMA vs Prophet (log-fit + monthly forecast)",
    x     = "Anno", y = "Produzione"
  ) +
  theme_minimal()

print(grafico)

# ───────────────────────────────────────────────────────────────────────────────
# 6) Accuracy
# ───────────────────────────────────────────────────────────────────────────────
acc_ets   <- accuracy(fc_ets,   test)   %>% mutate(model = "ETS")
acc_arima <- accuracy(fc_arima, test)   %>% mutate(model = "ARIMA")

# Prophet (manuale su y back-transform)
dfp <- test %>%
  left_join(fc_prophet, by = "anno") %>%
  as_tibble()

acc_prophet <- dfp %>%
  summarise(
    ME    = mean(cotone - .mean, na.rm = TRUE),
    RMSE  = sqrt(mean((cotone - .mean)^2, na.rm = TRUE)),
    MAE   = mean(abs(cotone - .mean), na.rm = TRUE),
    MAPE  = mean(abs((cotone - .mean) / cotone), na.rm = TRUE) * 100
  ) %>%
  mutate(.model = "Prophet", .type = "Test", model = "Prophet")

bind_rows(acc_ets, acc_arima, acc_prophet)

