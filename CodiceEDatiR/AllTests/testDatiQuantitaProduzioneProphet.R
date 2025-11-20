library(readxl)
library(fpp3)
library(prophet)
library(lubridate)
library(janitor)
library(dplyr)
library(tsibble)
library(ggplot2)

# ─────────────────────────────────────────────
# 1) Caricamento e pulizia dati
# ─────────────────────────────────────────────
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

# ─────────────────────────────────────────────
# 2) Aggregazione e trasformazione in tsibble mensile
# ─────────────────────────────────────────────
dati_ts <- dati %>%
  filter(!is.na(qta_prodotta), !is.na(anno), !is.na(mese)) %>%
  group_by(anno, mese) %>%
  summarise(qta_prodotta = sum(qta_prodotta, na.rm = TRUE), .groups = "drop") %>%
  mutate(mese = yearmonth(paste(anno, sprintf("%02d", mese), sep = "-"))) %>%
  as_tsibble(index = mese) %>%
  fill_gaps() %>%
  filter(!is.na(qta_prodotta))

# ─────────────────────────────────────────────
# 3) Prophet: preparazione dati
# ─────────────────────────────────────────────
prophet_df <- dati_ts %>%
  as_tibble() %>%
  mutate(ds = as.Date(mese)) %>%
  select(ds, y = qta_prodotta)

# Split temporale
train_prophet <- prophet_df %>% filter(year(ds) == 2024)
test_prophet  <- prophet_df %>% filter(year(ds) == 2025 & month(ds) != 3)

# ─────────────────────────────────────────────
# 4) Addestramento modello Prophet
# ─────────────────────────────────────────────
m <- prophet(train_prophet,
             seasonality.mode = 'additive',
             yearly.seasonality = FALSE,
             weekly.seasonality = FALSE,
             daily.seasonality = FALSE)

# ─────────────────────────────────────────────
# 5) Forecast (generazione corretta con estensione)
# ─────────────────────────────────────────────
h <- nrow(test_prophet)
future <- make_future_dataframe(m, periods = h, freq = "month")
forecast <- predict(m, future)

# ─────────────────────────────────────────────
# 6) Visualizzazione base Prophet
# ─────────────────────────────────────────────
plot(m, forecast)
prophet_plot_components(m, forecast)

# ─────────────────────────────────────────────
# 7) Accuracy Prophet
# ─────────────────────────────────────────────
pred_vs_real <- forecast %>%
  select(ds, yhat) %>%
  inner_join(test_prophet, by = "ds")

accuracy_df <- pred_vs_real %>%
  mutate(
    error = y - yhat,
    abs_error = abs(error),
    perc_error = abs_error / y * 100
  )

mape <- mean(accuracy_df$perc_error)
mae  <- mean(accuracy_df$abs_error)
rmse <- sqrt(mean(accuracy_df$error^2))

cat("\nProphet – Accuracy:\n")
cat(sprintf("MAPE: %.2f%%\n", mape))
cat(sprintf("MAE: %.0f\n", mae))
cat(sprintf("RMSE: %.0f\n", rmse))

# ─────────────────────────────────────────────
# 8) Grafico confronto Prophet vs Test
# ─────────────────────────────────────────────
confronto_df <- test_prophet %>%
  select(ds, y) %>%
  left_join(forecast %>% select(ds, yhat), by = "ds") %>%
  filter(!is.na(yhat))

print("Confronto Prophet vs Test:")
print(confronto_df)

# Grafico
ggplot(confronto_df, aes(x = ds)) +
  geom_line(aes(y = y), color = "red", size = 1.2) +
  geom_line(aes(y = yhat), color = "blue", linetype = "dashed", size = 1.2) +
  labs(title = "Confronto Prophet – Valori Reali vs Previsioni",
       x = "Data",
       y = "Q.tà prodotta",
       caption = "Rosso: reale · Blu tratteggiato: previsto") +
  theme_minimal()


