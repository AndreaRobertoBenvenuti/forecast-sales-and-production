## Script R che importa i dati da Excel, li trasforma in un tsibble annuale, esegue per ogni anno da 1965 a 1980 un rolling forecast di 3 anni con 
## modelli ETS, ARIMA e Prophet in tryCatch, raccoglie le metriche RMSE in una tabella e ne traccia l’andamento su un grafico.

library(readxl)
library(fpp3)
library(dplyr)
library(ggplot2)
library(prophet)
library(tibble)

file <- file.choose()
dati <- read_excel(file)

dati_ts <- dati %>%
  select(anno, cotone) %>%
  filter(!is.na(cotone)) %>%
  mutate(
    anno = as.numeric(anno),
    cotone = as.numeric(cotone)
  ) %>%
  as_tsibble(index = anno)

anni_split <- seq(1965, 1980)  # evita 1981-82 che hanno meno di 3 anni di test

risultati <- lapply(anni_split, function(anno_split) {
  train <- dati_ts %>% filter(anno <= anno_split)
  test  <- dati_ts %>% filter(anno > anno_split & anno <= anno_split + 3)
  if (nrow(test) < 3) return(NULL)
  
  # ETS e ARIMA (resta tutto uguale) …
  acc_ets   <- accuracy(forecast(train %>% model(ETS(cotone~error("A")+trend("A")+season("N"))), h=3), test) %>% mutate(model="ETS")
  acc_arima <- accuracy(forecast(train %>% model(ARIMA(cotone)), h=3), test) %>% mutate(model="ARIMA")
  
  # Prophet
  train_prophet <- train %>% rename(ds=anno, y=cotone) %>% as.data.frame()
  test_prophet  <- test  %>% rename(ds=anno, y=cotone) %>% as.data.frame()
  
  # Esegui fit in tryCatch, ma NON mettere cat() come ultimo comando
  result <- tryCatch({
    m <- prophet(train_prophet, yearly.seasonality=FALSE,
                 weekly.seasonality=FALSE,
                 daily.seasonality=FALSE,
                 verbose=FALSE)
    future <- make_future_dataframe(m, periods=3, freq="year")
    fc_p <- predict(m, future) %>%
      filter(ds %in% test_prophet$ds) %>%
      select(ds, yhat)
    dfm <- left_join(test_prophet, fc_p, by="ds")
    
    bind_rows(acc_ets, acc_arima,
              tibble(
                .model = "Prophet",
                .type  = "Test",
                ME     = mean(dfm$y - dfm$yhat),
                RMSE   = sqrt(mean((dfm$y - dfm$yhat)^2)),
                MAE    = mean(abs(dfm$y - dfm$yhat)),
                MAPE   = mean(abs((dfm$y - dfm$yhat) / dfm$y)) * 100,
                model  = "Prophet",
                split  = anno_split
              )
    )
  }, error = function(e) {
    # Se fallisce Prophet, restituisci comunque ETS+ARIMA
    bind_rows(acc_ets, acc_arima)
  })
  
  cat(">>> Split", anno_split, "processato\n")  # DEBUG
  result  # qui restituisco finalmente il data.frame
})

# Ora questo funziona di nuovo
tabella_risultati <- bind_rows(risultati)
print(tabella_risultati, n = Inf)


ggplot(tabella_risultati, aes(x = split, y = RMSE, color = model)) +
  geom_line() +
  geom_point() +
  labs(title = "Performance RMSE (previsione a 3 anni)", x = "Anno finale del train", y = "RMSE") +
  theme_minimal()

