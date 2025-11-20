## Per ogni anno di split (es. 1970, 1971, ...), il codice addestra i modelli ETS e ARIMA su tutti i dati fino a quell’anno e testa su tutti gli anni 
## successivi, calcolando le metriche di accuratezza complessive su quel periodo di test (di lunghezza variabile). Questo approccio evidenzia come le 
## performance cambiano quando si amplia l’orizzonte di previsione.

library(readxl)
library(fpp3)
library(dplyr)

# Carica il file Excel manualmente
file <- file.choose()
dati <- read_excel(file)

# Prepara la serie (puoi cambiare 'cotone' con 'fiocco' o 'fibre' se necessario)
dati_ts <- dati %>%
  select(anno, cotone) %>%
  filter(!is.na(cotone)) %>%
  mutate(
    anno = as.numeric(anno),
    cotone = as.numeric(cotone)
  ) %>%
  as_tsibble(index = anno)

# Definisci gli anni di split da testare
anni_split <- seq(1970, 1985)

# Ciclo su ciascun anno di split
risultati <- lapply(anni_split, function(anno_split) {
  train <- dati_ts %>% filter(anno <= anno_split)
  test  <- dati_ts %>% filter(anno > anno_split)
  
  if (nrow(test) < 3) return(NULL)  # Salta se test troppo corto
  
  # Modelli
  mod_ets <- train %>% model(ETS(cotone ~ error("A") + trend("A") + season("N")))
  mod_arima <- train %>% model(ARIMA(cotone))
  
  # Previsioni
  fc_ets <- forecast(mod_ets, h = nrow(test))
  fc_arima <- forecast(mod_arima, h = nrow(test))
  
  # Accuracy
  acc_ets <- accuracy(fc_ets, test) %>% mutate(model = "ETS", split = anno_split)
  acc_arima <- accuracy(fc_arima, test) %>% mutate(model = "ARIMA", split = anno_split)
  
  bind_rows(acc_ets, acc_arima)
})


# Combina tutto in un'unica tabella
tabella_risultati <- bind_rows(risultati)

# Visualizza
print(tabella_risultati)

# (Opzionale) Visualizzazione grafica
library(ggplot2)
ggplot(tabella_risultati, aes(x = split, y = RMSE, color = model)) +
  geom_line() +
  geom_point() +
  labs(title = "Performance RMSE su split temporali", x = "Anno di split", y = "RMSE") +
  theme_minimal()

