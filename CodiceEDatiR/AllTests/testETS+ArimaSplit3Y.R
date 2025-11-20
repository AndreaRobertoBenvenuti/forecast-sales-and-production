## Simile nella logica, ma qui per ogni anno di split il codice esegue una rolling forecast fissa di 3 anni: addestra i modelli fino all’anno t e 
## testa solo sui 3 anni successivi (t+1 a t+3). In questo modo si misura la capacità predittiva su un orizzonte costante e confrontabile, rendendo 
## più omogeneo il confronto tra gli anni.

library(readxl)
library(fpp3)
library(dplyr)
library(ggplot2)

# Carica il file Excel
file <- file.choose()
dati <- read_excel(file)

# Scegli la serie temporale (es. 'cotone', 'fiocco', 'fibre')
dati_ts <- dati %>%
  select(anno, cotone) %>%
  filter(!is.na(cotone)) %>%
  mutate(
    anno = as.numeric(anno),
    cotone = as.numeric(cotone)
  ) %>%
  as_tsibble(index = anno)

# Definizione degli split: train fino a questo anno incluso, test per i 3 anni successivi
anni_split <- seq(1965, 1982)  # si ferma prima così da avere sempre 3 anni di test

# Rolling forecast 3 anni fissi
risultati <- lapply(anni_split, function(anno_split) {
  train <- dati_ts %>% filter(anno <= anno_split)
  test  <- dati_ts %>% filter(anno > anno_split & anno <= anno_split + 3)
  
  if (nrow(test) < 3) return(NULL)  # Skippa se non ci sono abbastanza anni nel test
  
  # Modelli
  mod_ets <- train %>% model(ETS(cotone ~ error("A") + trend("A") + season("N")))
  mod_arima <- train %>% model(ARIMA(cotone))
  
  # Previsioni
  fc_ets <- forecast(mod_ets, h = 3)
  fc_arima <- forecast(mod_arima, h = 3)
  
  # Accuracy
  acc_ets <- accuracy(fc_ets, test) %>% mutate(model = "ETS", split = anno_split)
  acc_arima <- accuracy(fc_arima, test) %>% mutate(model = "ARIMA", split = anno_split)
  
  bind_rows(acc_ets, acc_arima)
})

# Tabella finale
tabella_risultati <- bind_rows(risultati)

# Stampa i risultati
print(tabella_risultati, n = Inf)


# Visualizzazione grafica
ggplot(tabella_risultati, aes(x = split, y = RMSE, color = model)) +
  geom_line() +
  geom_point() +
  labs(title = "Performance RMSE (previsione a 3 anni)", x = "Anno finale del train", y = "RMSE") +
  theme_minimal()

