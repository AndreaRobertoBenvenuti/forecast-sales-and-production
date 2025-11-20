library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(lubridate)
library(tsibble)
library(fpp3)
library(openxlsx)
library(ggplot2)
library(stringr)

# 1. Caricamento file
file <- file.choose()
dati <- read_excel(file, col_names = TRUE) %>%
  clean_names()

# 2. Rimuove la seconda riga se è descrittiva (commentala se non serve)
dati <- dati[-1, ]

# 3. Trasforma in formato tsibble
dati_long <- dati %>%
  filter(
    !is.na(cliente_cd),
    ordinato_quantita != "-",
    !cliente_cd %in% c("Totali", "-")
  ) %>%
  mutate(
    anno = as.numeric(anno),
    mese_num = match(tolower(mese), c("gen", "feb", "mar", "apr", "mag", "giu",
                                      "lug", "ago", "set", "ott", "nov", "dic")),
    data = yearmonth(paste(anno, sprintf("%02d", mese_num), sep = "-")),
    qta_prodotta = as.numeric(na_if(ordinato_quantita, "-"))
  ) %>%
  group_by(cliente_cd, data) %>%
  summarise(qta_prodotta = sum(qta_prodotta, na.rm = TRUE), .groups = "drop") %>%
  as_tsibble(index = data, key = cliente_cd)

# 4. Split e modello ETS
train <- dati_long %>% filter(year(data) == 2024)
test  <- dati_long %>% filter(year(data) == 2025 & month(data) %in% 1:3)

modelli_ets <- train %>% model(ETS(qta_prodotta ~ error("A") + trend("A") + season("N")))
previsioni  <- forecast(modelli_ets, h = 3)


# 5. ETS solo su clienti con dati sufficienti
clienti_validi <- dati_long %>%
  count(cliente_cd) %>%
  filter(n >= 3) %>%
  pull(cliente_cd)

clienti_esclusi <- setdiff(unique(dati$cliente_cd), clienti_validi)
print(clienti_esclusi)


dati_long <- dati_long %>%
  filter(cliente_cd %in% clienti_validi) %>%
  fill_gaps(.full = TRUE)

# 6. Split & modello
train <- dati_long %>% filter(year(data) == 2024)
test  <- dati_long %>% filter(year(data) == 2025 & month(data) %in% 1:3)

modelli_ets <- train %>% model(ETS(qta_prodotta ~ error("A") + trend("A") + season("N")))
previsioni  <- forecast(modelli_ets, h = 3)

# 7. Pulizia delle previsioni
previsioni_tidy <- previsioni %>% as_tibble() %>%
  filter(!is.na(.mean))

accuracy_df <- accuracy(previsioni, test)

# 8. Esporta in Excel
wb <- createWorkbook()
addWorksheet(wb, "Previsioni")
writeData(wb, "Previsioni", previsioni_tidy)

addWorksheet(wb, "Accuratezza")
writeData(wb, "Accuratezza", accuracy_df %>% as_tibble())

saveWorkbook(wb, "ETS_previsioni_accuracy.xlsx", overwrite = TRUE)


# Grafico per ogni cliente
clienti <- unique(dati_long$cliente_cd)

# 9. Grafico per ogni cliente con almeno 2 osservazioni nel totale + almeno 1 nel train o test
clienti_plot <- dati_long %>%
  group_by(cliente_cd) %>%
  summarise(n_obs = n()) %>%
  filter(n_obs >= 2) %>%
  pull(cliente_cd)

for (cliente in clienti_plot) {
  storico <- dati_long %>% filter(cliente_cd == cliente)
  previsione_cliente <- previsioni %>% filter(cliente_cd == cliente)
  test_cliente <- test %>% filter(cliente_cd == cliente)
  
  # Salta se anche la previsione e test sono vuoti (nessun punto da plottare)
  if (nrow(storico) < 2 && nrow(previsione_cliente) == 0 && nrow(test_cliente) == 0) {
    next
  }
  
  p <- autoplot(storico, qta_prodotta) +
    autolayer(previsione_cliente, level = NULL, color = "purple") +
    autolayer(test_cliente, qta_prodotta, color = "red") +
    labs(
      title = paste("ETS – Cliente:", cliente),
      x = "Mese", y = "Q.tà prodotta"
    )
  
  print(p)
}
