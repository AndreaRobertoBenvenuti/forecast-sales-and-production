# 📊 Time Series Forecasting - Production Prediction System

[![R](https://img.shields.io/badge/R-4.0%2B-blue)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/license-MIT-green)]()
[![Status](https://img.shields.io/badge/status-active-success)]()

Sistema completo di previsione per serie temporali di produzione industriale. Implementa modelli statistici tradizionali (ARIMA, ETS), approcci ibridi personalizzati e machine learning ensemble (Random Forest, XGBoost) con gestione avanzata di outlier, change points e regressori esterni.

## 🎯 Panoramica del Progetto

Il progetto nasce dall'esigenza di creare un **modulo predittivo accurato** per quantità prodotte mensili, integrabile in sistemi gestionali aziendali per ottimizzare:
- Pianificazione della produzione
- Gestione delle scorte
- Prevenzione stock-out
- Riduzione costi operativi

### Flusso di Lavoro
```
Data Warehouse → RStudio (Elaborazione + Forecasting) → Qlik Sense (Visualizzazione)
```

## 📁 Struttura della Repository

```
📦 TimeSeriesForecasting-Production/
├── 📂 AllTests/                    # Script di test e sperimentazione
│   ├── test*.R                     # Test vari modelli e approcci
│   └── confronto*.R                # Confronti comparativi
├── 📂 Data/                        # Dataset
│   ├── *.xlsx, *.csv              # Dati pubblici e sintetici
│   └── XX*.xlsx                    # [NOT ON GITHUB] Dati privati aziendali
├── 📂 Finali/                      # 🔥 Script principali pronti all'uso
│   ├── ARIMA_AutoSelection_7M.R
│   ├── HybridModel_Advanced_Forecast.R
│   ├── RobustForecasting_Production.R
│   └── 📂 Relazioni/               # Documentazione tecnica completa
│       ├── Feature_engineering.pdf
│       ├── HybridMLModel.pdf
│       ├── HybridModel.pdf
│       └── SASRelazione.pdf
├── 📂 Images/                      # Grafici e visualizzazioni
├── 📂 Lecture/                     # Materiale di studio e ricerca
└── 📂 S&OpPresentazione/           # Sales & Operations Planning docs
```

## 🚀 Script Principali (Cartella `Finali/`)

### 1. **ARIMA_AutoSelection_7M.R** - Modello di Selezione Automatica

**Descrizione:** Approccio "textbook" che confronta sistematicamente multiple metodologie e seleziona automaticamente il migliore tramite AIC.

**Caratteristiche:**
- ✅ Portfolio completo: MEAN, NAIVE, DRIFT, ETS, ARIMA, SNAIVE
- ✅ Selezione automatica basata su criteri statistici (AIC/BIC)
- ✅ Gestione regressori esterni (n° commesse)
- ✅ Output chiaro e interpretabile
- ⚡ Veloce e leggero computazionalmente

**Orizzonte:** 7 mesi (marzo-settembre 2025)

**Quando usarlo:**
- Analisi esplorative rapide
- Baseline di riferimento
- Serie temporali con pattern semplici
- Risorse computazionali limitate

---

### 2. **HybridModel_Advanced_Forecast.R** - Modello Ibrido Personalizzato

**Descrizione:** Architettura ibrida proprietaria su misura per il dominio produttivo che sfrutta relazioni specifiche quantità-commesse.

**Architettura Multi-Componente:**
```
┌─────────────────────────────────────────────────────┐
│  1. Regressione Quadratica (quantità-commesse)      │
│  2. Stagionalità Adattiva (STL robusta)             │
│  3. ARIMA sui Residui (dinamiche stocastiche)       │
│  4. Trend Breaks Detection (cambiamenti strutturali)│
│  5. Ensemble Pesato (performance-based)             │
└─────────────────────────────────────────────────────┘
```

**Innovazioni:**
- 🎯 Weighted moving average per stima commesse future
- 🔄 Adaptive seasonal component
- 📊 Intervalli confidenza dinamici crescenti
- 🛡️ Vincoli realistici anti-overfitting

**Quando usarlo:**
- Relazioni evidenti quantità-commesse
- Pattern multivariati complessi
- Necessità accuratezza elevata
- Previsioni medio termine (7 mesi)

---

### 3. **RobustForecasting_Production.R** - Framework Enterprise

**Descrizione:** Soluzione production-ready con gestione errori estensiva, fallback multipli e massima robustezza operativa.

**Framework Completo:**
- 🔧 **Tradizionali:** ARIMA, ETS (tutte le varianti)
- 🚀 **Moderni:** Prophet (trend non-lineari)
- 🤖 **Machine Learning:** Random Forest, XGBoost (caricamento condizionale)
- 🎭 **Ensemble:** Combinazioni weighted e stacked

**Robustezza Operativa:**
- ✅ Safe loading di package opzionali
- ✅ Error handling graceful con alternative
- ✅ Data validation automatica
- ✅ Fallback strategies a cascata
- ✅ Export automatico risultati

**Orizzonte:** 12 mesi (pianificazione long-term)

**Quando usarlo:**
- Implementazioni in produzione
- Dati problematici/incompleti
- Necessità affidabilità 24/7
- Gestione automatica errori critica

---

## 📊 Matrice Decisionale

| Criterio | ARIMA Auto | Hybrid Model | Robust Framework |
|----------|------------|--------------|------------------|
| **Complessità** | ⭐ Bassa | ⭐⭐⭐ Alta | ⭐⭐ Media |
| **Accuratezza** | ⭐⭐ Media | ⭐⭐⭐ Alta | ⭐⭐⭐ Alta |
| **Velocità** | 🚀 2-5 min | ⏱️ 10-15 min | ⏱️ 15-25 min |
| **Interpretabilità** | ⭐⭐⭐ Alta | ⭐⭐ Media | ⭐⭐ Media |
| **Robustezza** | ⭐⭐ Media | ⭐⭐ Media | ⭐⭐⭐ Molto Alta |
| **Manutenzione** | ✅ Semplice | ⚠️ Complessa | ✅ Gestibile |

## 🛠️ Setup e Installazione

### Requisiti di Sistema
- **R:** versione ≥ 4.0
- **RStudio:** raccomandato per gestione progetti
- **RAM:** minimo 8GB (16GB consigliati per ML models)

### Dipendenze R

#### Pacchetti Core (obbligatori)
```r
install.packages(c(
  "readxl",      # Import Excel
  "dplyr",       # Data manipulation  
  "lubridate",   # Date handling
  "ggplot2",     # Visualizzazioni
  "forecast",    # Modelli ARIMA/ETS
  "fpp3",        # Forecasting framework
  "janitor"      # Pulizia nomi colonne
))
```

#### Pacchetti Avanzati (opzionali per RobustForecasting)
```r
install.packages(c(
  "prophet",       # Facebook Prophet
  "randomForest",  # Random Forest
  "xgboost",       # XGBoost
  "changepoint",   # PELT algorithm
  "Rbeast"         # BEAST algorithm
))
```

### Quick Start

1. **Clone repository**
```bash
git clone https://github.com/tuousername/TimeSeriesForecasting-Production.git
cd TimeSeriesForecasting-Production
```

2. **Prepara i dati**
    - Formato richiesto: Excel/CSV con colonne `anno`, `mese`, `qta_prodotta`, `commessa`
    - Posiziona file in `Data/`

3. **Esegui script principale**
```r
# Apri RStudio
source("Finali/ARIMA_AutoSelection_7M.R")  # Per iniziare velocemente

# Oppure
source("Finali/HybridModel_Advanced_Forecast.R")  # Per accuratezza massima

# Oppure  
source("Finali/RobustForecasting_Production.R")   # Per produzione
```

4. **Risultati**
    - Grafici salvati in `Images/`
    - CSV esportati per Qlik Sense
    - Console output con metriche performance

## 📖 Documentazione Tecnica

Nella cartella `Finali/Relazioni/` trovi 4 documenti PDF completi:

| Documento | Contenuto | Pagine |
|-----------|-----------|--------|
| **SASRelazione.pdf** | Overview progetto, confronto 3 approcci, workflow | 19     |
| **HybridModel.pdf** | Architettura ibrida base, teoria matematica | 20     |
| **HybridMLModel.pdf** | ML Ensemble, feature engineering avanzato | 26     |
| **Feature_engineering.pdf** | Tecniche creazione variabili predittive | 46     |

## 🔍 Esempi Pratici

### Esempio 1: Previsione Rapida con ARIMA

```r
# Carica librerie
library(forecast)
library(readxl)
library(dplyr)

# Carica dati
file_path <- "Data/tuofile.xlsx"
dati <- read_excel(file_path)

# Esegui script
source("Finali/ARIMA_AutoSelection_7M.R")

# Output: previsioni 7 mesi con intervalli confidenza
```

### Esempio 2: Modello Ibrido per Pattern Complessi

```r
# Per dati con forte relazione quantità-commesse
source("Finali/HybridModel_Advanced_Forecast.R")

# Il modello automaticamente:
# 1. Modella relazione quadratica quantità-commesse
# 2. Estrae stagionalità adattiva
# 3. Applica ARIMA ai residui
# 4. Rileva trend breaks
# 5. Combina tutto in ensemble pesato
```

### Esempio 3: Produzione con Gestione Errori

```r
# Per ambiente produzione con dati reali (possibili problemi)
source("Finali/RobustForecasting_Production.R")

# Features:
# - Gestione automatica valori mancanti
# - Fallback se modelli complessi falliscono
# - Export automatico risultati
# - Logging completo per debugging
```

## 📈 Feature Engineering

Il sistema implementa **40+ variabili derivate** automaticamente:

### Categorie di Feature
- **Temporali:** sin/cos encoding mesi, trimestre, stagione
- **Lag:** valori ritardati (1, 3, 12 mesi)
- **Moving Averages:** medie mobili (3, 6, 12 mesi)
- **Derivate:** crescita %, volatilità, efficiency ratios
- **Regressori:** n° commesse, tipi commessa, mix produttivo

Dettagli completi in `Relazioni/Feature_engineering.pdf`

## 🔬 Testing e Validazione

### Metodologia
- **Time Series Cross-Validation:** expanding window rispettando ordine temporale
- **Metriche:** MAE, RMSE, MAPE
- **Intervalli:** Copertura 80% e 95%

### Script di Test
Nella cartella `AllTests/` trovi numerosi test comparativi:
- `testDatiQuantitaProduzioneARIMAETS.R`
- `confrontoARIMAETSPROHET.R`
- Altri test esplorativi

## ⚠️ Note sui Dati Privati

**File con prefisso `XX*` nella cartella `Data/` contengono informazioni aziendali sensibili e NON sono inclusi su GitHub.**

Per utilizzare gli script con i tuoi dati:
1. Rinomina i tuoi file seguendo la convenzione del progetto
2. Aggiorna il path in `file_path <- "Data/tuofile.xlsx"`
3. Assicurati che il formato corrisponda (colonne: anno, mese, qta_prodotta, commessa)

## 🗺️ Roadmap

### ✅ Completato
- [x] 3 approcci di forecasting completi
- [x] Feature engineering automatico
- [x] Change point detection (PELT, BEAST)
- [x] ML Ensemble (Random Forest, XGBoost)
- [x] Documentazione tecnica completa

### 🔄 In Sviluppo
- [ ] API REST per integrazione real-time
- [ ] Dashboard interattiva Shiny
- [ ] Integrazione dati esterni (ISTAT, Eurostat)
- [ ] Deep Learning (LSTM, Transformer)

### 🔮 Futuro
- [ ] Sistema di alert automatico
- [ ] Multi-horizon forecasting simultaneo
- [ ] Explainable AI (SHAP values)
- [ ] Containerizzazione Docker

## 🤝 Contributi

Questo progetto è stato sviluppato come parte di un progetto presso **SAS Evolution srl** per ottimizzare i processi di pianificazione produttiva.

## 📄 Licenza

MIT License - vedi file LICENSE per dettagli

## 📧 Contatti

**Andrea Roberto Benvenuti**
- GitHub: [@AndreaRB](https://github.com/AndreaRB)

---

*Ultimo aggiornamento: Ottobre 2025*