# ==============================================================================
# PROJECT: Chile Waste Valorization Potential - Full Strategic Analysis (4 VIZ + 1 KPI)
# AUTHOR: [Valentina Pinilla] 
# DATE: [NOVIEMBRE 2025] 
# GOAL: Demonstrate full analytical pipeline: Strategy, Scale (Region & Sector), and Trend.
# PLATFORM: R and Tidyverse (Data manipulation and visualization)
# DATA SOURCE: Plataforma RETC/Ley REP (https://datosretc.mma.gob.cl/).
# NOTE: This analysis is performed on a SAMPLE of the original database.
# ==============================================================================

# 1. Load Required Libraries
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(scales)) install.packages("scales") 
library(tidyverse)
library(scales)

# 2. Load Data from GitHub
github_raw_url <- "https://raw.githubusercontent.com/vapinillahr-pa/analisis-datos-ambientales/main/residuos_chile_muestra.csv"

data <- read_csv(github_raw_url)

# 3. Feature Engineering: Create Non-Valorized Volume (CRITICAL STEP)
data_processed <- data %>%
  # La condición se simplifica a la Eliminación, que es tu brecha de ineficiencia.
  mutate(
    Non_Valorized_Tons = if_else(
      tratamiento_nivel_1 == 'Eliminación', 
      cantidad_toneladas, # Si es Eliminación, toma el total de la fila
      0 # Si NO es Eliminación (es Valorización), el volumen es 0
    )
  )

# Define la fuente para las etiquetas de los gráficos
data_caption <- "Fuente: Plataforma RETC/Ley REP (https://datosretc.mma.gob.cl/). Análisis sobre una MUESTRA de datos."

# ==============================================================================
# 4. ANALYSIS 1: GLOBAL INEFFICIENCY KPI & VISUALIZATION (STRATEGY)
# ==============================================================================

# 4A. KPI CALCULATION
analysis_global <- data_processed %>%
  summarise(
    Total_General = sum(cantidad_toneladas, na.rm = TRUE),
    Total_Non_Valorizado = sum(Non_Valorized_Tons, na.rm = TRUE),
    KPI_Ineficiencia_Global = Total_Non_Valorizado / Total_General
  )

# OUTPUT: Print KPI to console
cat("========================================================\n")
cat("1. KPI INEFICIENCIA ESTRATÉGICA GLOBAL (NACIONAL)\n")
cat(paste("Tasa de No Valorización Nacional:", scales::percent(analysis_global$KPI_Ineficiencia_Global), "\n"))
cat("========================================================\n")


# 4B. VISUALIZATION 1: DONUT CHART LOGIC (GLOBAL COMPOSITION)
data_viz_global <- data_processed %>%
  group_by(tratamiento_nivel_1) %>%
  summarise(Tons = sum(cantidad_toneladas, na.rm = TRUE))

plot_global <- data_viz_global %>%
  ggplot(aes(x = "", y = Tons, fill = tratamiento_nivel_1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(
    title = "1. Composición del Residuo - Análisis Global (Muestra)", 
    subtitle = paste("Tasa de Ineficiencia Nacional:", scales::percent(analysis_global$KPI_Ineficiencia_Global)),
    caption = data_caption, 
    fill = "Tratamiento"
  )
print(plot_global)


# ==============================================================================
# 5. ANALYSIS 2 & VISUALIZATION 2 & 3 (SCALE: REGION & SECTOR)
# ==============================================================================

# 5A. VISUALIZATION 2: BAR CHART (VOLUME BY REGION)
regional_analysis <- data_processed %>%
  group_by(region) %>% 
  summarise(
    Total_Non_Valorized_Volume = sum(Non_Valorized_Tons, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Non_Valorized_Volume)) %>%
  slice_head(n = 10) 

regional_plot <- regional_analysis %>%
  ggplot(aes(x = reorder(region, Total_Non_Valorized_Volume), 
             y = Total_Non_Valorized_Volume)) +
  geom_col(fill = "#0072B2") + 
  coord_flip() + 
  labs(
    title = "2. Foco: Top 10 Volumen No Valorizado por Región (Muestra)",
    caption = data_caption,
    x = "Región",
    y = "Toneladas No Valorizadas"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)
print(regional_plot)


# 5B. VISUALIZATION 3: BAR CHART (VOLUME BY RUBRO)
rubro_analysis <- data_processed %>%
  group_by(rubro_vu_limpio) %>% 
  summarise(
    Total_Non_Valorized_Volume = sum(Non_Valorized_Tons, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Non_Valorized_Volume)) %>%
  slice_head(n = 10) 

rubro_plot <- rubro_analysis %>%
  ggplot(aes(x = reorder(rubro_vu_limpio, Total_Non_Valorized_Volume), 
             y = Total_Non_Valorized_Volume)) +
  geom_col(fill = "#56B4E9") + 
  coord_flip() + 
  labs(
    title = "3. Foco: Top 10 Volumen No Valorizado por Rubro (Muestra)",
    caption = data_caption,
    x = "Rubro",
    y = "Toneladas No Valorizadas"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)
print(rubro_plot)


# ==============================================================================
# 6. ANALYSIS 3 & VISUALIZATION 4 (TREND)
# ==============================================================================

# 6A. TREND CALCULATION
trend_analysis <- data_processed %>%
  group_by(año) %>% 
  summarise(
    Avg_Inefficiency_Rate = sum(Non_Valorized_Tons, na.rm = TRUE) / sum(cantidad_toneladas, na.rm = TRUE)
  )

# 6B. VISUALIZATION 4: TREND LINE CHART
trend_plot <- trend_analysis %>%
  ggplot(aes(x = año, y = Avg_Inefficiency_Rate)) +
  geom_line(color = "#D55E00", linewidth = 1.2) + 
  geom_point(color = "#D55E00", size = 3) + 
  labs(
    title = "4. Tasa de Ineficiencia de Residuos - Tendencia Anual (Muestra)", 
    caption = data_caption,
    x = "Año",
    y = "Tasa de No Valorización (%)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = trend_analysis$año) 

print(trend_plot)
# ==============================================================================
# FIN DEL SCRIPT
