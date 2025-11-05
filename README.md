# analisis-datos-ambientales
Proyecto de análisis de datos ambientales en Chile, enfocado en la gestión y visualización de residuos.
# ♻️ Análisis Estratégico de la Ineficiencia de Residuos en Chile (Ley REP)

## 🎯 Resumen Ejecutivo
Este proyecto demuestra la capacidad de transformar datos brutos en inteligencia estratégica, enfocándose en la **Ley de Responsabilidad Extendida del Productor (Ley REP)** en Chile.

El objetivo principal es medir la **Brecha de Ineficiencia Global** (residuos destinados a Eliminación), identificando dónde se encuentran las mayores oportunidades de Valorización.

## 📈 KPIs y Análisis Estratégico
El script en R (`analisis_completo_residuos.R`) genera 4 visualizaciones clave para la toma de decisiones:

* **KPI Global:** Tasa de No Valorización Nacional (Eliminación / Total de Residuos).
* **Análisis 1 (Estrategia):** Composición del Residuo por Tipo de Tratamiento.
* **Análisis 2 (Escala Geográfica):** Top 10 Regiones con mayor volumen No Valorizado.
* **Análisis 3 (Escala Sectorial):** Top 10 Rubros con mayor brecha de Ineficiencia.
* **Análisis 4 (Tendencia):** Evolución de la Tasa de Ineficiencia a lo largo del tiempo.

## 🛠️ Herramientas y Stack Tecnológico
* **Lenguaje:** R
* **Librerías:** `tidyverse` (para manipulación) y `ggplot2` (para visualización).
* **Visualización (Adicional):** Se utilizó Tableau para el diseño inicial del dashboard.
* **Control de Versiones y Datos:** GitHub.
* **Fuente de Datos:** Plataforma RETC/Ley REP (Muestra de datos).

## ⚙️ Instrucciones de Ejecución
El script `analisis_completo_residuos.R` es autónomo: descarga automáticamente los datos desde este repositorio y genera los 4 gráficos al ser ejecutado en R.

---
*Autor: Valentina Pinilla*
