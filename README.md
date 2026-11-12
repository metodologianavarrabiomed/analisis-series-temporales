<div align="center">

# 📈 Series Temporales: Análisis de Impacto de Intervenciones

![R](https://shields.io)
![Shiny](https://shields.io)
![R Markdown](https://shields.io)

**Aplicación interactiva desarrollada en R Shiny para evaluar el efecto de intervenciones en series temporales mediante escenarios contrafactuales.**

[![Abrir App](https://shields.io🚀_Abrir_Aplicación_en_Línea-2EA44F?style=for-the-badge)](https://janiretx.shinyapps.io/Series_temporales_medicamentos/)

</div>

---

<details>
<summary><b>📑 Tabla de Contenidos</b></summary>

- [🎯 Objetivo](#-objetivo)
- [✨ Funcionalidades Principales](#-funcionalidades-principales)
- [🧮 Metodología Estadística](#-metodología-estadística)
- [📂 Formato de los Datos](#-formato-de-los-datos)
- [🔍 Interpretación de Resultados](#-interpretación-de-resultados)
- [🛠️ Tecnologías Utilizadas](#️-tecnologías-utilizadas)
- [⚠️ Consideraciones](#️-consideraciones)

</details>

---

## 🎯 Objetivo

Comparar **valores observados** frente a **valores esperados** en un escenario contrafactual (sin intervención) mediante la modelización estadística de series temporales.

---

## ✨ Funcionalidades Principales

* **📊 Series Individuales:** Permite el análisis múltiple o individual de series mediante estrategias de modelización globales o personalizadas, con generación de gráficos y reportes interactivos.
* **⚖️ Análisis de Ratios:** Calcula ratios semanales a partir de la relación entre numeradores y denominadores, evaluando la serie original y su media móvil de orden 4.

---

## 🧮 Metodología Estadística

> [!NOTE]
> Los modelos se entrenan **exclusivamente con datos previos a la intervención** para proyectar el escenario contrafactual.

* **ARIMA + Fourier:** Combina ARIMA no estacional con tendencia lineal y términos de Fourier seleccionados por `AICc`.
* **Prophet:** Captura tendencias con cambios estructurales y estacionalidad mediante funciones de Fourier.
* **STL + ARIMA / STL + ETS:** Descompone la serie (estacionalidad, tendencia y residuo) y modela el componente residual con ARIMA o suavizado exponencial.

---

## 📂 Formato de los Datos

Los archivos Excel (`.xlsx`) para series individuales requieren tres columnas:
* `fecha`: Fecha semanal (`YYYY-MM-DD`).
* `tipo`: Identificador de la serie (texto).
* `pacientes`: Valor semanal registrado (numérico).

---

## 🔍 Interpretación de Resultados

* **Leyenda:** Grises para valores observados, verde continuo para ajuste previo, verde discontinuo para la predicción contrafactual y línea roja vertical para la fecha de intervención.
* **Métricas:** Calcula la diferencia ($\text{Observado} - \text{Predicho}$) y el porcentaje de caída durante el periodo de observación.

---

## 🛠️ Tecnologías Utilizadas

* **R, Shiny, CSS y R Markdown** para el procesamiento, interfaz visual y generación de reportes.

---

## ⚠️ Consideraciones

> [!IMPORTANT]
> La fiabilidad de los resultados depende de la calidad de los datos iniciales y el comportamiento histórico de cada serie. Las estimaciones son de carácter estadístico.
