# Análisis de impacto de intervenciones en series temporales

Aplicación web desarrollada en **R Shiny** para analizar la evolución de series temporales (como el consumo o prescripción de medicamentos) y cuantificar el efecto de una intervención comparando los datos reales frente a un escenario contrafactual.

> ### [**Abrir aplicación en línea**](https://janiretx.shinyapps.io/Series_temporales_medicamentos/)

## Qué hace la aplicación

* **Series individuales:** Permite analizar una o varias series a la vez. Puedes elegir entre ajustar un único modelo ganador para todas las series (*Modo Global*) o seleccionar el mejor modelo de forma independiente para cada serie (*Modo Individual*). Incluye gráficos interactivos y exportación de informes.
* **Análisis de ratios:** Calcula el ratio semanal a partir de numeradores y denominadores ($\sum \text{numerador} / \sum \text{denominador}$), modelizando tanto la serie original como su media móvil de orden 4.

## Modelos incluidos

Todos los modelos se entrenan únicamente con los datos anteriores a la fecha de intervención para proyectar qué habría ocurrido sin ella:

* **ARIMA + Fourier:** Modelo ARIMA no estacional con tendencia lineal y pares seno/coseno de Fourier (frecuencia semanal = 52). El número óptimo de términos ($K \in$) se selecciona minimizando el `AICc`.
* **Prophet:** Modelo aditivo que detecta automáticamente cambios de tendencia (*changepoints*) y ajusta la estacionalidad anual y semanal mediante series de Fourier.
* **STL + ARIMA:** Descompone la serie en estacionalidad, tendencia y residuo mediante STL, y ajusta un modelo ARIMA sobre el componente residual.
* **STL + ETS:** Utiliza la misma descomposición STL, pero modela el residuo mediante suavizado exponencial (ETS).

## Estructura del archivo de entrada

Para el análisis de series individuales hay que subir un fichero Excel (`.xlsx`) que contenga exactamente estas tres columnas:

| Columna | Descripción | Formato |
| :--- | :--- | :--- |
| `fecha` | Fecha semanal de observación | `YYYY-MM-DD` |
| `tipo` | Nombre o categoría de la serie (ej. tipo de fármaco) | Texto |
| `pacientes` | Valor numérico semanal | Numérico |

## Lectura de resultados

* **Gráficos:** La línea gris muestra los datos observados, la verde continua el ajuste del modelo antes de la intervención, la verde discontinua la proyección contrafactual y la línea roja vertical marca la fecha de intervención.
* **Tablas:** Durante el periodo de observación definido se calcula la diferencia ($\text{Observado} - \text{Predicho}$) y el porcentaje de variación o caída respecto a lo esperado: $(\text{Diferencia} / \text{Predicho}) \times 100$.

## Tecnologías

* **R** y **Shiny** para el cálculo estadístico y la interfaz.
* **R Markdown** para la generación automática de informes descargables.
* **CSS** para los estilos de la aplicación.


## ⚠️ Consideraciones

> La fiabilidad de los resultados depende de la calidad de los datos iniciales y el comportamiento histórico de cada serie. Las estimaciones son de carácter estadístico.
