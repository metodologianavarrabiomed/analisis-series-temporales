suppressPackageStartupMessages({
  library(dplyr)
  library(zoo)
  library(ggplot2)
  library(plotly)
  library(lubridate)
  library(knitr)
  library(kableExtra)
  library(scales)
  library(forecast)
  library(prophet)
})


# =============================== UTILIDADES ===============================

calc_aicc_arima <- function(fit) {
  aic  <- AIC(fit)
  k    <- length(fit$coef)
  n    <- length(fit$x)
  aic + (2*k*(k+1)) / pmax(1, (n - k - 1))
}

calc_rmse <- function(resids) sqrt(mean(resids^2, na.rm = TRUE))

calc_ljung <- function(resids) {
  n <- sum(is.finite(resids))
  lag <- max(5, min(20, floor(n / 4)))
  tryCatch({
    Box.test(resids, lag = lag, type = "Ljung-Box")$p.value
  }, error = function(e) NA_real_)
}

to_zoo <- function(values, dates) {
  zoo::zoo(as.numeric(values), order.by = as.Date(dates))
}

es_serie_valida <- function(y, fechas) {

  # ordenar fechas
  fechas <- sort(as.Date(fechas))

  # diferencias entre fechas (en días)
  diffs <- as.numeric(diff(fechas))
  diffs <- diffs[is.finite(diffs) & diffs > 0]

  # no hay ni siquiera un intervalo válido
  if (length(diffs) < 1) {
    return("menos de 2 periodos")
  }

  # periodicidad dominante
  freq_tab <- table(diffs)
  dominante <- as.numeric(names(freq_tab)[which.max(freq_tab)])

  # duración total de la serie (en días)
  duracion_total <- as.numeric(max(fechas) - min(fechas))

  # número de periodos completos
  n_periodos <- duracion_total / dominante

  # ---- MENSAJE 1: menos de 2 periodos ----
  if (n_periodos < 2) {
    return("menos de 2 periodos")
  }

  # 2. periodicidad dominante (moda)
  freq_tab <- table(diffs)
  dominante <- as.numeric(names(freq_tab)[which.max(freq_tab)])
  prop_dominante <- max(freq_tab) / sum(freq_tab)
  
  # criterio: al menos 60% de los intervalos iguales
  if (prop_dominante < 0.6) {
    return("no periódica")
  }
  
  return(NA_character_)
}

validar_ratio_series <- function(num_df, den_df) {
  
  # Validar numerador
  motivo_num <- es_serie_valida(
    y = num_df$pacientes,
    fechas = num_df$fecha
  )
  
  if (!is.na(motivo_num)) {
    return(paste("numerador:", motivo_num))
  }
  
  # Validar denominador
  motivo_den <- es_serie_valida(
    y = den_df$pacientes,
    fechas = den_df$fecha
  )
  
  if (!is.na(motivo_den)) {
    return(paste("denominador:", motivo_den))
  }
  
  return(NA_character_)
}



# =============================== MODELOS ===============================

# ------------------------- STL + ETS -------------------------
stl_ets <- function(ts_tr, datos, interv, z_tr) {
  
  fit_stl_ets <- forecast::stlm(
    ts_tr,
    s.window = "periodic",
    robust   = TRUE,
    method   = "ets",
    etsmodel = "AAN" # la estacionalidad la aporta STL; ETS capta nivel + tendencia
  )
  
  h <- sum(datos$fecha > interv)
  
  # Fechas de predicción (si hay futuro)
  if (h > 0) {
    fc <- forecast::forecast(fit_stl_ets, h = h)
    first_fc_date <- datos$fecha[min(which(datos$fecha > interv))]
    fc_dates      <- seq.Date(from = first_fc_date, by = "week", length.out = h)
    
    fc_mean_z  <- to_zoo(fc$mean, fc_dates)
    fc_lower_z <- to_zoo(fc$lower[, "95%"], fc_dates)
    fc_upper_z <- to_zoo(fc$upper[, "95%"], fc_dates)
  } else {
    fc_mean_z <- fc_lower_z <- fc_upper_z <- NULL
  }
  
  fitted_z  <- to_zoo(fitted(fit_stl_ets), zoo::index(z_tr))
  resids    <- as.numeric(stats::residuals(fit_stl_ets))
  
  # AICc del submodelo ETS dentro de STLM (si existe)
  aicc_val <- tryCatch({
    if (!is.null(fit_stl_ets$model)) forecast::AICc(fit_stl_ets$model) else NA_real_
  }, error = function(e) NA_real_)
  
  list(
    name       = "stl_ets",
    fit        = fit_stl_ets,
    aicc       = aicc_val,
    ljung      = calc_ljung(resids),
    rmse       = calc_rmse(resids),
    fitted_z   = fitted_z,
    fc_mean_z  = fc_mean_z,
    fc_lower_z = fc_lower_z,
    fc_upper_z = fc_upper_z
  )
}

# ------------------------- STL + ARIMA -------------------------
stl_arima <- function(ts_tr, datos, interv, z_tr) {
  
  fit_stl_arima <- forecast::stlm(
    ts_tr,
    s.window = "periodic",
    robust   = TRUE,
    method   = "arima",
    modelfunction = function(x) forecast::Arima(x, order = c(1,1,1), include.drift = TRUE)
  )
  
  h <- sum(datos$fecha > interv)
  
  # Fechas de predicción (si hay futuro)
  if (h > 0) {
    fc_arima <- forecast::forecast(fit_stl_arima, h = h)
    first_fc_date <- datos$fecha[min(which(datos$fecha > interv))]
    fc_dates      <- seq.Date(from = first_fc_date, by = "week", length.out = h)
    
    fc_mean_z   <- to_zoo(fc_arima$mean,             fc_dates)
    fc_lower_z  <- to_zoo(fc_arima$lower[, "95%"],   fc_dates)
    fc_upper_z  <- to_zoo(fc_arima$upper[, "95%"],   fc_dates)
  } else {
    fc_mean_z <- fc_lower_z <- fc_upper_z <- NULL
  }
  
  fitted_z  <- to_zoo(fitted(fit_stl_arima), zoo::index(z_tr))
  resids    <- as.numeric(stats::residuals(fit_stl_arima))
  
  # AICc del componente ARIMA subyacente (fit_stl_arima$fit)
  aicc_val <- tryCatch({
    if (!is.null(fit_stl_arima$fit)) calc_aicc_arima(fit_stl_arima$fit) else NA_real_
  }, error = function(e) NA_real_)
  
  list(
    name       = "stl_arima",
    fit        = fit_stl_arima,
    aicc       = aicc_val,
    ljung      = calc_ljung(resids),
    rmse       = calc_rmse(resids),
    fitted_z   = fitted_z,
    fc_mean_z  = fc_mean_z,
    fc_lower_z = fc_lower_z,
    fc_upper_z = fc_upper_z
  )
}

# ------------------------- Prophet -------------------------
prophet_model <- function(datos, ts_tot, ts_tr, z_tr) {
  # datos: data.frame con columnas fecha, pacientes (filtrado al 'tipo' actual)
  pr_df <- datos |>
    dplyr::rename(ds = fecha, y = pacientes) |>
    dplyr::arrange(ds)
  
  # Ajustamos sobre el tramo de entrenamiento (mismo nº puntos que ts_tr)
  n_tr <- length(ts_tr)
  stopifnot(n_tr <= nrow(pr_df))
  tr_end_date <- pr_df$ds[n_tr]
  pr_tr <- pr_df |>
    dplyr::filter(ds <= tr_end_date)
  
  # Entrenamos Prophet
  pr_mod <- prophet::prophet(
    pr_tr,
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality  = FALSE
  )
  
  h <- length(ts_tot) - length(ts_tr)
  
  if (h > 0) {
    pr_future   <- prophet::make_future_dataframe(pr_mod, periods = h, freq = "week")
    # >>>>> CAMBIO AQUÍ: usar predict() genérico, NO prophet::predict <<<<<
    forecast_pr <- predict(pr_mod, pr_future)
    forecast_pr$ds <- as.Date(forecast_pr$ds)
    
    # Predicciones (sólo futuro)
    fc_full <- dplyr::filter(forecast_pr, ds > tr_end_date)
    fc_mean_z   <- to_zoo(fc_full$yhat,        fc_full$ds)
    fc_lower_z  <- to_zoo(fc_full$yhat_lower,  fc_full$ds)
    fc_upper_z  <- to_zoo(fc_full$yhat_upper,  fc_full$ds)
  } else {
    fc_mean_z <- fc_lower_z <- fc_upper_z <- NULL
  }
  
  # Ajustes (fitted) en el tramo histórico utilizado
  # >>>>> CAMBIO AQUÍ: usar predict() genérico <<<<<
  fitted_df <- predict(pr_mod, pr_mod$history)
  fitted_z <- to_zoo(fitted_df$yhat, pr_mod$history$ds)
  fitted_z <- fitted_z[index(fitted_z) <= max(index(z_tr))]
  
  # Métricas en entrenamiento
  hist_join <- pr_tr |>
    dplyr::left_join(dplyr::select(fitted_df, ds, yhat = yhat), by = "ds")
  
  resids <- hist_join$y - hist_join$yhat
  
  list(
    name       = "prophet",
    fit        = pr_mod,
    aicc       = NA_real_,               # Prophet no tiene AICc comparable
    ljung      = calc_ljung(resids),
    rmse       = calc_rmse(resids),
    fitted_z   = fitted_z,
    fc_mean_z  = fc_mean_z,
    fc_lower_z = fc_lower_z,
    fc_upper_z = fc_upper_z
  )
}

# ------------------------- ARIMA + Fourier -------------------------
arima_fourier <- function(ts_tr, z_tr, h) {
  
  AICc_manual <- function(model) {
    aic <- AIC(model)
    k   <- length(coef(model))
    n   <- length(model$x)
    aic + (2*k*(k+1)) / pmax(1, (n - k - 1))
  }
  
  if (h <= 0) {
    # Aun así ajustamos el mejor modelo en entrenamiento para métricas
    K_grid <- 1:10
    results <- list()
    for (K in K_grid) {
      X_tr  <- forecast::fourier(ts_tr, K = K)
      t_tr  <- 1:length(ts_tr)
      X_tr2 <- cbind(X_tr, trend = t_tr)
      
      fit <- try(forecast::auto.arima(
        ts_tr,
        xreg = X_tr2,
        seasonal = FALSE,
        stepwise = FALSE,
        approximation = FALSE
      ), silent = TRUE)
      
      if (!inherits(fit, "try-error")) {
        results[[length(results) + 1]] <- list(
          K    = K,
          aicc = AICc_manual(fit),
          fit  = fit
        )
      }
    }
    stopifnot(length(results) > 0)
    best_idx <- which.min(vapply(results, function(x) x$aicc, numeric(1)))
    fit_best <- results[[best_idx]]$fit
    
    fitted_z <- to_zoo(fitted(fit_best), zoo::index(z_tr))
    resids   <- as.numeric(stats::residuals(fit_best))
    
    orden <- forecast::arimaorder(fit_best)
    
    drift <- ifelse(
      "drift" %in% names(coef(fit_best)),
      "Sí",
      "No"
    )
    
    return(list(
      name       = "arima_fourier",
      fit        = fit_best,
      
      p          = orden[1],
      d          = orden[2],
      q          = orden[3],
      drift      = drift,
      
      aicc       = results[[best_idx]]$aicc,
      ljung      = calc_ljung(resids),
      rmse       = calc_rmse(resids),
      
      fitted_z   = fitted_z,
      fc_mean_z  = fc_mean_z,
      fc_lower_z = fc_lower_z,
      fc_upper_z = fc_upper_z,
      
      K          = best_K
    ))
  }
  
  # Si hay horizonte futuro:
  K_grid <- 1:10
  results <- list()
  
  for (K in K_grid) {
    X_tr  <- forecast::fourier(ts_tr, K = K)
    t_tr  <- 1:length(ts_tr)
    X_tr2 <- cbind(X_tr, trend = t_tr)
    
    fit <- try(forecast::auto.arima(
      ts_tr,
      xreg = X_tr2,
      seasonal = FALSE,
      stepwise = FALSE,
      approximation = FALSE
    ), silent = TRUE)
    
    if (!inherits(fit, "try-error")) {
      results[[length(results) + 1]] <- list(
        K    = K,
        aicc = AICc_manual(fit),
        fit  = fit
      )
    }
  }
  
  stopifnot(length(results) > 0)
  
  best_idx  <- which.min(vapply(results, function(x) x$aicc, numeric(1)))
  best_K    <- results[[best_idx]]$K
  fit_best  <- results[[best_idx]]$fit
  
  if (deparse(substitute(ts_tr)) != "") {
    print(summary(fit_best))
  }
  
  X_tr_best  <- forecast::fourier(ts_tr, K = best_K)
  t_tr       <- 1:length(ts_tr)
  X_tr2_best <- cbind(X_tr_best, trend = t_tr)
  
  X_fut_best  <- forecast::fourier(ts_tr, K = best_K, h = h)
  t_fut       <- (length(ts_tr) + 1):(length(ts_tr) + h)
  X_fut2_best <- cbind(X_fut_best, trend = t_fut)
  
  fc <- forecast::forecast(fit_best, xreg = X_fut2_best, h = h)
  
  pred_dates <- seq(max(zoo::index(z_tr)) + 7, by = "7 days", length.out = h)
  
  fitted_z   <- to_zoo(fitted(fit_best), zoo::index(z_tr))
  fc_mean_z  <- to_zoo(fc$mean,              pred_dates)
  fc_lower_z <- to_zoo(fc$lower[, "95%"],    pred_dates)
  fc_upper_z <- to_zoo(fc$upper[, "95%"],    pred_dates)
  
  resids <- as.numeric(stats::residuals(fit_best))
  
  orden <- forecast::arimaorder(fit_best)
  
  drift <- ifelse(
    "drift" %in% names(coef(fit_best)),
    "Sí",
    "No"
  )
  
  list(
    name       = "arima_fourier",
    fit        = fit_best,
    
    p          = orden[1],
    d          = orden[2],
    q          = orden[3],
    drift      = drift,
    
    aicc       = results[[best_idx]]$aicc,
    ljung      = calc_ljung(resids),
    rmse       = calc_rmse(resids),
    
    fitted_z   = fitted_z,
    fc_mean_z  = fc_mean_z,
    fc_lower_z = fc_lower_z,
    fc_upper_z = fc_upper_z,
    
    K          = best_K
  )
}


# =============================== ELECCIÓN MODELOS POR TIPO ===============================

# eleccion_modelos_por_tipo <- function(AC, ts_tot, ts_tr, interv, z_tr, h) {
#   
#   res_prophet   <- prophet_model(AC, ts_tot, ts_tr)
#   res_stl_ets   <- stl_ets(ts_tr, AC, interv, z_tr)
#   res_stl_arima <- stl_arima(ts_tr, AC, interv, z_tr)
#   res_fourier   <- arima_fourier(ts_tr, z_tr, h)
#   
#   resultados <- list(res_prophet, res_stl_ets, res_stl_arima, res_fourier)
#   
#   
#   
#   tabla <- data.frame(
#     modelo = sapply(resultados, `[[`, "name"),
#     AICc   = sapply(resultados, function(x) ifelse(is.na(x$aicc), Inf, x$aicc)),
#     Ljung  = sapply(resultados, `[[`, "ljung"),
#     RMSE   = sapply(resultados, `[[`, "rmse"),
#     stringsAsFactors = FALSE
#   )
#   
#   tabla <- tabla[!is.na(tabla$AICc) & is.finite(tabla$AICc), ]
#   
#   w_aicc  <- 0.4
#   w_ljung <- 0.3
#   w_rmse  <- 0.3
#   
#   r_aicc  <- rank(tabla$AICc)
#   r_ljung <- rank(-tabla$Ljung)
#   r_rmse  <- rank(tabla$RMSE)
#   
#   tabla$score <- w_aicc*r_aicc + w_ljung*r_ljung + w_rmse*r_rmse
#   
#   list(
#     tabla = tabla,
#     resultados = resultados
#   )
#   
#   print(list(tabla = tabla,resultados = resultados))
# }

eleccion_modelos_por_tipo <- function(AC, ts_tot, ts_tr, interv, z_tr, h) {

  res_prophet   <- prophet_model(AC, ts_tot, ts_tr, z_tr)
  res_stl_ets   <- stl_ets(ts_tr, AC, interv, z_tr)
  res_stl_arima <- stl_arima(ts_tr, AC, interv, z_tr)
  res_fourier   <- arima_fourier(ts_tr, z_tr, h)

  resultados <- list(res_prophet, res_stl_ets, res_stl_arima, res_fourier)
  
  print("ANTES DEL FILTRO")
  
  for(i in seq_along(resultados)){
    
    cat("\nMODELO:", resultados[[i]]$name, "\n")
    cat("AICc:", resultados[[i]]$aicc, "\n")
    cat("Ljung:", resultados[[i]]$ljung, "\n")
    cat("RMSE:", resultados[[i]]$rmse, "\n")
    
  }
  
  # FILTRAR SOLO LOS MODELOS CON AICc FINITO
  valid_idx <- sapply(resultados, function(x) !is.na(x$aicc) && is.finite(x$aicc))
  resultados <- resultados[valid_idx]
  
  # AHORA construimos la tabla solo con resultados válidos
  tabla <- data.frame(
    modelo = sapply(resultados, `[[`, "name"),
    AICc   = sapply(resultados, `[[`, "aicc"),
    Ljung  = sapply(resultados, `[[`, "ljung"),
    RMSE   = sapply(resultados, `[[`, "rmse"),
    stringsAsFactors = FALSE
  )
  
  # Manejar caso sin modelos válidos
  if (nrow(tabla) == 0) {
    return(list(
      tabla = data.frame(
        modelo = NA,
        AICc = NA,
        Ljung = NA,
        RMSE = NA,
        score = NA
      ),
      resultados = resultados
    ))
  }
  
  # … luego el cálculo de score sigue igual
  w_aicc  <- 0.4
  w_ljung <- 0.3
  w_rmse  <- 0.3
  
  r_aicc  <- rank(tabla$AICc)
  r_ljung <- rank(-tabla$Ljung)
  r_rmse  <- rank(tabla$RMSE)
  
  tabla$score <- w_aicc*r_aicc + w_ljung*r_ljung + w_rmse*r_rmse
  
  list(
    tabla = tabla,
    resultados = resultados
  )
  
}

# =============================== ELECCIÓN MODELO GLOBAL ===============================

eleccion_modelo_global <- function(tablas_por_tipo) {
  
  df <- do.call(rbind, lapply(tablas_por_tipo, function(x) x$tabla))
  
  ranking_global <- df %>%
    group_by(modelo) %>%
    summarise(suma_score = sum(score), .groups = "drop") %>%
    arrange(suma_score)
  
  modelo_ganador <- ranking_global$modelo[1]
  
  list(
    modelo = modelo_ganador,
    ranking = ranking_global
  )
}

write.csv(
  data.frame(
    fecha = index(fc_mean_z),
    pred = as.numeric(fc_mean_z),
    lower = as.numeric(fc_lower_z),
    upper = as.numeric(fc_upper_z)
  ),
  "penicilina_contrafactual.csv",
  row.names = FALSE
)

# =============================== TABLA RESULTADOS ===============================

tabla_resultados <- function(
                      obs,
                      pred,
                      lower = NULL,
                      upper = NULL
                  ) {
  
  df <- tibble(
    Fecha      = as.Date(index(obs)),
    Observado  = as.numeric(coredata(obs)),
    Prediccion = as.numeric(coredata(pred)),
    IC95_inf   = as.numeric(coredata(lower)),
    IC95_sup   = as.numeric(coredata(upper))
  ) %>%
    mutate(
      Error    = Prediccion - Observado,
      ErrorAbs = abs(Error)
    )
  
  total_obs  <- sum(df$Observado,  na.rm = TRUE)
  total_pred <- sum(df$Prediccion, na.rm = TRUE)
  diferencia_total <- total_obs - total_pred
  pct_diff <- (diferencia_total / total_pred) * 100
  
  media_obs  <- mean(df$Observado,  na.rm = TRUE)
  media_pred <- mean(df$Prediccion, na.rm = TRUE)
  
  df_ic <- tibble(
    Fecha = as.Date(index(pred)),
    Pred  = as.numeric(pred),
    Lower = pmax(as.numeric(lower), 0),
    Upper = pmax(as.numeric(upper), 0)
  )
  
  ic_global <- df_ic %>%
    summarise(
      IC95_inf = sum(Lower, na.rm = TRUE),
      IC95_sup = sum(Upper, na.rm = TRUE)
    )
  
  resumen_global <- tibble(
    Observado  = total_obs,
    Prediccion = total_pred,
    Diferencia = diferencia_total,
    Caida_pct  = pct_diff,
    Media_observado   = media_obs,
    Media_prediccion  = media_pred,
    IC95_inf         = ic_global$IC95_inf,
    IC95_sup         = ic_global$IC95_sup) 
  
  tabla_global <- resumen_global %>%
    mutate(
      Caida_pct = percent(Caida_pct/100, accuracy = 0.1)
    ) %>%
    kable("html", caption = "Resumen global") %>%
    kable_styling(full_width = FALSE, position = "left") %>%
    column_spec(1:ncol(resumen_global), extra_css = "text-align: center;") %>% as.character() %>% htmltools::HTML()
  
  df_ic <- tibble(
    Fecha = as.Date(index(pred)),
    Pred  = as.numeric(pred),
    Lower = pmax(as.numeric(lower), 0),
    Upper = pmax(as.numeric(upper), 0)
  )
  
  ic_anual <- df_ic %>%
    mutate(Año = format(Fecha, "%Y")) %>%
    group_by(Año) %>%
    summarise(
      IC95_inf = sum(Lower, na.rm = TRUE),
      IC95_sup = sum(Upper, na.rm = TRUE),
      .groups = "drop"
    )
  
  resumen_anual <- df %>%
    mutate(Año = format(Fecha, "%Y")) %>%
    group_by(Año) %>%
    summarise(
      Observado_total  = sum(Observado,  na.rm = TRUE),
      Prediccion_total = sum(Prediccion, na.rm = TRUE),
      Media_observado  = mean(Observado,  na.rm = TRUE),
      Media_prediccion = mean(Prediccion, na.rm = TRUE),
      Diferencia       = Observado_total - Prediccion_total,
      Caida_pct        = (Diferencia / Prediccion_total) * 100,
      .groups = "drop") %>% select(Año,
                                   Observado_total, Prediccion_total,
                                   Diferencia, Caida_pct,
                                   Media_observado, Media_prediccion)
  
  resumen_anual <- resumen_anual %>% left_join(ic_anual, by = "Año")
  
  tabla_anual <- resumen_anual %>%
    mutate(
      Caida_pct = percent(Caida_pct / 100, accuracy = 0.1)
    ) %>%
    kable("html", caption = "Resumen anual") %>%
    kable_styling(full_width = FALSE, position = "left") %>% 
    column_spec(1:ncol(resumen_anual), extra_css = "text-align: center;") %>% as.character() %>% htmltools::HTML()
  
  return(list(
    resumen_global = resumen_global,
    resumen_anual = resumen_anual,
    tabla_global = tabla_global,
    tabla_anual = tabla_anual
  ))
}

# =============================== FUNCIÓN PRINCIPAL ===============================

analisis_series_temporales <- function(datos,
                                       fecha_intervencion,
                                       inicio_observacion = fecha_intervencion,
                                       fin_observacion = max(datos$fecha),
                                       modo = c("individual", "global"),
                                       verbose = FALSE) {
  
  modo <- match.arg(modo)
  
  # --- Preparación ---
  
  
  parse_safe_date <- function(x, default = NA) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) {
      return(default)
    }
    if (inherits(x, "Date")) {
      return(x[1])
    }
    as.Date(lubridate::ymd(x))
  }
  
  fecha_intervencion <- parse_safe_date(fecha_intervencion)
  
  inicio_observacion <- parse_safe_date(
    inicio_observacion,
    default = fecha_intervencion
  )
  
  fin_observacion <- parse_safe_date(
    fin_observacion,
    default = max(datos$fecha, na.rm = TRUE)
  )

  datos <- datos %>% mutate(fecha = as.Date(fecha)) %>% arrange(fecha)
  tipos <- unique(datos$tipo)
  
  resultados_por_tipo <- list()
  tablas_por_tipo     <- list()
  
  series_ignoradas <- tibble(
    tipo = character(),
    motivo = character()
  )
  
  # ======================================================
  # 1) CALCULAR LOS 4 MODELOS POR TIPO (UNA SOLA VEZ)
  # ======================================================
  for (t in tipos) {
    
    AC <- datos %>% filter(tipo == t) %>% arrange(fecha) %>% select(fecha, pacientes)
    
    
    motivo <- es_serie_valida(y = AC$pacientes, fechas = AC$fecha)
    
    if (!is.na(motivo)) {
      series_ignoradas <- add_row(
        series_ignoradas,
        tipo   = t,
        motivo = motivo
      )
      next   # 🔴 SALTA ESTA SERIE SIN ERROR
    }
    
    
    z    <- zoo(AC$pacientes, order.by = AC$fecha)
    z_tr <- window(z, end = fecha_intervencion)
    
    # MALLA FUTURA CANÓNICA (solo fechas realmente observadas)
    malla_futura <- index(z)[index(z) > fecha_intervencion]
    h <- length(malla_futura)
    
    ts_tot <- ts(coredata(z), frequency = 52)
    ts_tr  <- ts(coredata(z_tr), frequency = 52)
    # h      <- length(z) - length(z_tr)
    
    res_tipo <- eleccion_modelos_por_tipo(AC, ts_tot, ts_tr, fecha_intervencion, z_tr, h)
    
    resultados_por_tipo[[t]] <- res_tipo$resultados
    tablas_por_tipo[[t]]     <- res_tipo
  }
  
  tipos_validos <- setdiff(tipos, series_ignoradas$tipo)
  
  tabla_modelos_ganadores <- data.frame()
  
  for (t in names(tablas_por_tipo)) {
    
    tabla_local <- tablas_por_tipo[[t]]$tabla
    
    mejor <- tabla_local[which.min(tabla_local$score), ]
    
    tabla_modelos_ganadores <- rbind(
      tabla_modelos_ganadores,
      data.frame(
        Variable = t,
        Modelo   = mejor$modelo,
        AICc     = round(mejor$AICc, 2),
        LjungBox_p = round(mejor$Ljung, 3),
        RMSE     = round(mejor$RMSE, 2)
      )
    )
  }
  
  print(tabla_modelos_ganadores)
  
  tabla_especificaciones <- data.frame()
  
  for (t in names(resultados_por_tipo)) {
    
    modelo <- resultados_por_tipo[[t]][[which.min(
      sapply(resultados_por_tipo[[t]], function(x) x$aicc)
    )]]
    
    tabla_especificaciones <- rbind(
      tabla_especificaciones,
      data.frame(
        Variable = t,
        ARIMA = paste0(
          "(",
          modelo$p, ",",
          modelo$d, ",",
          modelo$q, ")"
        ),
        Drift = modelo$drift,
        Fourier_K = modelo$K,
        AICc = round(modelo$aicc, 2),
        Ljung_p = round(modelo$ljung, 3)
      )
    )
    
  }
  
  print(tabla_especificaciones)
  
  # ======================================================
  # 2) ELEGIR MODELO GLOBAL (solo si modo="global")
  # ======================================================
  modelo_global <- NULL
  
  if (modo == "global") {
    global <- eleccion_modelo_global(tablas_por_tipo)
    modelo_global <- global$modelo
    if (isTRUE(verbose)) {
      cat("\n[MODO GLOBAL] Modelo elegido:", modelo_global, "\n")
    }
  } else {
    if (isTRUE(verbose)) {
      cat("\n[MODO INDIVIDUAL] Cada tipo usará su mejor modelo local.\n")
    }
  }
  
  # ======================================================
  # 3) APLICAR EL MODELO (GLOBAL O INDIVIDUAL)
  # ======================================================
  salida <- list()
  
  # Cabecera HTML (única, sin duplicados con cat)
  if (modo == "global") {
    salida[["__cabecera__"]] <- htmltools::HTML(
      sprintf("<div style='font-size:24px; font-weight:600; margin-bottom:20px;'> Modo GLOBAL — Modelo seleccionado: <code>%s</code></div>", modelo_global)
    )
  } else {
    salida[["__cabecera__"]] <- htmltools::HTML("<div style='font-size:24px; font-weight:600; margin-bottom:20px;'> Modo INDIVIDUAL — Cada tipo usa su mejor modelo</div>")
  }
  
  for (t in tipos_validos) {
    
    AC <- datos %>% filter(tipo == t) %>% arrange(fecha) %>% select(fecha, pacientes)
    
    z    <- zoo(AC$pacientes, order.by = AC$fecha)
    z_tr <- window(z, end = fecha_intervencion)
    malla_futura <- index(z)[index(z) > fecha_intervencion]
    
    ts_tot <- ts(coredata(z), frequency = 52)
    ts_tr  <- ts(coredata(z_tr), frequency = 52)
    h      <- length(z) - length(z_tr)
    
    lista_modelos <- resultados_por_tipo[[t]]
    
    # ------- Elegir el modelo a aplicar -------
    if (modo == "global") {
      
      idx <- which(sapply(lista_modelos, `[[`, "name") == modelo_global)
      
      if (length(idx) == 1) {
        modelo <- lista_modelos[[idx]]
      } else {
        # Recalcular si faltase (poco probable)
        modelo <- switch(modelo_global,
                         "stl_ets"       = stl_ets(ts_tr, AC, fecha_intervencion, z_tr),
                         "stl_arima"     = stl_arima(ts_tr, AC, fecha_intervencion, z_tr),
                         "prophet"       = prophet_model(AC, ts_tot, ts_tr, z_tr),
                         "arima_fourier" = arima_fourier(ts_tr, z_tr, h))
      }
      modelo_usado <- modelo_global
      
    } else {
      # MODO INDIVIDUAL: usar el mejor local
      tabla_local <- tablas_por_tipo[[t]]$tabla
      mejor_local <- tabla_local$modelo[which.min(tabla_local$score)]
      
      idx <- which(sapply(lista_modelos, `[[`, "name") == mejor_local)
      modelo <- lista_modelos[[idx]]
      
      modelo_usado <- mejor_local
    }
    
    # ------- Gráfico -------
    fit_tr_z  <- modelo$fitted_z
    fc_mean_z <- modelo$fc_mean_z
    
    if (!is.null(modelo$fc_mean_z) && length(modelo$fc_mean_z) > 0) {
      fc_vals <- as.numeric(coredata(modelo$fc_mean_z))
      fc_vals <- fc_vals[seq_len(min(length(fc_vals), length(malla_futura)))]
      
      fc_mean_z <- zoo(fc_vals, order.by = malla_futura[seq_along(fc_vals)])
    } else {
      fc_mean_z <- zoo(numeric(0), order.by = as.Date(character(0)))
    }
    
    
    hay_pred <- !is.null(fc_mean_z) && inherits(fc_mean_z, "zoo") && length(fc_mean_z) > 0
    
    if (hay_pred) {
      
      df_fc <- data.frame(
        fecha = index(modelo$fc_mean_z),
        pred  = as.numeric(modelo$fc_mean_z),
        lower = as.numeric(modelo$fc_lower_z),
        upper = as.numeric(modelo$fc_upper_z)
      )
      
    }
    
    print(length(index(fit_tr_z)))
    print(length(index(fc_mean_z)))
    print(length(index(z)))
    
    # p <- ggplot() +
    #   geom_line(aes(index(z), coredata(z)), colour = "grey60") +
    #   geom_line(aes(index(fit_tr_z), coredata(fit_tr_z)), colour = "#83D0BB") +
    #   { if (!is.null(fc_mean_z) && length(fc_mean_z) > 0)
    #     geom_line(aes(index(fc_mean_z), coredata(fc_mean_z)),
    #               colour = "#83D0BB", linetype = "dashed") } +
    #   geom_vline(xintercept = as.numeric(fecha_intervencion), colour = "red") +
    #   labs(
    #     title = paste("Modelo aplicado:", modelo_usado),  # ← dejamos SOLO el título
    #     x = "Fecha", y = "Pacientes"
    #   ) +
    #   theme_minimal()
    
    p <- ggplot() +
      geom_line(aes(index(z), coredata(z)), colour = "grey60") +
      geom_line(aes(index(fit_tr_z), coredata(fit_tr_z)), colour = "#83D0BB") +
      
      {
        if (!is.null(fc_mean_z) && length(fc_mean_z) > 0)
          geom_ribbon(
            data = df_fc,
            aes(x = fecha, ymin = lower, ymax = upper),
            fill = "#83D0BB",
            alpha = 0.2
          )
      } +
      
      {
        if (!is.null(fc_mean_z) && length(fc_mean_z) > 0)
          geom_line(
            data = df_fc,
            aes(x = fecha, y = pred),
            colour = "#83D0BB",
            linetype = "dashed"
          )
      } +
      
      geom_vline(
        xintercept = as.numeric(fecha_intervencion),
        colour = "red"
      ) +
      
      labs(
        title = paste("Modelo aplicado:", modelo_usado),
        x = "Fecha",
        y = "Pacientes"
      ) +
      
      theme_minimal()
    
    # Bloque: quitamos "modelo_aplicado" para evitar duplicar con el título
    bloque <- list(
      titulo = htmltools::HTML(paste0("<h2>", t, "</h2>")),
      grafico = plotly::as_widget(ggplotly(p))
    )
    
    # ------- Tablas -------
    obs <- z[index(z) >= inicio_observacion & index(z) <= fin_observacion]
    
    if (!is.null(fc_mean_z) && length(fc_mean_z) > 0) {
      fc <- fc_mean_z[index(fc_mean_z) >= inicio_observacion &
                        index(fc_mean_z) <= fin_observacion]
    } else {
      fc <- zoo::zoo(numeric(0), order.by = as.Date(character(0)))
    }
    print(fc)
    print(obs)
    
    if (!is.null(modelo$fc_lower_z) && length(modelo$fc_lower_z) > 0) {
      
      fc_lower <- modelo$fc_lower_z[
        index(modelo$fc_lower_z) >= inicio_observacion &
          index(modelo$fc_lower_z) <= fin_observacion
      ]
      
      fc_upper <- modelo$fc_upper_z[
        index(modelo$fc_upper_z) >= inicio_observacion &
          index(modelo$fc_upper_z) <= fin_observacion
      ]
      
    } else {
      
      fc_lower <- zoo::zoo(
        numeric(0),
        order.by = as.Date(character(0))
      )
      
      fc_upper <- zoo::zoo(
        numeric(0),
        order.by = as.Date(character(0))
      )
      
    }
    res_tablas <- tabla_resultados(
      obs,
      fc,
      fc_lower,
      fc_upper
    )
    print(res_tablas)
    bloque$tabla_global <- res_tablas$tabla_global
    bloque$tabla_anual  <- res_tablas$tabla_anual
    
    salida[[t]] <- bloque
  }
  if (nrow(series_ignoradas) > 0) {
    aviso <- div(
      class = "alert alert-warning",
      h4("⚠️ Fármacos ignorados"),
      p("Las siguientes series no se han analizado:"),
      tags$ul(
        lapply(seq_len(nrow(series_ignoradas)), function(i) {
          tags$li(
            paste0(
              series_ignoradas$tipo[i],
              " — ",
              series_ignoradas$motivo[i]
            )
          )
        })
      )
    )
    
    salida <- c(list(aviso), salida)
  }
  
  do.call(htmltools::tagList, salida)
}

# ====================================== RATIOS ======================================


analisis_ratios_series <- function(datos,
                                   numerador_tipos,
                                   denominador_tipos,
                                   fecha_intervencion,
                                   inicio_observacion = NULL,
                                   fin_observacion = NULL,
                                   modo = c("individual", "global"),
                                   ventana_mm = 4,
                                   verbose = FALSE) {
  
  modo <- match.arg(modo)
  
  # ---------- Preparación fechas ----------
  parse_safe_date <- function(x, default = NA) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) return(default)
    if (inherits(x, "Date")) return(x[1])
    as.Date(lubridate::ymd(x))
  }
  
  fecha_intervencion <- parse_safe_date(fecha_intervencion)
  
  datos <- datos %>%
    mutate(fecha = as.Date(fecha)) %>%
    arrange(fecha)
  
  # ---------- Construir numerador y denominador ----------
  num_series <- datos %>%
    filter(tipo %in% numerador_tipos) %>%
    group_by(fecha) %>%
    summarise(pacientes = sum(pacientes, na.rm = TRUE), .groups = "drop")
  
  den_series <- datos %>%
    filter(tipo %in% denominador_tipos) %>%
    group_by(fecha) %>%
    summarise(pacientes = sum(pacientes, na.rm = TRUE), .groups = "drop")
  
  # ---------- Validar bases ----------
  series_ignoradas <- tibble(tipo = character(), motivo = character())
  
  motivo_base <- validar_ratio_series(num_series, den_series)
  
  mostrar_ratio <- TRUE
  
  if (!is.na(motivo_base)) {
    if (isTRUE(verbose)) {
      message(sprintf("[SERIE IGNORADA] Ratio → %s", motivo_base))
    }
    series_ignoradas <- add_row(
      series_ignoradas,
      tipo   = "Ratio",
      motivo = motivo_base
    )
    mostrar_ratio <- FALSE
  }
  
  # ---------- Crear series SOLO si bases válidas ----------
  series_lista <- list()
  
  if (isTRUE(mostrar_ratio)) {
    ratios <- datos %>%
      group_by(fecha) %>%
      summarise(
        num = sum(pacientes[tipo %in% numerador_tipos], na.rm = TRUE),
        den = sum(pacientes[tipo %in% denominador_tipos], na.rm = TRUE),
        ratio = ifelse(den == 0, NA_real_, num / den),
        .groups = "drop"
      )
    
    ratios$mm4 <- zoo::rollmean(
      ratios$ratio,
      k = ventana_mm,
      fill = NA,
      align = "right"
    )
    
    series_lista[["Ratio"]]       <- ratios %>% select(fecha, valor = ratio)
    series_lista[["Media móvil"]] <- ratios %>% select(fecha, valor = mm4)
  }
  
  resultados_por_tipo <- list()
  tablas_por_tipo     <- list()
  
  # ======================================================
  # 1) CALCULAR MODELOS
  # ======================================================
  for (t in names(series_lista)) {
    AC <- series_lista[[t]] %>% dplyr::rename(pacientes = valor)
    
    motivo <- es_serie_valida(y = AC$pacientes, fechas = AC$fecha)
    if (!is.na(motivo)) {
      series_ignoradas <- add_row(series_ignoradas, tipo = t, motivo = motivo)
      next
    }
    
    z    <- zoo(AC$pacientes, order.by = AC$fecha)
    z_tr <- window(z, end = fecha_intervencion)
    
    ts_tot <- ts(coredata(z), frequency = 52)
    ts_tr  <- ts(coredata(z_tr), frequency = 52)
    h      <- length(z) - length(z_tr)
    
    res_tipo <- eleccion_modelos_por_tipo(AC, ts_tot, ts_tr, fecha_intervencion, z_tr, h)
    resultados_por_tipo[[t]] <- res_tipo$resultados
    tablas_por_tipo[[t]]     <- res_tipo
  }
  
  tipos_validos <- setdiff(names(series_lista), series_ignoradas$tipo)
  
  # ======================================================
  # 2) MODELO GLOBAL
  # ======================================================
  modelo_global <- NULL
  if (modo == "global") {
    global <- eleccion_modelo_global(tablas_por_tipo)
    modelo_global <- global$modelo
  }
  
  # ======================================================
  # 3) APLICAR EL MODELO Y GENERAR SALIDAS
  # ======================================================
  salida <- list()
  
  if (modo == "global") {
    salida[["__cabecera__"]] <- htmltools::HTML(
      sprintf("<div style='font-size:24px; font-weight:600; margin-bottom:20px;'> Modo GLOBAL — Modelo seleccionado: <code>%s</code></div>", modelo_global)
    )
  } else {
    salida[["__cabecera__"]] <- htmltools::HTML("<div style='font-size:24px; font-weight:600; margin-bottom:20px;'> Modo INDIVIDUAL — Cada tipo usa su mejor modelo</div>")
  }
  
  for (t in tipos_validos) {
    AC <- series_lista[[t]] %>% dplyr::rename(pacientes = valor)
    
    z    <- zoo(AC$pacientes, order.by = AC$fecha)
    z_tr <- window(z, end = fecha_intervencion)
    malla_futura <- index(z)[index(z) > fecha_intervencion]
    
    ts_tot <- ts(coredata(z), frequency = 52)
    ts_tr  <- ts(coredata(z_tr), frequency = 52)
    h      <- length(malla_futura)
    
    lista_modelos <- resultados_por_tipo[[t]]
    
    if (modo == "global") {
      idx <- which(sapply(lista_modelos, `[[`, "name") == modelo_global)
      if (length(idx) == 1) {
        modelo <- lista_modelos[[idx]]
      } else {
        modelo <- switch(modelo_global,
                         "stl_ets"       = stl_ets(ts_tr, AC, fecha_intervencion, z_tr),
                         "stl_arima"     = stl_arima(ts_tr, AC, fecha_intervencion, z_tr),
                         "prophet"       = prophet_model(AC, ts_tot, ts_tr, z_tr),
                         "arima_fourier" = arima_fourier(ts_tr, z_tr, h))
      }
      modelo_usado <- modelo_global
    } else {
      tabla_local <- tablas_por_tipo[[t]]$tabla
      mejor_local <- tabla_local$modelo[which.min(tabla_local$score)]
      idx <- which(sapply(lista_modelos, `[[`, "name") == mejor_local)
      modelo <- lista_modelos[[idx]]
      modelo_usado <- mejor_local
    }
    
    # ------- Preparación de Predicciones e IC95 -------
    fit_tr_z  <- modelo$fitted_z
    fc_mean_z <- modelo$fc_mean_z
    
    if (!is.null(modelo$fc_mean_z) && length(modelo$fc_mean_z) > 0) {
      fc_vals  <- as.numeric(coredata(modelo$fc_mean_z))
      low_vals <- as.numeric(coredata(modelo$fc_lower_z))
      upp_vals <- as.numeric(coredata(modelo$fc_upper_z))
      
      limite    <- min(length(fc_vals), length(malla_futura))
      fc_mean_z  <- zoo(fc_vals[seq_len(limite)],  order.by = malla_futura[seq_len(limite)])
      fc_lower_z <- zoo(low_vals[seq_len(limite)], order.by = malla_futura[seq_len(limite)])
      fc_upper_z <- zoo(upp_vals[seq_len(limite)], order.by = malla_futura[seq_len(limite)])
      
      df_fc <- data.frame(
        fecha = index(fc_mean_z),
        pred  = as.numeric(fc_mean_z),
        lower = as.numeric(fc_lower_z),
        upper = as.numeric(fc_upper_z)
      )
    } else {
      fc_mean_z  <- zoo(numeric(0), order.by = as.Date(character(0)))
      fc_lower_z <- zoo(numeric(0), order.by = as.Date(character(0)))
      fc_upper_z <- zoo(numeric(0), order.by = as.Date(character(0)))
      df_fc      <- data.frame()
    }
    
    # ------- Gráfico con Sombreado de Ribbons -------
    # Ajuste de desfase para la media móvil en el histórico si corresponde
    eje_x_fit <- if (identical(t, "Media móvil")) index(fit_tr_z) + 21 else index(fit_tr_z)
    
    p <- ggplot() +
      geom_line(aes(x = index(z), y = coredata(z)), colour = "grey60") +
      geom_line(aes(x = eje_x_fit, y = coredata(fit_tr_z)), colour = "#83D0BB") +
      geom_vline(xintercept = as.numeric(fecha_intervencion), colour = "red") +
      labs(title = paste("Modelo aplicado:", modelo_usado), x = "Fecha", y = "Valor") +
      theme_minimal()
    
    if (nrow(df_fc) > 0) {
      p <- p + 
        geom_ribbon(data = df_fc, aes(x = fecha, ymin = lower, ymax = upper), fill = "#83D0BB", alpha = 0.2) +
        geom_line(data = df_fc, aes(x = fecha, y = pred), colour = "#83D0BB", linetype = "dashed")
    }
    
    bloque <- list(
      titulo  = htmltools::HTML(paste0("<h2>", t, "</h2>")),
      grafico = plotly::as_widget(ggplotly(p))
    )
    
    # ------- Filtrado e Inserción en Tablas -------
    obs <- z[index(z) >= inicio_observacion & index(z) <= fin_observacion]
    
    if (length(fc_mean_z) > 0) {
      fc       <- fc_mean_z[index(fc_mean_z) >= inicio_observacion & index(fc_mean_z) <= fin_observacion]
      fc_lower <- fc_lower_z[index(fc_lower_z) >= inicio_observacion & index(fc_lower_z) <= fin_observacion]
      fc_upper <- fc_upper_z[index(fc_upper_z) >= inicio_observacion & index(fc_upper_z) <= fin_observacion]
    } else {
      fc       <- zoo::zoo(numeric(0), order.by = as.Date(character(0)))
      fc_lower <- zoo::zoo(numeric(0), order.by = as.Date(character(0)))
      fc_upper <- zoo::zoo(numeric(0), order.by = as.Date(character(0)))
    }
    
    # >>> CAMBIO AQUÍ: Ahora se pasan fc_lower y fc_upper a la tabla de resultados
    res_tablas          <- tabla_resultados(obs, fc, fc_lower, fc_upper)
    bloque$tabla_global <- res_tablas$tabla_global
    bloque$tabla_anual  <- res_tablas$tabla_anual
    
    salida[[t]] <- bloque
  }
  
  if (nrow(series_ignoradas) > 0) {
    aviso <- div(
      class = "alert alert-warning",
      h4("⚠️ Fármacos ignorados"),
      p("Las siguientes series no se han analizado:"),
      tags$ul(
        lapply(seq_len(nrow(series_ignoradas)), function(i) {
          tags$li(paste0(series_ignoradas$tipo[i], " — ", series_ignoradas$motivo[i]))
        })
      )
    )
    salida <- c(list(aviso), salida)
  }
  
  do.call(htmltools::tagList, salida)
}


# ====================================== ANALISIS SEGMENTADO ====================================== 
# 
# analisis_sensibilidad_por_farmaco <- function(datos, fecha_intervencion, K_fourier = 2) {
#   library(nlme)
#   library(forecast)
#   library(dplyr)
#   library(tibble)
#   library(htmltools)
#   library(knitr)
#   library(kableExtra)
#   
#   # Asegurar formato de fecha y ordenamiento
#   datos <- datos %>% 
#     mutate(fecha = as.Date(fecha)) %>% 
#     arrange(fecha)
#   
#   tipos <- unique(datos$tipo)
#   resultados_completos <- list()
#   tablas_html_salida  <- list()
#   
#   # Cabecera informativa para el reporte HTML
#   tablas_html_salida[["__cabecera__"]] <- htmltools::HTML(
#     "<div style='font-size:24px; font-weight:600; margin-bottom:20px;'>⚠️ Análisis de Sensibilidad: Regresión Segmentada con Corrección AR(1) y Estacionalidad</div>"
#   )
#   
#   # Bucle para recorrer automáticamente cada fármaco / tipo
#   for (t in tipos) {
#     
#     # 1. Filtrar los datos del fármaco actual
#     df_farmaco <- datos %>% 
#       filter(tipo == t) %>% 
#       mutate(tiempo = row_number())
#     
#     # Validar que la serie sea apta para el análisis (mínimo de datos)
#     if (nrow(df_farmaco) < 10) next
#     
#     # 2. Crear las variables estructurales de la regresión segmentada (ITS)
#     df_farmaco <- df_farmaco %>% 
#       mutate(
#         intervencion = ifelse(fecha >= as.Date(fecha_intervencion), 1, 0),
#         tiempo_post  = ifelse(fecha >= as.Date(fecha_intervencion), 
#                               tiempo - min(tiempo[fecha >= as.Date(fecha_intervencion)]) + 1, 
#                               0)
#       )
#     
#     # 3. Generar armónicos de Fourier para controlar la estacionalidad (Frecuencia 52 semanas)
#     X_fourier <- forecast::fourier(ts(df_farmaco$pacientes, frequency = 52), K = K_fourier)
#     df_farmaco    <- cbind(df_farmaco, X_fourier)
#     
#     # Construir la fórmula dinámica incluyendo los S1, C1, S2, C2 correspondientes
#     nombres_fourier <- colnames(X_fourier)
#     formula_str     <- paste("pacientes ~ tiempo + intervencion + tiempo_post +", 
#                              paste(nombres_fourier, collapse = " + "))
#     formula_modelo  <- as.formula(formula_str)
#     
#     # 4. Ajustar modelo GLS con estructura AR(1) para corregir autocorrelación
#     modelo_gls <- tryCatch({
#       gls(
#         model       = formula_modelo, 
#         data        = df_farmaco, 
#         correlation = corAR1(form = ~ tiempo), 
#         method      = "ML"
#       )
#     }, error = function(e) {
#       # Si falla la convergencia por AR(1) en algún fármaco, cae a un modelo lineal estándar (OLS)
#       lm(formula_modelo, data = df_farmaco)
#     })
#     
#     # 5. Extraer y formatear la tabla de coeficientes
#     resumen_coef <- summary(modelo_gls)$tTable %>% 
#       as.data.frame() %>% 
#       tibble::rownames_to_column(var = "Efecto")
#     
#     # Filtrar solo los efectos principales para no saturar al revisor con los términos de Fourier
#     significado <- c(
#       "(Intercept)"  = "Nivel basal inicial",
#       "tiempo"       = "Tendencia pre-intervención (pendiente)",
#       "intervencion" = "Cambio inmediato de NIVEL",
#       "tiempo_post"  = "Cambio en la PENDIENTE post-intervención"
#     )
#     
#     tabla_farmaco <- resumen_coef %>% 
#       filter(Efecto %in% names(significado)) %>% 
#       mutate(
#         Variable       = t,
#         Interpretacion = significado[Efecto],
#         Value          = round(Value, 4),
#         Std.Error      = round(Std.Error, 4),
#         `t-value`      = round(`t-value`, 2),
#         `p-value`      = round(`p-value`, 4)
#       ) %>% 
#       select(Variable, Efecto, Interpretacion, Value, Std.Error, `t-value`, `p-value`)
#     
#     # Almacenar los datos crudos por si los necesitas
#     resultados_completos[[t]] <- tabla_farmaco
#     
#     # 6. Crear la tabla en HTML elegante formateada para el reporte de RMarkdown
#     html_tabla <- tabla_farmaco %>% 
#       select(-Variable) %>% 
#       mutate(
#         Value     = format(Value, decimal.mark = ","),
#         Std.Error = format(Std.Error, decimal.mark = ","),
#         `t-value` = format(`t-value`, decimal.mark = ","),
#         `p-value` = ifelse(`p-value` < 0.001, "< 0,001", format(`p-value`, decimal.mark = ","))
#       ) %>% 
#       kable("html", caption = paste("Regresión Segmentada - Fármaco:", t)) %>% 
#       kable_styling(full_width = FALSE, position = "left") %>% 
#       column_spec(1:6, extra_css = "text-align: center;") %>% 
#       as.character() %>% 
#       htmltools::HTML()
#     
#     tablas_html_salida[[t]] <- list(
#       titulo = htmltools::HTML(paste0("<h2>Resultados de Sensibilidad: ", t, "</h2>")),
#       tabla  = html_tabla
#     )
#   }
#   
#   # Desenrollar la lista para que RMarkdown la pinte directamente en el visor/informe
#   elementos_salida <- list(tablas_html_salida[["__cabecera__"]])
#   for(t in names(resultados_completos)) {
#     elementos_salida <- c(elementos_salida, list(tablas_html_salida[[t]]$titulo, tablas_html_salida[[t]]$tabla))
#   }
#   
#   return(do.call(htmltools::tagList, elementos_salida))
# }
# 
# analisis_sensibilidad_consola <- function(datos, fecha_intervencion, K_fourier = 2) {
#   library(nlme)
#   library(forecast)
#   library(dplyr)
#   library(tibble)
#   
#   datos <- datos %>% mutate(fecha = as.Date(fecha)) %>% arrange(fecha)
#   tipos <- unique(datos$tipo)
#   
#   cat("\n========================================================================\n")
#   cat("   REPORTE DE SENSIBILIDAD: REGRESIÓN SEGMENTADA CON CORRECCIÓN AR(1)   \n")
#   cat("========================================================================\n")
#   
#   for (t in tipos) {
#     df_farmaco <- datos %>% filter(tipo == t) %>% mutate(tiempo = row_number())
#     if (nrow(df_farmaco) < 10) next
#     
#     # Variables estructurales de la regresión segmentada (ITS)
#     df_farmaco <- df_farmaco %>% mutate(
#       intervencion = ifelse(fecha >= as.Date(fecha_intervencion), 1, 0),
#       tiempo_post  = ifelse(fecha >= as.Date(fecha_intervencion), 
#                             tiempo - min(tiempo[fecha >= as.Date(fecha_intervencion)]) + 1, 0)
#     )
#     
#     # Control de estacionalidad estricta usando Fourier (Frecuencia 52)
#     X_fourier <- forecast::fourier(ts(df_farmaco$pacientes, frequency = 52), K = K_fourier)
#     
#     # CORRECCIÓN CLAVE: Sustituir guiones y caracteres especiales en los nombres de Fourier
#     colnames(X_fourier) <- gsub("[^a-zA-Z0-9]", "_", colnames(X_fourier))
#     
#     df_farmaco <- cbind(df_farmaco, X_fourier)
#     
#     # Construcción limpia y segura de la fórmula
#     nombres_fourier <- colnames(X_fourier)
#     formula_str <- paste("pacientes ~ tiempo + intervencion + tiempo_post +", 
#                          paste(nombres_fourier, collapse = " + "))
#     
#     formula_limpia <- as.formula(formula_str)
#     
#     # Ajuste robusto del modelo estadístico
#     modelo_gls <- tryCatch({
#       gls(model = formula_limpia, data = df_farmaco, 
#           correlation = corAR1(form = ~ tiempo), method = "ML")
#     }, error = function(e) {
#       # Retorno seguro a estimador lineal estándar si falla la convergencia de la covarianza
#       lm(formula_limpia, data = df_farmaco)
#     })
#     
#     # Extracción y estructuración de los parámetros esenciales
#     resumen_coef <- summary(modelo_gls)$tTable %>% 
#       as.data.frame() %>% 
#       tibble::rownames_to_column(var = "Efecto")
#     
#     significado <- c(
#       "(Intercept)"  = "Nivel inicial", 
#       "tiempo"       = "Tendencia PRE ", 
#       "intervencion" = "Cambio NIVEL  ", 
#       "tiempo_post"  = "Cambio PENDIE "
#     )
#     
#     tabla_consola <- resumen_coef %>% 
#       filter(Efecto %in% names(significado)) %>% 
#       mutate(
#         Efecto = significado[Efecto], 
#         Estimacion = round(Value, 4), 
#         Std.Error = round(Std.Error, 4), 
#         `p-value` = round(`p-value`, 4)
#       ) %>% 
#       select(Efecto, Estimacion, Std.Error, `p-value`)
#     
#     cat(paste0("\n>>> FÁRMACO / SERIE: ", t, "\n"))
#     print(tabla_consola, row.names = FALSE)
#     cat("------------------------------------------------------------------------\n")
#   }
# }

