trend_raw <- read_excel(
  trend_path,
  col_types = c(
    "text",    # LEVEL_NAME
    "text",    # PICKUP_AREA
    "text",    # TRIP_SERVICE_TYPE_CODE
    "text",    # INDICATOR_TYPE
    "date",    # period
    "numeric", # YEAR_POINT
    "numeric", # MONTH_POINT
    "numeric", # INDICATOR_VALUE
    "numeric", # INDICATOR_OBSERVATION
    "text",    # MODEL_TYPE
    "text",    # TREND_DIRECTION
    "numeric", # ESTIMATED_ANNUAL_CHANGE_PCT
    "numeric", # SLOPE_EST
    "numeric", # SLOPE_P
    "text",    # SHIFT_DIRECTION
    "numeric", # ESTIMATED_SHIFT_PCT
    "numeric", # SHIFT_EST
    "numeric"  # SHIFT_P
  )
) |>
  mutate(
    date = as.Date(period)
  )


# ------------------------------------------------------------
# Core lookup: returns both flags for the report month
# ------------------------------------------------------------
model_override <- c(
  "FLEET_UTILIZATION" = "LOGIT_ROLL_BINOMIAL",
  "VEHICLE_OCCUPANCY_RATE" = "LOGIT_ROLL_BINOMIAL"
)

trend_lookup_flags <- function(indicator,
                               service    = "TAXI",
                               area_type  = "REGIONAL",
                               model_type = "RECENT_1M_SHIFT_LOG",
                               region     = region_name,
                               end_date   = date_to) {
  
  if (indicator %in% names(model_override)) {
    model_type <- unname(model_override[[indicator]])
  }
  
  yr <- year(end_date)
  mo <- month(end_date)
  
  df <- trend_raw |>
    filter(
      LEVEL_NAME             == area_type,
      PICKUP_AREA            == region,
      TRIP_SERVICE_TYPE_CODE == service,
      INDICATOR_TYPE         == indicator,
      YEAR_POINT             == yr,
      MONTH_POINT            == mo,
      MODEL_TYPE             == model_type
    )
  
  if (nrow(df) == 0) {
    return(list(
      trend_direction = NA_character_,
      shift_direction = NA_character_
    ))
  }
  
  row <- dplyr::slice_tail(df, n = 1)
  
  list(
    trend_direction = row$TREND_DIRECTION[[1]],
    shift_direction = row$SHIFT_DIRECTION[[1]]
  )
}

# ------------------------------------------------------------
# Single accessor in the same style as metric()
# ------------------------------------------------------------
trend_metric <- function(indicator,
                         service   = "TAXI",
                         type      = c("trend_direction", "shift_direction"),
                         area_type = "REGIONAL") {
  type  <- match.arg(type)
  flags <- trend_lookup_flags(
    indicator = indicator,
    service   = service,
    area_type = area_type
  )
  flags[[type]]
}

# Optional convenience wrappers (you can keep or delete)
trend_direction_metric <- function(indicator,
                                   service   = "TAXI",
                                   area_type = "REGIONAL") {
  trend_metric(indicator, service, "trend_direction", area_type)
}

shift_direction_metric <- function(indicator,
                                   service   = "TAXI",
                                   area_type = "REGIONAL") {
  trend_metric(indicator, service, "shift_direction", area_type)
}

# ------------------------------------------------------------
# Normalise codes coming out of Excel (fix typos)
# ------------------------------------------------------------
normalize_trend_code <- function(x) {
  x <- toupper(trimws(x))
  gsub("TRNED", "TREND", x, fixed = TRUE)
}

normalize_shift_code <- function(x) {
  x <- toupper(trimws(x))
  gsub("SHFIT", "SHIFT", x, fixed = TRUE)
}

# ------------------------------------------------------------
# Map codes -> sentences
# ------------------------------------------------------------
trend_direction_text <- function(code, subject = "this indicator") {
  if (is.na(code) || code == "") return(NA_character_)
  code <- normalize_trend_code(code)
  
  if (code == "NO_SIGNIFICANT_TREND") {
    paste0(
      "Looking over the past two years, Statistical analysis suggests no meaningful long-term trend."
    )
  } else if (code == "TREND DOWN") {
    paste0(
      "Looking over the past two years, Statistical analysis suggests a long-term downward trend."
    )
  } else if (code == "TREND UP") {
    paste0(
      "Looking over the past two years, Statistical analysis suggests a long-term upward trend."
    )
  } else {
    NA_character_
  }
}

shift_direction_text <- function(code) {
  if (is.na(code) || code == "") return(NA_character_)
  code <- normalize_shift_code(code)
  
  if (code == "NO_SIGNIFICANT_SHIFT") {
    "Statistical analysis of the short-term trend suggests no active shift in market direction."
  } else if (code == "SHIFT DOWN") {
    "Statistical analysis of the short-term trend suggests a downward shift in market direction."
  } else if (code == "SHIFT UP") {
    "Statistical analysis of the short-term trend suggests an upward shift in market direction."
  } else {
    NA_character_
  }
}

# ------------------------------------------------------------
# Narratives that you call in chunks
# ------------------------------------------------------------
trend_narrative <- function(indicator,
                            service = "TAXI",
                            subject = "total trip revenue") {
  code <- trend_metric(indicator, service, "trend_direction")
  trend_direction_text(code, subject = subject)
}

shift_narrative <- function(indicator,
                            service = "TAXI") {
  code <- trend_metric(indicator, service, "shift_direction")
  shift_direction_text(code)
}
