# ------------------------------------------------------------
# 0) Optional: indicator aliases (backward compatible)
#    If you previously had different names, map them here.
# ------------------------------------------------------------
indicator_alias <- c(
  # Old -> New (observed in the new file)
  "FLEET_UTILIZATION" = "FLEET_UTILIZATION_RATE"
)

resolve_indicator <- function(indicator) {
  if (indicator %in% names(indicator_alias)) unname(indicator_alias[[indicator]]) else indicator
}

# ------------------------------------------------------------
# 1) Read the Excel output + build date
# ------------------------------------------------------------
read_trend_output <- function(trend_path) {
  
  trend_raw <- read_excel(
    trend_path,
    col_types = c(
      "text",    # REPORTING_LEVEL
      "text",    # REPORTING_NAME
      "text",    # PICKUP_AREA
      "text",    # SERVICE_TYPE
      "text",    # INDICATOR_NAME
      "numeric", # YEAR
      "numeric", # MONTH
      "text",    # MODEL_TYPE
      "text",    # TREND_DIRECTION
      "text",    # SHIFT_DIRECTION
      "numeric", # avg_monthly_change_pct
      "numeric", # slope_estimate
      "numeric", # slope_p_value
      "numeric", # shift_estimate
      "numeric", # shift_p_value
      "numeric"  # OBSERVATION_COUNT
    )
  ) |>
    mutate(
      YEAR  = as.integer(YEAR),
      MONTH = as.integer(MONTH),
      date  = as.Date(sprintf("%04d-%02d-01", YEAR, MONTH))
    )
  
  trend_raw
}

trend_raw <- read_trend_output(trend_path)


# ------------------------------------------------------------
# 2) Normalizer (no typo-fix; just clean strings)
# ------------------------------------------------------------
normalize_dir <- function(x) {
  if (is.na(x)) return(NA_character_)
  x <- tolower(trimws(x))
  gsub("\\s+", " ", x)
}

# ------------------------------------------------------------
# 3) Map codes -> narrative sentences
#    (handles: "trend up/down", "no trend", "not conclusive", etc.)
# ------------------------------------------------------------
trend_direction_text <- function(code) {
  code <- normalize_dir(code)
  if (is.na(code) || code == "") return(NA_character_)
  
  if (code %in% c("no trend", "no significant trend", "no_significant_trend")) {
    "Looking over the past two years, statistical analysis of the long-term trend suggests that this indicator has remained stable."
  } else if (code %in% c("trend down", "trend_down")) {
    "Looking over the past two years, statistical analysis of the long-term trend suggests that this indicator is on a downward trajectory."
  } else if (code %in% c("trend up", "trend_up")) {
    "Looking over the past two years, statistical analysis of the long-term trend suggests that this indicator is on an upward trajectory."
  } else if (code %in% c("not conclusive", "inconclusive")) {
    "Looking over the past two years, results are not conclusive regarding a long-term trend."
  } else {
    NA_character_
  }
}

shift_direction_text <- function(code) {
  code <- normalize_dir(code)
  if (is.na(code) || code == "") return(NA_character_)
  
  if (code %in% c("no shift", "no significant shift", "no_significant_shift")) {
    "Statistical analysis of the short-term trend suggests that there is no active shift in market direction."
  } else if (code %in% c("shift down", "shift_down")) {
    "Statistical analysis of the short-term trend suggests that there is a downward shift in market direction."
  } else if (code %in% c("shift up", "shift_up")) {
    "Statistical analysis of the short-term trend suggests that there is an upward shift in market direction."
  } else if (code %in% c("not conclusive", "inconclusive")) {
    "Short-term results are not conclusive regarding a shift in market direction."
  } else {
    NA_character_
  }
}



# ------------------------------------------------------------
# 4) Core lookup: trend + shift for report month
#    Trend model: LONG_TERM_TREND
#    Shift model: RECENT_K_SHIFT (your requirement)
# ------------------------------------------------------------
trend_lookup_flags <- function(trend_raw,
                               indicator,
                               service     = "TAXI",
                               area_type   = "REGIONAL",
                               region      = region_name,
                               end_date    = date_to,
                               trend_model = "LONG_TERM_TREND",
                               shift_model = "RECENT_K_SHIFT") {
  
  indicator <- resolve_indicator(indicator)
  
  yr <- year(end_date)
  mo <- month(end_date)
  
  base <- trend_raw |>
    filter(
      REPORTING_LEVEL == area_type,
      REPORTING_NAME  == region,
      SERVICE_TYPE    == service,
      INDICATOR_NAME  == indicator,
      YEAR            == yr,
      MONTH           == mo
    )
  
  # Long-term trend
  df_trend <- base |> filter(MODEL_TYPE == trend_model)
  trend_dir <- if (nrow(df_trend) == 0) NA_character_
  else dplyr::slice_tail(df_trend, n = 1)$TREND_DIRECTION[[1]]
  
  # Short-term shift (RECENT_K_SHIFT)
  df_shift <- base |> filter(MODEL_TYPE == shift_model)
  shift_dir <- if (nrow(df_shift) == 0) NA_character_
  else dplyr::slice_tail(df_shift, n = 1)$SHIFT_DIRECTION[[1]]
  
  list(
    trend_direction = trend_dir,
    shift_direction = shift_dir
  )
}

# ------------------------------------------------------------
# 5) Accessor
# ------------------------------------------------------------
trend_metric <- function(trend_raw,
                         indicator,
                         service   = "TAXI",
                         type      = c("trend_direction", "shift_direction"),
                         area_type = "REGIONAL",
                         region    = region_name,
                         end_date  = date_to) {
  
  type <- match.arg(type)
  
  flags <- trend_lookup_flags(
    trend_raw  = trend_raw,
    indicator  = indicator,
    service    = service,
    area_type  = area_type,
    region     = region,
    end_date   = end_date
  )
  
  flags[[type]]
}

# ------------------------------------------------------------
# 6) Narratives (Rmd-friendly)
#    Assumes trend_raw, region_name, date_to exist in env.
# ------------------------------------------------------------
trend_narrative <- function(indicator,
                            service   = "TAXI",
                            area_type = "REGIONAL",
                            region    = region_name,
                            end_date  = date_to) {
  
  code <- trend_metric(
    trend_raw  = trend_raw,
    indicator  = indicator,
    service    = service,
    type       = "trend_direction",
    area_type  = area_type,
    region     = region,
    end_date   = end_date
  )
  
  trend_direction_text(code)
}

shift_narrative <- function(indicator,
                            service   = "TAXI",
                            area_type = "REGIONAL",
                            region    = region_name,
                            end_date  = date_to) {
  
  code <- trend_metric(
    trend_raw  = trend_raw,
    indicator  = indicator,
    service    = service,
    type       = "shift_direction",
    area_type  = area_type,
    region     = region,
    end_date   = end_date
  )
  
  shift_direction_text(code)
}

# ------------------------------------------------------------
# 7) Quick helpers (optional)
# ------------------------------------------------------------
list_indicators <- function(trend_raw) sort(unique(trend_raw$INDICATOR_NAME))
list_models     <- function(trend_raw) sort(unique(trend_raw$MODEL_TYPE))
