suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(lubridate)
})

trend_path <- "\\\\Sfp.idir.bcgov\\s143\\S86501\\PTBoard\\Initiatives\\Data Modelling & Policy Initiative\\Data Economic Modelling Projects\\2025_011_Trend_Analysis\\trend_results_2025_1205.xlsx"

trend_raw <- read_excel(
  trend_path,
  col_types = c(
    "text",    # LEVEL_NAME
    "text",    # PICKUP_AREA
    "text",    # TRIP_SERVICE_TYPE_CODE
    "text",    # INDICATOR_TYPE
    "numeric", # YEAR_POINT
    "numeric", # MONTH_POINT
    "date",    # period
    "numeric", # INDICATOR_VALUE
    "text",    # MODEL_TYPE
    "text",    # TREND_DIRECTION
    "numeric", # SLOPE_EST
    "numeric", # SLOPE_P
    "text",    # SHIFT_DIRECTION
    "numeric", # SHIFT_EST
    "numeric", # SHIFT_P
    "numeric"  # K_RECENT

  )
) %>%
  mutate(
    date = as.Date(period)  # convenience; not strictly required
  )


trend_lookup_flags <- function(indicator,
                               service   = "TAXI",
                               area_type = "REGIONAL",
                               model_type = "RECENT_1M_SHIFT",
                               region    = region_name,
                               end_date  = date_to) {
  yr <- year(end_date)
  mo <- month(end_date)
  
  df <- trend_raw %>%
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






trend_direction_metric <- function(indicator,
                                   service   = "TAXI",
                                   area_type = "REGIONAL") {
  trend_lookup_flags(
    indicator = indicator,
    service   = service,
    area_type = area_type
  )$trend_direction
}

shift_direction_metric <- function(indicator,
                                   service   = "TAXI",
                                   area_type = "REGIONAL") {
  trend_lookup_flags(
    indicator = indicator,
    service   = service,
    area_type = area_type
  )$shift_direction
}







