# ===== Setup =====
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(stringr)
  library(ggplot2)
  library(scales)
  library(flextable)
  library(officer)
})

# ----- Parameters -----
date_from   <- make_date(2023, 1, 1)
date_to     <- make_date(2024, 3, 1)
region_name <- "Metro Vancouver"

# Derived labels
end_label  <- format(date_to, "%B %Y")
prev_label <- format(date_to %m-% years(1), "%B %Y")
prev_year  <- year(date_to) - 1

# ===== Data Load + Minimal Shaping =====
raw <- read_excel("\\\\Sfp.idir.bcgov\\S143\\S86501\\PTBoard\\Economics\\Datahub\\3_indicators.xlsx")

raw_norm <- raw %>%
  transmute(
    date     = make_date(as.integer(YEAR_POINT), as.integer(MONTH_POINT), 1),
    area     = PICKUP_AREA,
    area_type = GEOSPATIAL_AREA_TYPE_CODE,
    indicator = INDICATOR_TYPE,
    service   = TRIP_SERVICE_TYPE_CODE,
    value     = suppressWarnings(as.numeric(INDICATOR_VALUE))
  ) %>%
  filter(!is.na(date), !is.na(value))

windowed <- raw_norm %>% filter(between(date, date_from, date_to))
