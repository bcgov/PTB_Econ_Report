
# ============================================================

# Setup

# ============================================================

knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

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

# -----------------------------

# Parameters (edit as needed)

# -----------------------------

date_from   <- make_date(2023, 1, 1)     # report window start (inclusive)
date_to     <- make_date(2024, 3, 1)     # report window end   (inclusive)
region_name <- "Metro Vancouver"          # display name used across report

# Derived labels for narrative text

end_label  <- format(date_to, "%B %Y")
prev_label <- format(date_to %m-% years(1), "%B %Y")
prev_year  <- year(date_to) - 1


# ============================================================

# Data Load + Minimal Shaping

# ============================================================

# NOTE: keep raw as-is; do minimal typing so downstream functions are predictable.

raw <- read_excel("\\\\Sfp.idir.bcgov\\S143\\S86501\\PTBoard\\Economics\\Datahub\\3_indicators.xlsx")

# Basic normalized view with a proper Date column and numeric values.

# We DO NOT filter by indicator/region/service here; do that in helpers.

raw_norm <- raw %>%
  transmute(
    date   = make_date(as.integer(YEAR_POINT), as.integer(MONTH_POINT), 1),
    area   = PICKUP_AREA,
    area_type = GEOSPATIAL_AREA_TYPE_CODE,
    indicator = INDICATOR_TYPE,
    service   = TRIP_SERVICE_TYPE_CODE,
    value     = suppressWarnings(as.numeric(INDICATOR_VALUE))
  ) %>%
  filter(!is.na(date), !is.na(value))

# Restrict to reporting window for “windowed” analyses; we’ll still compute YTD from full data when needed.

windowed <- raw_norm %>%
  filter(between(date, date_from, date_to))

```



# ============================================================

# Helpers (single source of truth for math/formatting)

# ============================================================

# 1) Region aliasing: treat "Metro Vancouver Regional District" and "Metro Vancouver" as the same.

with_region_alias <- function(region) {
  if (identical(region, "Metro Vancouver Regional District")) {
    c(region, "Metro Vancouver")
  } else if (identical(region, "Metro Vancouver")) {
    c("Metro Vancouver", "Metro Vancouver Regional District")
  } else {
    region
  }
}

# 2) Formatter utilities (consistent across the doc)

fmt_pct <- function(x, digits = 2) {
  if (is.na(x)) return("n/a")
  paste0(ifelse(x >= 0, "+", ""), format(round(x, digits), nsmall = digits), "%")
}

fmt_num <- function(x, digits = 0, currency = FALSE) {
  if (is.na(x)) return("n/a")
  base <- format(round(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE)
  if (currency) paste0("$", base) else base
}

# 3) Core filter (use everywhere to keep logic consistent)

filter_indicator <- function(data,
                             indicator,
                             region,
                             services  = c("TAXI","TNS"),
                             area_type = "REGIONAL",
                             date_from = NULL,
                             date_to   = NULL) {
  region_vec <- with_region_alias(region)
  
  out <- data %>%
    filter(
      area_type == !!area_type,
      area %in% region_vec,
      indicator == !!indicator,
      service %in% services
    )
  
  if (!is.null(date_from)) out <- out %>% filter(date >= date_from)
  if (!is.null(date_to))   out <- out %>% filter(date <= date_to)
  
  out
}

# 4) Safe last-month extraction (sums across selected services; returns NA if prev/denominator missing)

last_month_value <- function(data, indicator, region, services = c("TAXI","TNS"), area_type = "REGIONAL",
                             date_from = NULL, date_to = NULL) {
  df <- filter_indicator(data, indicator, region, services, area_type, date_from, date_to)
  if (nrow(df) == 0) return(NA_real_)
  last_date <- max(df$date, na.rm = TRUE)
  df %>% filter(date == last_date) %>% summarise(v = sum(value, na.rm = TRUE)) %>% pull(v)
}

last_month_yoy <- function(data, indicator, region, services = c("TAXI","TNS"), area_type = "REGIONAL",
                           date_from = NULL, date_to = NULL) {
  df <- filter_indicator(data, indicator, region, services, area_type, date_from, date_to)
  if (nrow(df) == 0) return(NA_real_)
  last_date <- max(df$date, na.rm = TRUE)
  prev_date <- last_date %m-% years(1)
  
  cur  <- df %>% filter(date == last_date) %>% summarise(v = sum(value, na.rm = TRUE)) %>% pull(v)
  prev <- df %>% filter(date == prev_date) %>% summarise(v = sum(value, na.rm = TRUE)) %>% pull(v)
  
  if (is.na(prev) || prev == 0) return(NA_real_)
  (cur/prev - 1) * 100
}

# 5) YTD YoY through a specific end month (uses *full* data so we can sum Jan..end_month of both years)

ytd_yoy <- function(data, indicator, region, end_date,
                    services = c("TAXI","TNS"), area_type = "REGIONAL") {
  df <- filter_indicator(data, indicator, region, services, area_type) # no windowing here
  if (nrow(df) == 0) return(NA_real_)
  
  y_end <- year(end_date); m_end <- month(end_date)
  
  cur  <- df %>% filter(year(date) == y_end,     month(date) <= m_end) %>% summarise(v = sum(value, na.rm = TRUE)) %>% pull(v)
  prev <- df %>% filter(year(date) == y_end - 1, month(date) <= m_end) %>% summarise(v = sum(value, na.rm = TRUE)) %>% pull(v)
  
  if (is.na(prev) || prev == 0) return(NA_real_)
  (cur/prev - 1) * 100
}

# 6) Compact monthly table (Taxi/TNS) with service headers; accepts window explicitly

make_indicator_table_compact <- function(
    data, indicator, region, services = c("TAXI","TNS"), area_type = "REGIONAL",
    date_from = NULL, date_to = NULL, digits = 0, zero_to_blank = FALSE,
    currency = NULL, font_size = 9
) {
  
  # auto-detect currency style if not set
  
  if (is.null(currency)) currency <- str_detect(indicator, regex("REVENUE|FARE", ignore_case = TRUE))
  
  df <- filter_indicator(data, indicator, region, services, area_type, date_from, date_to) %>%
    transmute(service, y = year(date), m = month(date), value)
  
  if (nrow(df) == 0) stop("No rows matched your filters.")
  
  # ensure all months exist per service/year
  
  dfc <- df %>%
    group_by(service, y) %>%
    complete(m = 1:12, fill = list(value = NA_real_)) %>%
    ungroup() %>%
    mutate(m_lab = month(m, label = TRUE, abbr = TRUE))
  
  month_cols <- as.character(month(1:12, label = TRUE, abbr = TRUE))
  
  wide <- dfc %>%
    select(service, y, m_lab, value) %>%
    pivot_wider(names_from = m_lab, values_from = value) %>%
    arrange(match(service, services), y)
  
  # optional blank zeros to reduce noise
  
  if (isTRUE(zero_to_blank)) {
    wide <- wide %>% mutate(across(all_of(month_cols), ~ ifelse(.x == 0, NA_real_, .x)))
  }
  
  # insert service header rows
  
  add_headers <- function(w, services) {
    out <- list()
    for (srv in services) {
      block <- w %>% filter(service == srv)
      if (nrow(block) == 0) next
      header <- tibble(service = srv, y = NA_integer_)
      for (m in month_cols) header[[m]] <- NA_real_
      out[[srv]] <- bind_rows(header, block)
    }
    bind_rows(out)
  }
  
  wide2 <- add_headers(wide, services)
  out   <- wide2 %>% mutate(Year = ifelse(is.na(y), service, format(y, big.mark = ","))) %>%
    select(Year, all_of(month_cols))
  
  ft <- flextable(out)
  
  # style service header rows
  
  hdr_idx <- which(is.na(wide2$y))
  if (length(hdr_idx)) {
    for (i in hdr_idx) ft <- merge_at(ft, i = i, j = 1:ncol(out))
    ft <- bold(ft, i = hdr_idx, bold = TRUE)
    ft <- align(ft, i = hdr_idx, align = "left")
    ft <- bg(ft, i = hdr_idx, bg = "#d9ead3")
  }
  
  # zebra for year rows
  
  yr_idx <- setdiff(seq_len(nrow(out)), hdr_idx)
  ft <- bg(ft, i = yr_idx, bg = "#ffffff")
  ft <- bg(ft, i = yr_idx[c(TRUE, FALSE)], bg = "#f7f7f7")
  ft <- bold(ft, i = yr_idx, j = 1)
  
  # number/currency format
  
  if (isTRUE(currency)) {
    ft <- colformat_num(ft, j = month_cols, digits = digits, big.mark = ",", prefix = "$", na_str = "")
  } else {
    ft <- colformat_num(ft, j = month_cols, digits = digits, big.mark = ",", na_str = "")
  }
  
  # layout
  
  ft <- fontsize(ft, part = "all", size = font_size)
  ft <- autofit(ft)
  ft <- fit_to_width(ft, max_width = 9)
  
  # dynamic title row
  
  rng <- df %>% summarise(min_y = min(y, na.rm = TRUE), max_y = max(y, na.rm = TRUE))
  title_txt <- paste(region, indicator, sprintf("(%d–%d)", rng$min_y, rng$max_y))
  ft <- add_header_row(ft, values = title_txt, colwidths = ncol(out))
  ft <- bold(ft, part = "header", i = 1)
  ft <- fontsize(ft, part = "header", i = 1, size = font_size + 2)
  ft <- align(ft, part = "header", i = 1, align = "center")
  
  ft
}

# 7) Stacked column chart (Taxi over TNS or vice versa)

make_indicator_chart_stacked <- function(
    data, indicator, region, services = c("TNS","TAXI"), area_type = "REGIONAL",
    date_from = NULL, date_to = NULL,
    title = "Trip Volume (trips)",
    caption = NULL,
    colors = c(TAXI = "#69B7FF", TNS = "#1E3A8A"),
    base_size = 10
) {
  df <- filter_indicator(data, indicator, region, services, area_type, date_from, date_to) %>%
    mutate(service = factor(service, levels = services))
  
  if (nrow(df) == 0) stop("No rows matched your filters.")
  
  ggplot(df, aes(x = date, y = value, fill = service)) +
    geom_col(width = 26, position = "stack") +
    scale_fill_manual(values = colors, name = "Service Type") +
    scale_y_continuous(labels = label_number(accuracy = 0.1, scale_cut = cut_short_scale()), expand = c(0, 0.02)) +
    scale_x_date(breaks = pretty_breaks(n = 18), date_labels = "%Y\n%b") +
    labs(title = title, subtitle = region, x = NULL, y = NULL, caption = caption) +
    theme_minimal(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_text(size = base_size, face = "bold"),
      legend.key.width = unit(1.2, "lines"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(margin = margin(t = 4)),
      plot.margin = margin(6, 8, 6, 6)
    )
}

# 8) One-stop metrics helper for inline use

compute_metrics <- function(data, indicator, region, focus_service = "TNS",
                            area_type = "REGIONAL", date_from = NULL, date_to = NULL) {
  list(
    last_value = last_month_value(data, indicator, region, services = focus_service,
                                  area_type = area_type, date_from = date_from, date_to = date_to),
    yoy_last   = last_month_yoy(data, indicator, region, services = focus_service,
                                area_type = area_type, date_from = date_from, date_to = date_to),
    ytd_yoy    = ytd_yoy(data, indicator, region, end_date = date_to,
                         services = focus_service, area_type = area_type)
  )
}




# ============================================================

# Metrics (Trip Volume, TNS focus)

# ============================================================

# last-month / YoY / YTD YoY for TNS trips in the selected region and window

m <- compute_metrics(
  data          = raw_norm,
  indicator     = "TRIP_VOLUME",
  region        = region_name,
  focus_service = "TNS",
  area_type     = "REGIONAL",
  date_from     = date_from,
  date_to       = date_to
)

tns_last_val      <- m$last_value
tns_last_val_fmt  <- fmt_num(tns_last_val, 0)
tns_yoy_fmt       <- fmt_pct(m$yoy_last, 2)
tns_ytd_yoy_fmt   <- fmt_pct(m$ytd_yoy, 2)

# (If you still want combined TAXI+TNS YoY/YTD YoY, just call with services = c("TAXI","TNS"))

```