
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
  library(stringr) 
  library(knitr)
  library(kableExtra)
  
})

# -----------------------------

# Parameters (edit as needed)

# -----------------------------

date_from   <- make_date(2023, 1, 1)     # report window start (inclusive)
date_to     <- make_date(2025, 5, 1)     # report window end   (inclusive)
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
    area_type = LEVEL_NAME,
    indicator = INDICATOR_TYPE,
    service   = TRIP_SERVICE_TYPE_CODE,
    value     = suppressWarnings(as.numeric(INDICATOR_VALUE))
  ) %>%
  filter(!is.na(date), !is.na(value))

# Restrict to reporting window for “windowed” analyses; we’ll still compute YTD from full data when needed.

windowed <- raw_norm %>%
  filter(between(date, date_from, date_to))


# ============================================================

# Helpers (single source of truth for math/formatting)

# ============================================================

# 1) Region aliasing: treat "Metro Vancouver Regional District" and "Metro Vancouver" as the same.

#with_region_alias <- function(region) {
#  if (identical(region, "Metro Vancouver Regional District")) {
#    c(region, "Metro Vancouver")
#  } else if (identical(region, "Metro Vancouver")) {
#    c("Metro Vancouver", "Metro Vancouver Regional District")
#  } else {
#    region
#  }
#}

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
  region_vec <- region_name
  
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
#    Override: Year column plain (no comma formatting), no table title row

make_indicator_table_compact <- function(
    data, indicator, region, services = c("TAXI","TNS"), area_type = "REGIONAL",
    date_from = NULL, date_to = NULL, digits = 0, zero_to_blank = FALSE,
    currency = NULL, font_size = 6
) {
  # auto-detect currency style if not set
  if (is.null(currency)) {
    currency <- stringr::str_detect(
      indicator,
      stringr::regex("REVENUE|FARE", ignore_case = TRUE)
    )
  }
  
  df <- filter_indicator(
    data      = data,
    indicator = indicator,
    region    = region,
    services  = services,
    area_type = area_type,
    date_from = date_from,
    date_to   = date_to
  ) %>%
    dplyr::transmute(
      service,
      y = lubridate::year(date),
      m = lubridate::month(date),
      value = round(value, 2)
    )
  
  if (nrow(df) == 0) stop("No rows matched your filters.")
  
  dfc <- df %>%
    dplyr::group_by(service, y) %>%
    tidyr::complete(m = 1:12, fill = list(value = NA_real_)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(m_lab = lubridate::month(m, label = TRUE, abbr = TRUE))
  
  month_cols <- as.character(lubridate::month(1:12, label = TRUE, abbr = TRUE))
  
  wide <- dfc %>%
    dplyr::select(service, y, m_lab, value) %>%
    tidyr::pivot_wider(names_from = m_lab, values_from = value) %>%
    dplyr::arrange(match(service, services), y)
  
  
  # optional blank zeros to reduce noise
  if (isTRUE(zero_to_blank)) {
    wide <- wide %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(month_cols),
        ~ ifelse(.x == 0, NA_real_, .x)
      ))
  }
  
  # insert service header rows
  add_headers <- function(w, services) {
    out <- list()
    for (srv in services) {
      block <- w %>% dplyr::filter(service == srv)
      if (nrow(block) == 0) next
      header <- tibble::tibble(service = srv, y = NA_integer_)
      for (m in month_cols) header[[m]] <- NA_real_
      out[[srv]] <- dplyr::bind_rows(header, block)
    }
    dplyr::bind_rows(out)
  }
  
  wide2 <- add_headers(wide, services)
  
  # Year as plain string (no big.mark commas);
  # header rows use service label in Year column
  out <- wide2 %>%
    dplyr::mutate(
      Year = ifelse(is.na(y), service, as.character(y))
    ) %>%
    dplyr::select(Year, dplyr::all_of(month_cols))
  
  ft <- flextable::flextable(out)
  
  # style service header rows
  hdr_idx <- which(is.na(wide2$y))
  if (length(hdr_idx)) {
    for (i in hdr_idx) {
      ft <- flextable::merge_at(ft, i = i, j = 1:ncol(out))
    }
    ft <- flextable::bold(ft, i = hdr_idx, bold = TRUE)
    ft <- flextable::align(ft, i = hdr_idx, align = "left")
    ft <- flextable::bg(ft, i = hdr_idx, bg = "#ECECEC")
  }
  
  # zebra for year rows
  yr_idx <- setdiff(seq_len(nrow(out)), hdr_idx)
  ft <- flextable::bg(ft, i = yr_idx, bg = "#ffffff")
  ft <- flextable::bg(ft, i = yr_idx[c(TRUE, FALSE)], bg = "#f7f7f7")
  ft <- flextable::bold(ft, i = yr_idx, j = 1)
  
  # number/currency format
  if (isTRUE(currency)) {
    ft <- flextable::colformat_num(
      ft, j = month_cols, digits = digits,
      big.mark = ",", prefix = "$", na_str = ""
    )
  } else {
    ft <- flextable::colformat_num(
      ft, j = month_cols, digits = digits,
      big.mark = ",", na_str = ""
    )
  }
  
  # layout
  ft <- flextable::fontsize(ft, part = "all", size = font_size)
  ft <- flextable::autofit(ft)
  ft <- flextable::set_table_properties(ft, width = 0.1, layout = "autofit")
  ft <- flextable::width(ft, width = 0.1)
  
  # no title header row added
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
    labs(title = title, subtitle = paste(region, "Regional District"), x = NULL, y = NULL, caption = caption) +
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


# 7.1) Stacked column chart (Taxi over TNS or vice versa) with lable
make_indicator_chart_stacked_2 <- function(
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
    
    # ADD LABELS
    geom_text(
      aes(label = scales::number(value, accuracy = 0.01)),
      position = position_stack(vjust = 0.5),
      size = 3,
      color = "white"
    ) +
    
    scale_fill_manual(values = colors, name = "Service Type") +
    scale_y_continuous(labels = label_number(accuracy = 0.1, scale_cut = cut_short_scale()), expand = c(0, 0.02)) +
    scale_x_date(breaks = pretty_breaks(n = 18), date_labels = "%Y\n%b") +
    labs(title = title, subtitle = paste(region, "Regional District"), x = NULL, y = NULL, caption = caption) +
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

# 11) MoM
last_month_mom <- function(data, indicator, region,
                           services = c("TAXI","TNS"),
                           area_type = "REGIONAL",
                           date_from = NULL, date_to = NULL) {
  df <- filter_indicator(data, indicator, region, services, area_type, date_from, date_to)
  if (nrow(df) == 0) return(NA_real_)
  
  # آخرین ماه در پنجره
  last_date <- max(df$date, na.rm = TRUE)
  # ماه قبل
  prev_date <- last_date %m-% months(1)
  
  cur  <- df %>%
    filter(date == last_date) %>%
    summarise(v = sum(value, na.rm = TRUE)) %>%
    pull(v)
  
  prev <- df %>%
    filter(date == prev_date) %>%
    summarise(v = sum(value, na.rm = TRUE)) %>%
    pull(v)
  
  if (is.na(prev) || prev == 0) return(NA_real_)
  
  # هم‌اسکیل با yoy_ytd: درصد، نه نسبت
  (cur / prev - 1) * 100
}


# 8) One-stop metrics helper for inline use

compute_metrics <- function(data, indicator, region, focus_service = "TNS",
                            area_type = "REGIONAL", date_from = NULL, date_to = NULL) {
  list(
    last_value = last_month_value(
      data, indicator, region,
      services  = focus_service,
      area_type = area_type,
      date_from = date_from,
      date_to   = date_to
    ),
    yoy_last   = last_month_yoy(
      data, indicator, region,
      services  = focus_service,
      area_type = area_type,
      date_from = date_from,
      date_to   = date_to
    ),
    ytd_yoy    = ytd_yoy(
      data, indicator, region,
      end_date  = date_to,
      services  = focus_service,
      area_type = area_type
    ),
    mom_last   = last_month_mom(            # NEW
      data, indicator, region,
      services  = focus_service,
      area_type = area_type,
      date_from = date_from,
      date_to   = date_to
    )
  )
}


# 9) Dual-line chart (Taxi vs TNS) — e.g., Wait Time (minutes)

make_indicator_chart_lines <- function(
    data,
    indicator  = "WAIT_TIME",
    region,
    services   = c("TAXI", "TNS"),
    area_type  = "REGIONAL",
    date_from  = NULL,
    date_to    = NULL,
    title      = NULL,
    caption    = NULL,
    colors     = c(TAXI = "#69B7FF", TNS = "#1E3A8A"),
    base_size  = 10
) {
  # get + tidy
  df <- filter_indicator(
    data        = data,
    indicator   = indicator,
    region      = region,
    services    = services,
    area_type   = area_type,
    date_from   = date_from,
    date_to     = date_to
  ) %>%
    mutate(service = factor(service, levels = services)) %>%
    group_by(date, service) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  
  if (nrow(df) == 0) stop("No rows matched your filters.")
  
  # auto title/y label
  if (is.null(title)) {
    pretty_ind <- gsub("_", " ", indicator)
    title <- paste0(stringr::str_to_title(pretty_ind))
  }
  y_lab <- if (indicator == "WAIT_TIME") "minutes" else NULL
  
  ggplot(df, aes(x = date, y = value, color = service)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.8, stroke = 0.2) +
    scale_color_manual(values = colors, name = "Service Type") +
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    scale_x_date(breaks = pretty_breaks(n = 18), date_labels = "%b\n%Y") +
    labs(
      title    = title,
      subtitle = paste(region, "Regional District"),
      x        = NULL,
      y        = y_lab,
      caption  = caption
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_text(size = base_size, face = "bold"),
      legend.key.width = unit(1.2, "lines"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(margin = margin(t = 4)),
      plot.margin = margin(6, 8, 6, 6)
    )
}


# 10) line chart multiple indicators

make_multi_indicator_chart_lines <- function(
    data,
    indicators,                      # e.g. c("ACTIVE_WAV", "ACTIVE_WAV_WITH_ACCESSIBLE_TRIP", "ALLOCATED_WAV")
    region,
    services   = "TAXI",             # usually one service for this style of chart
    area_type  = "REGIONAL",
    date_from  = NULL,
    date_to    = NULL,
    title      = NULL,
    caption    = NULL,
    colors     = NULL,               # named vector, one color per indicator (optional)
    labels     = NULL, 
    base_size  = 10
) {
  # build tidy df for all indicators
  df <- purrr::map_dfr(
    indicators,
    ~ filter_indicator(
      data       = data,
      indicator  = .x,
      region     = region,
      services   = services,
      area_type  = area_type,
      date_from  = date_from,
      date_to    = date_to
    ) %>%
      mutate(indicator = .x)
  ) %>%
    mutate(
      indicator = factor(indicator, levels = indicators),
      service   = factor(service,  levels = services)
    ) %>%
    group_by(date, indicator) %>%              # collapse over service if you passed one
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  
  if (nrow(df) == 0) stop("No rows matched your filters.")
  
  # auto title
  if (is.null(title)) {
    pretty_inds <- gsub("_", " ", indicators)
    title <- paste0("Indicators: ", paste(stringr::str_to_title(pretty_inds), collapse = ", "))
  }
  
  # colour palette: one colour per indicator if not supplied
  if (is.null(colors)) {
    pal <- scales::hue_pal()(length(indicators))
    names(pal) <- indicators
    colors <- pal
  } else {
    if (is.null(names(colors))) names(colors) <- indicators
    colors <- colors[indicators]
  }
  
  ggplot(df, aes(x = date, y = value, color = indicator)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.8, stroke = 0.2) +
    scale_color_manual(values = colors,labels = labels, name = NULL) +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
    scale_x_date(
      breaks = scales::pretty_breaks(n = 18),
      date_labels = "%b\n%Y"
    ) +
    labs(
      title    = title,
      subtitle = paste(region, "Regional District"),
      x        = NULL,
      y        = NULL,
      caption  = caption
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_text(size = base_size, face = "bold"),
      legend.key.width = unit(1.6, "lines"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(margin = margin(t = 4)),
      plot.margin = margin(6, 8, 6, 6)
    )
}






# ============================================================

# Metrics 

# ============================================================

indicators <- c(
  "ACCESSIBLE_TRIP",
  "ACTIVE_VEHICLE",
  "ACTIVE_WAV",
  "FARE_PER_5KM",
  "FLEET_UTILIZATION",
  "REVENUE_PER_VEHICLE",
  "TRIP_REVENUE",
  "TRIP_VOLUME",
  "VEHICLE_OCCUPANCY_RATE",
  "VEHICLE_PER_1000_PERSONS",
  "WAIT_TIME",
  # NEW:
  "ACTIVE_WAV_PER_1000_PERSONS",
  "ACTIVE_WAV_WITH_ACCESSIBLE_TRIP",
  "ACTIVE_WAV_WITH_ACCESS_TRIP_PER_1000_PERSONS",
  "ALLOCATED_LICENSEE",
  "ALLOCATED_WAV",
  "DRIVER_EARNING",
  "MAX_FLEET"
)

ind_units <- list(
  ACCESSIBLE_TRIP            = list(unit="count",   digits=0, currency=FALSE, scale=1),
  ACTIVE_VEHICLE             = list(unit="count",   digits=0, currency=FALSE, scale=1),
  ACTIVE_WAV                 = list(unit="count",   digits=0, currency=FALSE, scale=1),
  FARE_PER_5KM               = list(unit="currency",digits=2, currency=TRUE,  scale=1),
  FLEET_UTILIZATION          = list(unit="pct",     digits=1, currency=FALSE, scale=100), # اگر داده‌ات 0..1 است
  REVENUE_PER_VEHICLE        = list(unit="currency",digits=0, currency=TRUE,  scale=1),
  TRIP_REVENUE               = list(unit="currency",digits=0, currency=TRUE,  scale=1),
  TRIP_VOLUME                = list(unit="count",   digits=0, currency=FALSE, scale=1),
  VEHICLE_OCCUPANCY_RATE     = list(unit="pct",     digits=1, currency=FALSE, scale=100),
  VEHICLE_PER_1000_PERSONS   = list(unit="count",   digits=2, currency=FALSE, scale=1),
  WAIT_TIME                  = list(unit="minutes", digits=1, currency=FALSE, scale=1),
  # New Indicators ---------------------------------------------------------
  ACTIVE_WAV_PER_1000_PERSONS           = list(unit="count",    digits=2, currency=FALSE, scale=1),
  ACTIVE_WAV_WITH_ACCESSIBLE_TRIP       = list(unit="count",    digits=0, currency=FALSE, scale=1),
  ACTIVE_WAV_WITH_ACCESS_TRIP_PER_1000_PERSONS = list(unit="count", digits=2, currency=FALSE, scale=1),
  
  ALLOCATED_LICENSEE        = list(unit="count",    digits=0, currency=FALSE, scale=1),
  ALLOCATED_WAV             = list(unit="count",    digits=0, currency=FALSE, scale=1),
  
  DRIVER_EARNING            = list(unit="currency", digits=0, currency=TRUE,  scale=1),
  
  MAX_FLEET                 = list(unit="count",    digits=0, currency=FALSE, scale=1)
)

fmt_value <- function(x, meta){
  if (is.na(x)) return("n/a")
  x <- x * (meta$scale %||% 1)
  unit <- meta$unit
  d    <- meta$digits %||% 0
  if (isTRUE(meta$currency) || unit == "currency")      return(fmt_num(x, d, currency = TRUE))
  if (unit == "pct")                                    return(paste0(format(round(x, d), nsmall=d), "%"))
  if (unit == "minutes")                                return(paste0(fmt_num(x, d), " min"))
  fmt_num(x, d)
}

suppressPackageStartupMessages({ library(purrr); library(dplyr) })

metrics <- setNames(vector("list", length(indicators)), indicators)

for (ind in indicators) {
  meta <- ind_units[[ind]]
  for (srv in c("TAXI","TNS")) {
    m <- compute_metrics(
      data          = raw_norm,
      indicator     = ind,
      region        = region_name,
      focus_service = srv,
      area_type     = "REGIONAL",
      date_from     = date_from,
      date_to       = date_to
    )
    
    metrics[[ind]][[srv]] <- list(
      last_value      = m$last_value,
      yoy_last        = m$yoy_last,
      ytd_yoy         = m$ytd_yoy,
      mom_last        = m$mom_last,           # NEW: raw MoM
      
      last_value_fmt  = fmt_value(m$last_value, meta),
      yoy_fmt         = fmt_pct(m$yoy_last, 2),
      ytd_yoy_fmt     = fmt_pct(m$ytd_yoy, 2),
      mom_fmt         = fmt_pct(m$mom_last, 2) # NEW: formatted MoM
    )
    
  }
}


metric <- function(
    indicator,
    service,
    field = c(
      "last_value_fmt",
      "yoy_fmt",
      "ytd_yoy_fmt",
      "mom_fmt",    # NEW: formatted MoM
      "last_value",
      "yoy_last",
      "ytd_yoy",
      "mom_last"    # NEW: raw MoM
    )
) {
  field <- match.arg(field)
  metrics[[indicator]][[service]][[field]]
}























