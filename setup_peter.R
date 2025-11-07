source("setup.R")

# Override to prevent comma formatting in Year column (e.g., 2,023)
make_indicator_table_compact <- function(
    data, indicator, region, services = c("TAXI","TNS"), area_type = "REGIONAL",
    date_from = NULL, date_to = NULL, digits = 0, zero_to_blank = FALSE,
    currency = NULL, font_size = 4.5
) {
  if (is.null(currency)) currency <- stringr::str_detect(indicator, stringr::regex("REVENUE|FARE", ignore_case = TRUE))

  df <- filter_indicator(data, indicator, region, services, area_type, date_from, date_to) %>%
    dplyr::transmute(service, y = lubridate::year(date), m = lubridate::month(date), value)

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

  if (isTRUE(zero_to_blank)) {
    wide <- wide %>% dplyr::mutate(dplyr::across(dplyr::all_of(month_cols), ~ ifelse(.x == 0, NA_real_, .x)))
  }

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
  # Key change: do NOT format year with big.mark; use plain string
  out   <- wide2 %>% dplyr::mutate(Year = ifelse(is.na(y), service, as.character(y))) %>%
    dplyr::select(Year, dplyr::all_of(month_cols))

  ft <- flextable::flextable(out)

  hdr_idx <- which(is.na(wide2$y))
  if (length(hdr_idx)) {
    for (i in hdr_idx) ft <- flextable::merge_at(ft, i = i, j = 1:ncol(out))
    ft <- flextable::bold(ft, i = hdr_idx, bold = TRUE)
    ft <- flextable::align(ft, i = hdr_idx, align = "left")
    ft <- flextable::bg(ft, i = hdr_idx, bg = "#d9ead3")
  }

  yr_idx <- setdiff(seq_len(nrow(out)), hdr_idx)
  ft <- flextable::bg(ft, i = yr_idx, bg = "#ffffff")
  ft <- flextable::bg(ft, i = yr_idx[c(TRUE, FALSE)], bg = "#f7f7f7")
  ft <- flextable::bold(ft, i = yr_idx, j = 1)

  if (isTRUE(currency)) {
    ft <- flextable::colformat_num(ft, j = month_cols, digits = digits, big.mark = ",", prefix = "$", na_str = "")
  } else {
    ft <- flextable::colformat_num(ft, j = month_cols, digits = digits, big.mark = ",", na_str = "")
  }

  ft <- flextable::fontsize(ft, part = "all", size = font_size)
  ft <- flextable::autofit(ft)
  ft <- flextable::set_table_properties(ft, width = 0.1, layout = "autofit")
  ft <- flextable::width(ft, width = 0.1)

  rng <- df %>% dplyr::summarise(min_y = min(y, na.rm = TRUE), max_y = max(y, na.rm = TRUE))
  title_txt <- paste(region, indicator, sprintf("(%d-%d)", rng$min_y, rng$max_y))
  ft <- flextable::add_header_row(ft, values = title_txt, colwidths = ncol(out))
  ft <- flextable::bold(ft, part = "header", i = 1)
  ft <- flextable::fontsize(ft, part = "header", i = 1, size = font_size + 2)
  ft <- flextable::align(ft, part = "header", i = 1, align = "center")

  ft
}

