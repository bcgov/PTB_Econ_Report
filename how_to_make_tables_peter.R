# 0) Packages (you already loaded these)
library(dplyr)
library(tidyr)
library(flextable)
library(officer)   # fp_border


load ('W:\\PTBoard\\Initiatives\\Data Modelling & Policy Initiative\\Data Economic Modelling Projects\\2023_011_Indicators\\src\\trip_volume\\3_trip_volume_cube.Rda')

trip_vol_reduced<-trip_vol%>%
  group_by(
    yr,
    mth,
    GEOSPATIAL_AREA_TYPE_CODE,
    TRIP_SERVICE_TYPE_CODE,
    PICKUP_AREA,
    PTS_ORGANIZATION_NAME
  )%>%
  summarize(
    vol=sum(volume),
    rev=sum(rev_per_vehcile)
  )

month_labs <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# Prep: Year as *character* (no numeric formatting ever applied)
df_wide <- trip_vol_reduced %>%
  filter(GEOSPATIAL_AREA_TYPE_CODE == "REGIONAL") %>%
  transmute(
    Region  = PICKUP_AREA,
    Service = TRIP_SERVICE_TYPE_CODE,
    Year    = sprintf("%d", as.integer(yr)),           # <-- key: make Year a character
    Month   = factor(mth, levels = 1:12, labels = month_labs),
    Value   = vol
  ) %>%
  group_by(Region, Service, Year, Month) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Month, values_from = Value, values_fill = 0L) %>%
  arrange(Region, Service, Year)

data_g <- as_grouped_data(df_wide, groups = c("Region","Service"), columns = c("Year", month_labs))

ft <- as_flextable(data_g, hide_grouplabel = TRUE) %>%
  align(j = "Year", align = "left", part = "body") %>%
  colformat_num(j = month_labs, digits = 0, big.mark = ",") %>%   # only months get commas
  autofit()


# ----------------------------
# 5) Don’t repeat the RD banner on the second Service; add separators above each RD
# ----------------------------
bd <- ft$body$dataset
reg_rows <- which(is.na(bd$Year) & is.na(bd$Service))               # RD header rows

# (a) Blank duplicate RD headers (keep only the first per RD)
dup_reg_rows <- reg_rows[duplicated(bd$Region[reg_rows])]
if (length(dup_reg_rows)) {
  ft <- compose(ft, i = dup_reg_rows, j = 1, value = as_paragraph("")) %>%
    padding(i = dup_reg_rows, j = 1, padding.top = 0, padding.bottom = 0)
}

# (b)
b_top <- officer::fp_border(color = "#9CA3AF", width = 1.5)
ft <- border(ft, i = row_lines, j = ft$col_keys, border.top = b_top)