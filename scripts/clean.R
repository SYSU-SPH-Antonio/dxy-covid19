# ===================================================================== #
# nCoV DXY Cleaning Code                                                #
# Sean Browning                                                         #
# ===================================================================== #
library(dplyr)
library(tidyr)
library(sf)
library(janitor)
china <- "\U4E2D\U56FD" #中国

# === Cleaning Prefecture data ==========================================
# --- Read in data ------------------------------------------
ncov_prefecture <- readRDS("data/ncov.RDS")$cities %>%
  clean_names()

adm2_shp <- st_read("data/china_adm2.geojson")

ncov_area <- readRDS("data/ncov.RDS")$area %>%
  clean_names()

# --- Make lookup table for province data from the map -------------------------
province_lookup <- adm2_shp[, c("ADM1_EN", "ADM1_ZH"), drop = TRUE] %>%
  distinct(adm1_en = ADM1_EN, adm1_zh = ADM1_ZH)

# --- Create ADM2-based clean set ----------------------------------------------
# Handle HK, Macau, Taiwan
ncov_adm2_ext <- ncov_area %>%
  filter(province_name %in% c("香港", "台湾", "澳门")) %>%
  mutate(
    adm1_zh = case_when(
      province_name == "台湾" ~ "\U53F0\U6E7E\U7701", # Taiwan
      province_name == "澳门" ~ "\U6FB3\U95E8\U7279\U522B\U884C\U653F\U533A", # Macau
      province_name == "香港" ~ "\U9999\U6E2F\U7279\U522B\U884C\U653F\U533A", #HK
      TRUE ~ province_name
    ),
    adm2_zh = province_name,
    country_en = "China" # This is not a political statement.
  ) %>%
  select(country_zh = country, country_en, adm1_zh, adm2_zh, location_id, matches("time"), matches("count"))

# Handle the rest of China
ncov_adm2_china <- ncov_area %>%
  filter(country %in% china, !province_name %in% c("香港", "台湾", "澳门")) %>%
  select(country, adm1_zh = province_name, matches("time"), cities) %>%
  unnest(cities) %>%
  clean_names() %>%
  mutate(country_en = "China") %>%
  select(
    country_zh = country,
    country_en,
    adm1_zh,
    adm2_zh = city_name,
    location_id,
    matches("time"),
    matches("count")
  )

# Bind all together, add English ADM1
ncov_adm2 <- bind_rows(
  ncov_adm2_china,
  ncov_adm2_ext
) %>%
left_join(
  province_lookup,
  by = "adm1_zh"
) %>%
select(country_zh, country_en, adm1_zh, adm1_en, everything())

# NOTE: At this point, ADM1 is clean, but ADM2 stil has mis-matches

# --- Clean ADM2 -------------------------------------------------------------

raw_lookup <- ncov_adm2 %>%
  distinct(adm1_zh, adm2_zh, location_id)

map_lookup <- adm2_shp[, c("ADM1_ZH", "ADM1_EN", "ADM2_ZH", "ADM2_EN"), drop = TRUE] %>%
  setNames(tolower(names(.))) %>%
  as_tibble()
