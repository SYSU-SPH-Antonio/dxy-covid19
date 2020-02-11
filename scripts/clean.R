# ===================================================================== #
# nCoV DXY Cleaning Code                                                #
# Sean Browning                                                         #
# ===================================================================== #
library(dplyr)
library(tidyr)
library(sf)
library(janitor)
china <- "\U4E2D\U56FD"

# === Cleaning Prefecture data ==========================================
# Read in data
ncov_prefecture <- readRDS("data/ncov.RDS")$cities %>%
  clean_names()
adm2_shp <- st_read("data/china_adm2.geojson")

ncov_area <- readRDS("data/ncov.RDS")$area %>%
  clean_names()
# Manual corrections 
ncov_prefecture <- ncov_prefecture %>%
  mutate(
    province_name = case_when(
      province_name == "\U6FB3\U95E8" ~ "\U6FB3\U95E8\U7279\U522B\U884C\U653F\U533A" # Macau
    )
  )
# Make lookup table for province data
province_lookup <- adm2_shp[, c("ADM1_EN", "ADM1_ZH"), drop = TRUE] %>% 
  distinct() %>%
  setNames(c("province", "zh"))

setdiff(ncov_prefecture$provinceName, province_lookup$zh)
# Filter to only Chinese prefectures
ncov_prefecture <- ncov_prefecture %>%
  filter(country %in% china) %>%
  select(-cities)

# make list of adm2 boundries by postal code
adm2_codes <- adm2_shp[, "ADM2_PCODE", drop = TRUE]

dxy_codes <- ncov_prefecture %>%
  filter(!locationId %in% c(-1L:0L, NA_integer_)) %>%
  mutate(id = sprintf("CN%i", locationId)) %>%
  distinct(locationId) %>%
  pull()

missing_map_codes <- setdiff(adm2_codes, dxy_codes)
unknown_dxy_codes <- setdiff(dxy_codes, adm2_codes)

ncov_prefecture %>%
  filter(locationId %in% as.integer(sub("CN", "", unknown_dxy_codes))) %>%
  distinct(provinceName, cityName, id = locationId)

ncov_prefecture %>%
      filter(locationId %in% as.integer(sub("CN", "", unknown_dxy_codes))) %>%
      distinct(provinceName, cityName, id = locationId) %>%
      mutate(missing = sprintf("%s - %s", provinceName, cityName)) %>%
      distinct(missing) %>%
      pull(missing) %>% 
      iconv(from="UTF8", to="GBK") %>%
      cat(sep = "\n", file = "out.txt")

