# ===================================================================== #
# nCoV DXY Cleaning Code                                                #
# Sean Browning                                                         #
# ===================================================================== #
library(dplyr)
library(tidyr)
library(sf)
library(janitor)
library(stringr)
library(tmap)
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
  #select(country_zh = country, country_en, adm1_zh, adm2_zh, location_id, matches("time"), matches("count"))
  select(country_zh = country, country_en, adm1_zh, adm2_zh, matches("time"), matches("count"))

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


############################################


#Dataset uses adm_2 to specify neighborhood for beijing, chongqing, tianjin and shanghai, but map only goes down to the city level
#Need to aggregate numbers for the four cities
ncov_municipality<-ncov_adm2 %>% group_by(adm1_zh)%>%
                  summarise(update_time=max(update_time),
                    current_confirmed_count=sum(current_confirmed_count),
                     confirmed_count=sum(confirmed_count),
                     suspected_count=sum(suspected_count),
                     cured_count=sum(cured_count),
                     dead_count=sum(dead_count))%>%
                    filter(adm1_zh %in% c("北京市","上海市","重庆市","天津市"))

ncov_municipality$adm2_zh<-ncov_municipality$adm1_zh


ncov_adm2<-ncov_adm2%>%
          filter(!adm1_zh %in% c("北京市","上海市","重庆市","天津市"))%>%
          bind_rows(ncov_municipality)

ncov_adm2_merged<-merge(map_lookup,raw_lookup,by.x = c("adm2_zh","adm1_zh"),by.y=c("adm2_zh","adm1_zh"))%>%
  setNames(tolower(names(.))) %>%
  as_tibble()

#Removing extra words like "city" etc to help with merging
ncov_adm2$adm2_zh<-gsub("\\[1]|\\[2]|\\|\\[3]|\\市|\\县", "", ncov_adm2$adm2_zh)
adm2_shp$ADM2_ZH<-gsub("\\[1]|\\[2]|\\|\\[3]|\\市|\\县", "", adm2_shp$ADM2_ZH)

adm2_shp<-adm2_shp%>% setNames(tolower(names(.)))

## Left join data onto shape file by adm2 and adm1

ncov_adm2_merged<-adm2_shp%>% left_join(ncov_adm2,by=c("adm2_zh","adm1_zh"))%>%
                                          setNames(tolower(names(.))) 

### Left with 49 unmatched, 5 lines with unknown prefecture
unmatched<-ncov_adm2[!ncov_adm2$adm2_zh%in%ncov_adm2_merged$adm2_zh,]


####Display map
##Note that province shape borders is off
tm_shape(ncov_adm2_merged)+
  tm_fill(c('confirmed_count','cured_count'),
          palette='BuPu',
          style='quantile',
          title=c('Confirmed','Cured'))+
tm_shape(province_shp)+
  tm_borders()

