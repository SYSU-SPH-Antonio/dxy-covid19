library(rhdx)
library(sf)

# Chinese ADM 0-2 shapefile
# https://data.humdata.org/dataset/china-administrative-boundaries
shp_lnk <- "17a2aaa2-dea9-4a2e-8b3f-92d1bdfb850c"

# Pull prefecture and provencial shapefile
# Save both as GEOJSON
set_rhdx_config(hdx_site = "prod")

prefecture_shp <- pull_dataset(shp_lnk) %>%
  get_resource(5) %>%
  read_resource(download_folder = tempdir())

st_write(prefecture_shp, "data/china_adm2.geojson", delete_dsn = TRUE)

province_shp <- pull_dataset(shp_lnk) %>%
  get_resource(3) %>%
  read_resource(download_folder = tempdir())

st_write(province_shp, "data/china_adm1.geojson", delete_dsn = TRUE)