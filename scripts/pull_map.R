library(rhdx)
library(sf)
library(dplyr) 

download.file('http://stat-athens.aueb.gr/~jbn/papers/files/14/14_bivpois_RDATA.zip', 
              f <- tempfile())
unzip(f, exdir=tempdir())
load(file.path(tempdir(), '.RData'))

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

###If above code doesn't work, use code below to load maps from filed in working directory
province_shp<-st_read("maps/chn_admbnda_adm1_ocha.shp")
prefecture_shp<-st_read("maps/chn_admbnda_adm2_ocha.shp")
