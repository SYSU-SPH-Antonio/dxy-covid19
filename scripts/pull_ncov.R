renv::restore()
library(dplyr)
library(jsonlite)
library(tidyr)

conv_time <- function(x) {
  as.POSIXct('1970-01-01', tz = 'GMT') + x / 1000
}

get_ncov <- function(
  port = c("area", "overall", "provinceName", "news", "rumors"),
  base = "https://lab.isaaclin.cn/nCoV/api/") {

    ncov <- lapply(port,
                   function(x) {
                     jsonlite::fromJSON(paste0(base, x))$results
                   })

    names(ncov) <- port


  ncov
}


conv_ncov <- function(ncov) {
  # Convert Time Stamps
  ncov$area$updateTime <- conv_time(ncov$area$updateTime)
  ncov$area$createTime <- conv_time(ncov$area$createTime)
  ncov$area$modifyTime <- conv_time(ncov$area$modifyTime)
  ncov$overall$updateTime <- conv_time(ncov$overall$updateTime)

  # Convert all to tibble
  ncov$area <- as_tibble(ncov$area)
  ncov$overall <- as_tibble(ncov$overall)

  # Pull prefecture data
  ncov$cities <- ncov$area %>%
    dplyr::select(country, provinceName, provinceShortName, updateTime, createTime, modifyTime, cities) %>%
    tidyr::unnest(cities)
  
  return(ncov)
}

# Pull data and convert
ncov <- get_ncov(port = c("area?latest=0", "overall", "provinceName"))
names(ncov)[1] <- "area"
ncov_tidy <- conv_ncov(ncov)

# Save to RDS
saveRDS(ncov_tidy, "data/ncov.RDS")