library(dplyr)
library(jsonlite)
library(tidyr)

conv_time <- function(x) {
  as.POSIXct('1970-01-01', tz = 'GMT') + x / 1000
}

get_ncov <- function(
  port = c("area", "overall", "news", "rumors"),
  base = "https://raw.githubusercontent.com/BlankerL/DXY-COVID-19-Data/master/json/") {

    stopifnot(port %in% c("area", "overall", "news", "rumors"))

    url_resolve <- function(x) {
      switch(
        x,
        area = "DXYArea.json",
        overall = "DXYOverall.json",
        news = "DXYNews.json",
        rumors = "DXYRumors.json"
      )
    }

    ncov <- lapply(port,
                   function(x) {
                     jsonlite::fromJSON(paste0(base, url_resolve(x)))$results
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
  
  return(ncov)
}

# Pull data and convert
ncov <- get_ncov(port = c("area", "overall"))
ncov_tidy <- conv_ncov(ncov)

# Save to RDS
saveRDS(ncov_tidy, "data/ncov.RDS")