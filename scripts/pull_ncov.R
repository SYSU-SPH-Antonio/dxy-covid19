library(dplyr)
library(mongolite)
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

# Query the DXY Crawler DB and pull aggregate counts
query_db <- function(collection = "DXYArea", uri = "localhost", dbname = "COVID-19") {
  con <- mongo(collection = collection, db = dbname, url = sprintf("mongodb://%s", uri))

  on.exit(con$disconnect())

  data_out <- con$aggregate(
  '[{
    "$sort": {
      "updateTime": -1,
      "crawlTime": -1
    }
  }]',
  options = '{"allowDiskUse":true}'
)

  return(data_out[, -1])
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
collections <- c(area = "DXYArea", overall = "DXYOverall")

ncov <- lapply(collections, query_db) %>%
  setNames(names(collections))

ncov_tidy <- conv_ncov(ncov)

# Save to RDS
saveRDS(ncov_tidy, "data/ncov.RDS")