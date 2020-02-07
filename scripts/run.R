conv_time <- function(x){
  as.POSIXct('1970-01-01', tz = 'GMT') + x / 1000
}

get_ncov <- function(port = c('area', 'overall', 'provinceName', 'news', 'rumors'), base = 'https://lab.isaaclin.cn/nCoV/api/'){

    ncov <- lapply(port,
                   function(x) {
                     jsonlite::fromJSON(paste0(base, x))$results
                   })

    names(ncov) <- port


  ncov
}


conv_ncov <- function(ncov){
  # convert time stamps
  ncov$area$updateTime <- conv_time(ncov$area$updateTime)
  ncov$area$createTime <- conv_time(ncov$area$createTime)
  ncov$area$modifyTime <- conv_time(ncov$area$modifyTime)
  ncov$overall$updateTime <- conv_time(ncov$overall$updateTime)

  # convert area into city
  ncov_area <- ncov$area
  for(i in 1:nrow(ncov_area)){
    if(!is.null(ncov_area$cities[i][[1]])){
      ncov_area$cities[i][[1]]$country <- ncov_area$country[i]
      ncov_area$cities[i][[1]]$provinceName <- ncov_area$provinceName[i]
      ncov_area$cities[i][[1]]$provinceShortName <- ncov_area$provinceShortName[i]
      ncov_area$cities[i][[1]]$updateTime <- ncov_area$updateTime[i]
      ncov_area$cities[i][[1]]$createTime <- ncov_area$createTime[i]
      ncov_area$cities[i][[1]]$modifyTime <- ncov_area$modifyTime[i]
    }
  }
  ncov$cities <- rbind(ncov_area$cities)
  ncov
}

ncov <- get_ncov(port = c('area?latest=0', 'overall', 'provinceName'))
names(ncov)[1] <- 'area'
ncov_tidy <- conv_ncov(ncov)
saveRDS(ncov_tidy, 'data/ncov.RDS')