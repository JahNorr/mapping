
require(dplyr)


require(dplyr)
require(ggplot2)

source("./code/libs/lib_estates.R")


build_sub_stx_02 <- function() {
  
  subdist <- subdist_from_func() 
  isl <- isl_from_func() 
  fips <- islands() %>% filter(tolower(Abbrev) == isl) %>% pull(CountyCode) %>% paste0("0", .)
  
  est <- estate_data() 
  
  df_est <- est %>% as.data.frame() %>% 
    mutate(lat = INTPTLAT)%>% 
    mutate(lon= INTPTLON) %>% 
    rename(estate = NAME) %>% 
    select(estate, fips = COUNTYFP,lat, lon) %>%  
    filter(fips == {{fips}}) %>% 
    filter(between(lon, -64.72, -64.70)) %>% 
    filter(between(lat,17.737,17.75)) %>% 
    arrange(lat,lon)
  
  estates <- df_est %>% pull(estate)
  
  #estates <-  paste0("'", paste0(estates,collapse = "', '"), "'")
  
  #estates <-  c('Contentment', 'Peters Farm', 'Friedensthal', 
  #'Orange Grove East', 'Christiansted', 'Richmond', 'LBJ Gardens', 'Protestant Cay')
  
  rms <- c("^Orange ", "LBJ ", "Peters")
  
  inc <- c("Old Hospital Grounds")
  
  estates <- c(estates,inc)
  
  invisible(
    sapply(rms, function(rm) {
      estates <<- estates[!grepl(rm,estates)]
    })
  )

  
  file <- paste0("./data/sub_", isl, "_", subdist, ".rds")
  
  saveRDS(estates, file = file)
  
}

map_subdist_stx_02 <- function(maplims = NULL, ...) {
  
  require(ggplot2, warn.conflicts = FALSE)
  
  subdist <- subdist_from_func() 
  isl <- isl_from_func()
  fips <- islands() %>% filter(tolower(Abbrev) == isl) %>% pull(CountyCode) %>% paste0("0", .)
  
  
  if(is.null(maplims)) {
    maplims <- numeric()
    maplims["minlat"]<-17.734
    maplims["minlon"]<- -64.72
    maplims["maxlat"]<-17.765
    maplims["maxlon"]<- -64.675
  }
  
  file <- paste0("./data/sub_", isl, "_", subdist, ".rds")
  
  estates <- readRDS(file = file)
  
  print(ggplot_estates(isl, estates, maplim = maplims, ...))
  
}
