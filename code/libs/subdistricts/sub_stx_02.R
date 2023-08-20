
require(dplyr)
require(ggplot2)

source("./code/libs/lib_estates.R")
source("./code/libs/lib_subdistricts.R")
source("./code/libs/lib_islands.R")
source("./code/libs/lib_subdistrict_estates.R")

build_sub_stx_02 <- function() {
  
  subdist <- subdist_from_func() 
  isl <- isl_from_func() 
  
  fips <- islands(isl) %>% pull(county_fips) 
  
  est <- subdistrict_estate_geodata() 
  
  df_est <- subdistrict_estates() %>%  
    select(estate, county_fips, center_lat, center_lon) %>%  
    filter(county_fips == {{fips}}) %>% 
    filter(between(center_lon, -64.72, -64.70)) %>% 
    filter(between(center_lat,17.737,17.75)) %>% 
    arrange(center_lat,center_lon)
  
  estates <- df_est %>% pull(estate)
  
  #estates <-  paste0("'", paste0(estates,collapse = "', '"), "'")
  
  #estates <-  c('Contentment', 'Peters Farm', 'Friedensthal', 
  #'Orange Grove East', 'Christiansted', 'Richmond', 'LBJ Gardens', 'Protestant Cay')
  
  rms <- c( "LBJ ", "Peters")
  
  inc <- c("Old Hospital Grounds")
  
  estates <- c(estates,inc)
  
  invisible(
    sapply(rms, function(rm) {
      estates <<- estates[!grepl(rm,estates)]
    })
  )

  
  file <- paste0("./data/subdistricts/sub_", isl, "_", subdist, ".rds")
  
  saveRDS(estates, file = file)
  updte_subdistrict_estates(isl = isl, estates = estates, as.integer(subdist))
  
}
