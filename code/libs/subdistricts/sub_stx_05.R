
require(dplyr)
require(ggplot2)

source("./code/libs/lib_islands.R")
source("./code/libs/lib_estates.R")
source("./code/libs/lib_subdistricts.R")
source("./code/libs/lib_islands.R")
source("./code/libs/lib_subdistrict_estates.R")


build_sub_stx_05 <- function() {
  
  subdist <- subdist_from_func() 
  isl <- isl_from_func() #
  
  fips <- islands(isl) %>% 
    pull(county_fips) 
  
  est <- subdistrict_estates() 
  
  srchlims <- numeric()
  srchlims["minlat"]<-17.72
  srchlims["maxlat"]<-17.78
  
  srchlims["minlon"]<- -64.82
  srchlims["maxlon"]<- -64.77
  
  
  # srchlims["minlat"]<-17.67
  # srchlims["maxlat"]<-17.8
  # 
  # srchlims["minlon"]<- -64.8
  # srchlims["maxlon"]<- -64.7
  
  df_est <- subdistrict_estates() %>% 
    mutate(lat = center_lat)%>% 
    mutate(lon= center_lon) %>% 
    filter(county_fips == fips) %>%     
    select(estate, subdist_estate,fips = county_fips,lat, lon) %>%  
    filter(between(lon, srchlims["minlon"],srchlims["maxlon"])) %>% 
    filter(between(lat,srchlims["minlat"],srchlims["maxlat"])) 
  
  est_rem <- subdistrict_estates() %>% filter(subdist %in% c(7,8)) %>% pull(subdist_estate) %>% sort()
  
  
  estates <- df_est %>% pull(subdist_estate) %>% 
    sort()
  
  estates <- estates[!estates%in%est_rem]
  
  #paste0("'", paste0(estates %>% sort(),collapse = "', '"), "'")
  
  rms <- c("Clifton", "VI C.*[BCD].$")
  
  inc <- c("River(B)", "Salt River", "Concordia", "Morning Star North")
  
  estates <- c(estates,inc)
  
  invisible(
    sapply(rms, function(rm) {
      estates <<- estates[!grepl(rm,estates)]
    })
  )
  
  #print(estates %>% sort())

  
  offset <- 0.01
  maplims <- numeric()
  maplims["minlat"] <- srchlims["minlat"] - offset
  maplims["maxlat"] <- srchlims["maxlat"] + offset
  
  maplims["minlon"] <- srchlims["minlon"] - offset
  maplims["maxlon"] <- srchlims["maxlon"] + offset
  
  
  file <- paste0("./data/subdistricts/sub_", isl, "_", subdist, ".rds")
  
  saveRDS(estates, file = file)
  updte_subdistrict_estates(isl = isl, estates = estates, as.integer(subdist))
  
}
