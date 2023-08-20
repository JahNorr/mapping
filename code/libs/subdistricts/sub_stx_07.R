
require(dplyr)
require(ggplot2)

source("./code/libs/lib_estates.R")
source("./code/libs/lib_subdistricts.R")
source("./code/libs/lib_islands.R")
source("./code/libs/lib_subdistrict_estates.R")

build_sub_stx_07 <- function() {
  
  subdist <- subdist_from_func() 
  isl <- isl_from_func() #
  
  fips <- islands(isl) %>% 
    pull(county_fips) 
  
  est <- estate_data_raw() 
  
  srchlims <- numeric()
  srchlims["minlat"]<-17.71
  srchlims["maxlat"]<-17.8
  
  srchlims["minlon"]<- -64.8
  srchlims["maxlon"]<- -64.7
  
  df_est <- subdistrict_estates() %>% 
    mutate(lat = center_lat)%>% 
    mutate(lon= center_lon) %>% 
    filter(county_fips == fips) %>%     
    select(subdist_estate, fips = county_fips,lat, lon) %>%  
    filter(between(lon, srchlims["minlon"],srchlims["maxlon"])) %>% 
    filter(between(lat,srchlims["minlat"],srchlims["maxlat"])) %>% 
    arrange(lat,lon)
  
  
  estates <- df_est %>% pull(subdist_estate)
  
 # paste0("'", paste0(estates %>% sort(),collapse = "', '"), "'")
  
  rms <- c("Christi","Protestant", "La Val", "Clairm", "Salt R", "Morning Star N",
           "Mon B", "Corn", "Grange N", "VI Corp", "Grana", "Concord", "Wind",
           "Colq", "Leba" , "Little [FM]", "Golden G", "Canaa", "Bonne.*N", "Mount ",
           "Betsy", "Work", "Humb", "Cassa", "Pearl", "Glynn", "Body.*2", "Hermon", 
           "Bugby", "Catherin", "Contentm", "Peters", "Cottage", "Cald", "Castle", 
           "Annas", "Orange G", "Richmond", "Friede", "Clifton", "Spanish", 
           "Profit", "Kingshill", "Barren.*E", "Barren.*2")
  
  inc <- c("Barren Spot West (A)")
  
  estates <- c(estates,inc)
  
  invisible(
    sapply(rms, function(rm) {
      estates <<- estates[!grepl(rm,estates)]
    })
  )
  
  offset <- 0.01
  maplims <- numeric()
  maplims["minlat"] <- srchlims["minlat"] - offset*3
  maplims["maxlat"] <- srchlims["maxlat"] + offset
  
  maplims["minlon"] <- srchlims["minlon"] - offset
  maplims["maxlon"] <- srchlims["maxlon"] + offset
  
  
  file <- paste0("./data/subdistricts/sub_", isl, "_", subdist, ".rds")
  
  saveRDS(estates, file)
  
  updte_subdistrict_estates(isl = isl, estates = estates, as.integer(subdist))
  
}
