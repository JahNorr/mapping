
require(dplyr)
require(ggplot2)

source("./code/libs/lib_estates.R")
source("./code/libs/lib_subdistricts.R")
source("./code/libs/lib_islands.R")


build_sub_stx_09 <- function() {
  
  subdist <- subdist_from_func() 
  isl <- isl_from_func() #
  
  fips <- islands(isl) %>% 
    pull(county_fips) 
  
  est <- subdistrict_estates() 
  
  srchlims <- numeric()
  srchlims["minlat"]<-17.68
  srchlims["maxlat"]<-17.72
  
  srchlims["minlon"]<- -64.91
  srchlims["maxlon"]<- -64.825
  
  
  # srchlims["minlat"]<-17.67
  # srchlims["maxlat"]<-17.8
  # 
  # srchlims["minlon"]<- -64.8
  # srchlims["maxlon"]<- -64.7
  
  df_est <- subdistrict_estates() %>% 
    mutate(lat = center_lat)%>% 
    mutate(lon= center_lon) %>% 
    filter(county_fips == fips) %>%     
    select(estate, fips = county_fips,lat, lon) %>%  
    filter(between(lon, srchlims["minlon"],srchlims["maxlon"])) %>% 
    filter(between(lat,srchlims["minlat"],srchlims["maxlat"])) %>% 
    arrange(lat,lon)
  
  
  estates <- df_est %>% pull(estate)
  
  paste0("'", paste0(estates %>% sort(),collapse = "', '"), "'")
  
  # 'Cane', 'Cane South', 'Cane Valley', 'Carlton North', 'Carlton South', 'Concordia South', 
  # 'Concordia West', 'Diamond West', 'Enfield Green', 'Frederikshaab', 'Frederiksted', 
  # 'Hannahs Rest', 'Hogensberg', 'La Grange', 'Mars Hill and Stoney Ground', 'Mint', 
  # 'Mountain', 'St. Georges Hill', 'Stoney Ground East', 'Stoney Ground West', 'The Whim East', 
  # 'The Whim West', 'Two Brothers', 'Wheel of Fortune', 'White Lady', 'Whites Bay East', 
  # 'Whites Bay North', 'Whites Bay South', 'Williams Delight'
  
  rms <- c("La Gr", "Freder", "Wheel", "Concord.*W", "Mars", "Cane V", "St. Geor",
           "Hogen", "Two Bro", "Mountain", "Mint")
  
  inc <- c("Diamond West (B)", "Whim East (B)", "Carlton North (A)")
  
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
  update_subdistricts(isl = isl, estates = estates, as.integer(subdist))
  
  #print(ggplot_estates(isl, estates, maplim = maplims, subdistricts = T))
  
}



map_subdist_stx_09 <- function(maplims = NULL, ...) {
  
  require(ggplot2, warn.conflicts = FALSE)
  
  subdist <- subdist_from_func()
  isl <- isl_from_func()
  fips <- islands() %>% filter(tolower(Abbrev) == isl) %>% pull(CountyCode) %>% paste0("0", .)
  
  
  if(is.null(maplims)) {
    maplims <- numeric()
    maplims["minlat"]<-17.67
    maplims["minlon"]<- -64.8
    maplims["maxlat"]<-17.8
    maplims["maxlon"]<- -64.7
    
  }
  
  file <- paste0("./data/subdistricts/sub_", isl, "_", subdist, ".rds")
  
  estates <- readRDS(file = file)
  
  print(ggplot_estates(isl, estates, maplim = maplims, ...))
  
}
