
require(dplyr)
require(ggplot2)

source("./code/libs/lib_estates.R")
source("./code/libs/lib_subdistricts.R")
source("./code/libs/lib_islands.R")
source("./code/libs/lib_subdistrict_estates.R")

build_sub_stx_08 <- function() {
  
  subdist <- subdist_from_func() 
  isl <- isl_from_func() #
  
  fips <- islands(isl) %>% 
    pull(county_fips) 
  
  est <- subdistrict_estates() 
  
  srchlims <- numeric()
  srchlims["minlat"]<-17.68
  srchlims["maxlat"]<-17.74
  
  srchlims["minlon"]<- -64.84
  srchlims["maxlon"]<- -64.71
  
  
  # srchlims["minlat"]<-17.67
  # srchlims["maxlat"]<-17.8
  # 
  # srchlims["minlon"]<- -64.8
  # srchlims["maxlon"]<- -64.7
  
  df_est <- subdistrict_estates() %>% 
    mutate(lat = center_lat)%>% 
    mutate(lon= center_lon) %>% 
    filter(fips == county_fips) %>%     
    select(estate, fips = county_fips,lat, lon) %>%  
    filter(between(lon, srchlims["minlon"],srchlims["maxlon"])) %>% 
    filter(between(lat,srchlims["minlat"],srchlims["maxlat"])) %>% 
    arrange(lat,lon)
  
  
  estates <- df_est %>% pull(estate)
  
  #paste0("'", paste0(estates %>% sort(),collapse = "', '"), "'")
  
  estates <-  c('Anguilla', 'Annaberg and Shannon Grove', 'Barren Spot West', 'Bettys Hope', 
                'Blessing', 'Caldwell', 'Cane South', 'Carlton South', 'Cassava Garden', 
                'Castle Coakley', 'Clifton Hill', 'Coopers', 'Cottage', 'Diamond West', 
                'Enfield Green', 'Envy', 'Golden Grove', 'Grove Place', 'Hope East', 
                'Hope West', 'Jerusalem and Figtree Hill', 'Kingshill', 'Lower Love', 
                'Mannings Bay', 'Mint', 'Mount Pleasant South', 'Mountain', 'Negro Bay', 
                'Paradise', 'Plessen South', 'Profit', 'Public Port', 'Spanish Town', 
                'St. Georges', 'VI Corporation Land', 'Williams Delight')
  
  rms <- c('Barren Spot West .A.', "Plessen", "^Grove", "Enfield", "Carlton", "Cane .*[^A].$", "William",
           "Mountain", "^Hope [^E]", "George", "Love", "Mint", 
           "VI Corporation Land .A.", 
           "VI Corporation Land$")
  
  inc <- c("Pearl", "Barren Spot East", "Barren Spot West (B)",
           "Cane Garden (A)", "Peters Rest (B)",
           "VI Corporation Land (B)", 
           "VI Corporation Land (C)", "VI Corporation Land (D)", "Diamond West (A)")
  
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
