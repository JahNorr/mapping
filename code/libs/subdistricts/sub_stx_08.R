
require(dplyr)
require(ggplot2)

source("./code/libs/lib_estates.R")
source("./code/libs/lib_subdistricts.R")
source("./code/libs/lib_islands.R")

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
  
  paste0("'", paste0(estates %>% sort(),collapse = "', '"), "'")
  
  # 'Bugby Hole', 'Barren Spot West', 'Annas Hope', 'Grove Place', 'Upper Love',
  #' 'Diamond East', 'Grange North', 'Strawberry Hill', 'Springfield', 
  #' 'Sion Farm', 'La Reine', 'Montpellier West 2', 'Body Slob', 
  #' 'Hermon Hill', 'Plessen North', 'Grange Hill', 'Constitution Hill', 
  #' 'Contentment', 'Peters Farm', 'Ruby', 'Bulows Minde', 'Beeston Hill', 
  #' 'River', 'Two Friends', 'Friedensthal', 'Sion Hill', 'Hafensight', 
  #' 'Hard Labor', 'Orange Grove East', 'Bonne Esperance South',
  #'  'Colquohoun', 'Thomas', 'Christiansted', 'Richmond', 'Bellevue
  #'  Marys Fancy', 'Mon Bijou', 'Little Princess South', 'LBJ Gardens',
  #'   'Protestant Cay'
  #'   
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
           "Cane Garden (A)",
           "VI Corporation Land (B)", 
           "VI Corporation Land (C)", "VI Corporation Land (D)", "Diamond West (A)")
  
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



map_subdist_stx_08 <- function(maplims = NULL, ...) {
  
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
