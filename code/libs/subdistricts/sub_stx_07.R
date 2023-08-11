
require(dplyr)


require(dplyr)
require(ggplot2)

source("./code/libs/lib_estates.R")


build_sub_stx_07 <- function() {
  
  subdist <- subdist_from_func() 
  isl <- isl_from_func() #
  
  fips <- islands() %>% filter(tolower(Abbrev) == isl) %>% 
    pull(CountyCode) %>% 
    paste0("0", .)
  
  est <- estate_data_raw() 
  
  srchlims <- numeric()
  srchlims["minlat"]<-17.71
  srchlims["maxlat"]<-17.8
  
  srchlims["minlon"]<- -64.8
  srchlims["maxlon"]<- -64.7
  
  df_est <- estate_data() %>% 
    mutate(lat = center_lat)%>% 
    mutate(lon= center_lon) %>% 
    filter(fips == county_fips) %>%     
    select(estate, fips = county_fips,lat, lon) %>%  
    filter(between(lon, srchlims["minlon"],srchlims["maxlon"])) %>% 
    filter(between(lat,srchlims["minlat"],srchlims["maxlat"])) %>% 
    arrange(lat,lon)
  
  
  estates <- df_est %>% pull(estate)
  
  paste0("'", paste0(estates,collapse = "', '"), "'")
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
  #estates <-  c('Contentment', 'Peters Farm', 'Friedensthal', 
  #'Orange Grove East', 'Christiansted', 'Richmond', 'LBJ Gardens', 'Protestant Cay')
  
  rms <- c("Christi","Protesta", "La Val", "Clairm", "Salt R", "Morning Star N",
           "Mon B", "Corn", "Grange [^N]", "VI Corp", "Grana", "Concord", "Wind",
           "Colq", "Leba" , "Little", "Golden G", "Canaa", "Bonne", "Mount ",
           "Betsy", "Work", "Humb", "Cassa", "Pearl")
  
  inc <- c()
  
  estates <- c(estates,inc)
  
  invisible(
    sapply(rms, function(rm) {
      estates <<- estates[!grepl(rm,estates)]
    })
  )
  
  print(estates %>% sort())
  
  srchlims["minlat"]<-17.67
  srchlims["maxlat"]<-17.8
  
  srchlims["minlon"]<- -64.8
  srchlims["maxlon"]<- -64.7
  
  offset <- 0.01
  maplims <- numeric()
  maplims["minlat"] <- srchlims["minlat"] - offset
  maplims["maxlat"] <- srchlims["maxlat"] + offset
  
  maplims["minlon"] <- srchlims["minlon"] - offset
  maplims["maxlon"] <- srchlims["maxlon"] + offset
  
  
  file <- paste0("./data/sub_", isl, "_", subdist, ".rds")
  
  print(ggplot_estates(isl, estates, maplim = maplims))
  
  saveRDS(estates, file = file)
  #map_subdist_stx_07()
}



map_subdist_stx_07 <- function(maplims = NULL, ...) {
  
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
  
  file <- paste0("./data/sub_", isl, "_", subdist, ".rds")
  
  estates <- readRDS(file = file)
  
  print(ggplot_estates(isl, estates, maplim = maplims, ...))
  
}
