
require(dplyr)
require(ggplot2)

source("./code/libs/lib_estates.R")
source("./code/libs/lib_subdistricts.R")
source("./code/libs/lib_islands.R")

build_sub_stx_01 <- function() {
  
  subdist <- subdist_from_func() 
  isl <- isl_from_func() 
  
  fips <- islands(isl) %>% 
    pull(county_fips)
  
  df_est <- subdistrict_estates() %>% filter(county_fips == fips) %>% 
    arrange(center_lon, center_lat)
  
  srchlims <- numeric()
  srchlims["minlat"]<-17.665
  srchlims["minlon"]<- -64.75
  srchlims["maxlat"]<-17.79
  srchlims["maxlon"]<- -64.65
  
  estates <- df_est %>% slice(44:80) %>% pull(estate) 
  
  estates <- df_est %>% 
    filter(between(center_lon, srchlims["minlon"],srchlims["maxlon"])) %>% 
    filter(between(center_lat,srchlims["minlat"],srchlims["maxlat"]))%>% pull(estate) 
  
  estates <-  c('Boetzberg', 'Roberts Hill', 'Castle Nugent', 'La Presvallee', 'The Springs', 'St. Peters', 'Elizas Retreat', 'Altona', 'Mount Welcome', 'Longford', 'Holgers Hope', 'Recovery and Welcome', 'Altona Fort Louise Augusta', 'Spring Gut', 'Old Hospital Grounds', 'Recovery Hill', 'Grange South', 'Bugby Hole', 'Protestant Cay', 'Christiansted', 'Granard', 'Peters Farm', 'Corn Hill', 'Friedensthal', 'Catherines Rest', 'Contentment', 'Diamond Central', 'Richmond', 'Hermon Hill', 'LBJ Gardens', 'Retreat and Peters Minde', 'Orange Grove East', 'Grange North', 'Golden Rock', 'Cane Garden', 'Beeston Hill', 'Work and Rest')
  
  rms <- c("Christiansted", "Richmond", 'Golden Rock', 'Old Hospital Grounds', 
          'Altona Fort Louise Augusta', 'Protestant Cay','Boetzberg', 'Beeston Hill',
          'Orange Grove East', 'LBJ Gardens', "Roberts ", "La Grange", "Cane Garden")
  
  inc <- c("Annas Hope", "Grange", "Humbug", "Retreat", "Fareham", 
           "Prospect Hill", "Petronella", "Lowry Hill", "Carina", "Cane Garden (B)")
  
  invisible(
    sapply(rms, function(rm) {
      estates <<- estates[!grepl(rm,estates)]
    })
  )
  estates <- c(estates,inc)
  
  
  maplims <- numeric()
  maplims["minlat"]<-17.665
  maplims["minlon"]<- -64.75
  maplims["maxlat"]<-17.79
  maplims["maxlon"]<- -64.65
  
#  print(ggplot_estates("STX",estates, maplims = maplims, subdistricts = T))
  
  saveRDS(estates, file = "./data/subdistricts/sub_stx_01.rds")
  
  update_subdistricts(isl = isl, estates = estates, as.integer(subdist))
}


map_subdist_stx_01 <- function(maplims = NULL, ...) {
  
  require(ggplot2, warn.conflicts = FALSE)
  
  subdist <- subdist_from_func() 
  isl <- isl_from_func() 
  fips <- islands() %>% filter(tolower(Abbrev) == isl) %>% pull(CountyCode) %>% paste0("0", .)
  
  
  if(is.null(maplims)) {
    maplims <- numeric()
    maplims["minlat"]<-17.665
    maplims["minlon"]<- -64.75
    maplims["maxlat"]<-17.79
    maplims["maxlon"]<- -64.65
    
  }
  
  file <- paste0("./data/subdistricts/sub_", isl, "_", subdist, ".rds")
  
  estates <- readRDS(file = file)
  
  print(ggplot_estates(isl, estates, maplim = maplims, ...))
  
}
