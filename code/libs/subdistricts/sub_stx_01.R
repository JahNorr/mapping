
require(dplyr)


require(dplyr)
require(ggplot2)

source("./code/libs/lib_estates.R")


build_sub_stx_01 <- function() {
  
  subdist <- subdist_from_func() 
  isl <- isl_from_func() 
  
  fips <- islands() %>% 
    filter(tolower(Abbrev) == isl) %>% 
    pull(CountyCode) %>% paste0("0", .)
  
  est <- estate_data() 
  
  df_est <- est %>% as.data.frame() %>% 
    mutate(lat = INTPTLAT)%>% 
    mutate(lon= INTPTLON) %>% 
    rename(estate = NAME) %>% 
    select(estate, fips = COUNTYFP,lat, lon) 
  
  estates <- df_est %>% slice(44:80) %>% pull(estate) 
  
  lst_stx_est01 <-  c('Boetzberg', 'Roberts Hill', 'Castle Nugent', 'La Presvallee', 'The Springs', 'St. Peters', 'Elizas Retreat', 'Altona', 'Mount Welcome', 'Longford', 'Holgers Hope', 'Recovery and Welcome', 'Altona Fort Louise Augusta', 'Spring Gut', 'Old Hospital Grounds', 'Recovery Hill', 'Grange South', 'Bugby Hole', 'Protestant Cay', 'Christiansted', 'Granard', 'Peters Farm', 'Corn Hill', 'Friedensthal', 'Catherines Rest', 'Contentment', 'Diamond Central', 'Richmond', 'Hermon Hill', 'LBJ Gardens', 'Retreat and Peters Minde', 'Orange Grove East', 'Grange North', 'Golden Rock', 'Cane Garden', 'Beeston Hill', 'Work and Rest')
  
  rms <- c("Christiansted", "Richmond", 'Golden Rock', 'Old Hospital Grounds', 
          'Altona Fort Louise Augusta', 'Protestant Cay','Boetzberg', 'Beeston Hill',
          'Orange Grove East', 'LBJ Gardens', "Roberts ", "Prospect")
  
  inc <- c("Annas H","^Grange$", "Humbug", "^Retreat", "Fareham", 
           "Prospect Hill$", "Petronella", "Lowry", "Carina")
  
  estates <- c(estates,inc)
  
  invisible(
    sapply(rms, function(rm) {
      estates <<- estates[!grepl(rm,estates)]
    })
  )
  
  maplims["minlat"]<-17.665
  maplims["minlon"]<- -64.75
  maplims["maxlat"]<-17.79
  maplims["maxlon"]<- -64.65
  
  print(ggplot_estates("STX",estates, maplim = maplims))
  
  saveRDS(lst_stx_est01, file = "./data/sub_stx_01.rds")
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
  
  file <- paste0("./data/sub_", isl, "_", subdist, ".rds")
  
  estates <- readRDS(file = file)
  
  print(ggplot_estates(isl, estates, maplim = maplims, ...))
  
}
