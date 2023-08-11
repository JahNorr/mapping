


require(dplyr)
require(ggplot2)

source("./code/libs/lib_estates.R")
source("./code/libs/lib_estates_subdist.R")

map_subdistricts <- function(label = TRUE) {
  est <- estate_data() #%>% as.data.frame()
  
  # if(class(est) == "SpatVector") 
  df_est <- est %>% as.data.frame() %>% 
    mutate(subdist=0) %>% 
    mutate(lat = INTPTLAT)%>% 
    mutate(lon= INTPTLON) %>% 
    rename(estate = NAME) %>% 
    select(estate, subdist, fips = COUNTYFP,lat, lon)
  
  isl  <-  "stx"
  fips <- "010"
  
  
  invisible(
    sapply(1:8, function(index) {
      
      file <- paste0("./data/sub_", isl, "_", sprintf("%02d",index), ".rds")
      
      if(file.exists(file)) {
        estates <- readRDS(file)
        
        my_estates <- which_estates(df_est, estates, fips)
        df_est <<- df_est %>% mutate(subdist = ifelse(row_number() %in% my_estates , index, subdist))
      }
    })
  )
  
  df_estates <- df_est %>% 
    mutate(geom = row_number())
  
  
  
  # maplims["minlat"]<-17.665
  # maplims["minlon"]<- -64.75
  # maplims["maxlat"]<-17.79
  # maplims["maxlon"]<- -64.65
  
  isl <- "stx"
  maplims <- island_maplims(isl)
  
  print(ggplot_subdistricts(isl,df_estates, maplims = maplims, label = label, lbl_size = 2.0))
  
}
