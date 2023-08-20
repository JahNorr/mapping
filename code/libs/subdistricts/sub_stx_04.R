
require(dplyr)
require(ggplot2)

source("./code/libs/lib_islands.R")
source("./code/libs/lib_estates.R")
source("./code/libs/lib_subdistricts.R")
source("./code/libs/lib_islands.R")
source("./code/libs/lib_subdistrict_estates.R")


build_sub_stx_04 <- function() {
  
  subdist <- subdist_from_func() 
  isl <- isl_from_func() #
  
  fips <- islands(isl) %>% 
    pull(county_fips) 
  
  est <- subdistrict_estates() 
  
  srchlims <- numeric()
  srchlims["minlat"]<-17.67
  srchlims["maxlat"]<-17.73
  
  srchlims["minlon"]<- -65
  srchlims["maxlon"]<- -64.87
  
  estates <- c("Frederiksted", "Two Brothers")
  
  paste0("'", paste0(estates %>% sort(),collapse = "', '"), "'")
  
  offset <- 0.01
  maplims <- numeric()
  maplims["minlat"] <- srchlims["minlat"] - offset
  maplims["maxlat"] <- srchlims["maxlat"] + offset
  
  maplims["minlon"] <- srchlims["minlon"] - offset
  maplims["maxlon"] <- srchlims["maxlon"] + offset
  
  
  file <- paste0("./data/subdistricts/sub_", isl, "_", subdist, ".rds")
  
  saveRDS(estates, file = file)
  updte_subdistrict_estates(isl = isl, estates = estates, as.integer(subdist))
  
#  print(ggplot_estates(isl, estates, maplim = maplims, subdistricts = T))
  
}

