
require(dplyr)
require(ggplot2)

source("./code/libs/lib_estates.R")
source("./code/libs/lib_subdistricts.R")
source("./code/libs/lib_islands.R")

build_sub_stx_03 <- function() {
  
  subdist <- subdist_from_func()
  isl <- isl_from_func()
  fips <- islands(isl) %>% pull(county_fips) 
  
  maplims <- numeric()
  maplims["minlat"]<-17.665
  maplims["minlon"]<- -64.7
  maplims["maxlat"]<-17.79
  maplims["maxlon"]<- -64.55
  
  # tmp <- sub_stx_03 %>% paste0(collapse = "', '")
  # tmp <- paste0("'", tmp, "'")
  # 
  
  estates <- c(
    'Catherines Hope', 'South Grapetree Bay', 'South Slob', 'Great Pond', 'North Slob', 
    'Seven Hills', 'Green Cay', 'Hartman', 'Coakley Bay', 'Shoys', 'Tipperary', 
    'Little Profit', 'Mount Fancy', 'Teague Bay', 'North Grapetree Bay', 'Cotton Grove', 
    'Gumbs Land', 'Boetzberg', 'A Piece of Land', 'Southgate', 'Mount Retreat', 
    'Roberts Hill', 'Sallys Fancy', 'Isaacs Bay', 'Cotton Valley', 'Wood Cottage', 
    'Turner Hole', 'Sight', 'Madam Carty', 'Union and Mount Washington', 
    'All for the Better', 'Marienhoj', 'Yellow Cliff North', 'Hope and Carton Hill', 
    'Jacks Bay', 'Long Point and Cotton Garden', 'Mount Pleasant East', 
    'Pleasant Valley', 'Solitude East', 'Yellow Cliff South'
  )
  
  file <- paste0("./data/subdistricts/sub_", isl, "_", subdist, ".rds")
  
  saveRDS(estates, file = file)
  update_subdistricts(isl = isl, estates = estates, as.integer(subdist))
  
}


map_subdist_stx_03 <- function(maplims = NULL, ...) {
  
  require(ggplot2, warn.conflicts = FALSE)
  
  subdist <- subdist_from_func() 
  isl <- isl_from_func() 
  fips <- islands() %>% filter(tolower(Abbrev) == isl) %>% pull(CountyCode) %>% paste0("0", .)
  
  
  if(is.null(maplims)) {
    maplims <- numeric()
    maplims["minlat"]<-17.7
    maplims["minlon"]<- -64.7
    maplims["maxlat"]<-17.79
    maplims["maxlon"]<- -64.55
  }
  
  file <- paste0("./data/subdistricts/sub_", isl, "_", subdist, ".rds")
  
  estates <- readRDS(file = file)
  
  print(ggplot_estates(isl, estates, maplim = maplims, ...))
  
}
