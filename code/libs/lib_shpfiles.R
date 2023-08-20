
require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
require(ggplot2, quietly = TRUE, warn.conflicts = FALSE)


shp_data <- function(shp_file) {
  
  shp_geodata_raw(shp_file) %>% 
    as.data.frame()
  
}

shp_geodata_raw <- function(shp_file) {
  
  terra::vect(shp_file) 
  
}

shp_geodata <- function(shp_file) {
  
  terra::geom(shp_geodata_raw(shp_file)) %>% as.data.frame() 
  
}

shpfile_map <- function(shp_file, geoms = NULL, colour = "black", fill = NA) {
  
  require(ggplot2)
  
  shp_geo <- shp_geodata(shp_file)
  
  if(!is.null(geoms)) {
    
    shp_geo<- shp_geo %>% 
      filter(geom %in% geoms)
  }
  
  ggplot(shp_geo,aes(x = x, y = y, group = interaction(geom, part))) +
    geom_polygon( fill = fill, colour = colour)+
    coord_map()  
  
}

shp_map <- function(shp_geo, geoms = NULL, colour = "black", fill = NA) {
  
  if(!is.null(geoms)) {
    
    shp_geo <- shp_geo %>% 
      filter(geom %in% geoms)
  }
  
  ggplot(shp_geo,aes(x = x, y = y, group = interaction(geom, part))) +
    geom_polygon( fill = fill, colour = colour)+
    coord_map()  
  
}

shp_folder <- function(state, county) {
  
  df_fips <- fips::fips_st_2020_txt %>% filter(state_abb == toupper({{state}}))
  st_fips <- fips::state_fips(state)
  state <- state_abbrev(state)
  
  if(!is.null(county)) {
    
    co_fips <- fips::county_fips(state = state, county = county)
    
    if(state == "VI") {
      shp_fldr <- fips::fips_cnty_vi %>% 
        filter(grepl(co_fips, County.Code)) %>% 
        pull(Abbrev) 
      
      shp_fldr <- paste0("./data_raw/2022/", state, "/", shp_fldr, "/")
      
    } else {
      
      shp_fldr <- fips::fips_cnty_2020 %>% 
        filter(State.Code == {{st_fips}}) %>% 
        filter(grepl(county,Area.Name))  %>% 
        pull(Area.Name) %>% 
        gsub(" County", "", .) %>% 
        gsub(" ", "_", .)
      
      shp_fldr <- paste0("./data_raw/2022/", state, "/", shp_fldr, "/")
    }
    
  } else {
    
    shp_fldr <- paste0("./data_raw/2022/", state, "/")
  }
  
  shp_fldr
}

shp_filename <- function(year = 2022, state = "VI", county = NULL, item = NULL, 
                         pattern = NULL) {
  
  
  require(dplyr)
  
  if(is.null(pattern)) pattern <- paste0("rd", year%%100)
  
  if(!is.null(county)) 
    st_fips <- fips::county_fips(state = state, county = county, combine = TRUE) 
  else
    st_fips <- fips::state_fips(state = state)
  
  fldr <- shp_folder(state, county)
  
  res <- paste0(fldr, 
                item, "/tl_", pattern, "_", st_fips ,"_", item ,".shp") 
  
  res
}