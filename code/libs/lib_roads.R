
###############################
##
##
##      important roads
##
##      "Centerline", "Northside", "Northshore", "Concordia Rd"



build_roads <- function(year = 2022) {
  
  invisible({
    shp_file <- paste0("./data_raw/2022/VI/roads/STX_010/tl_", year,"_78010_roads.shp")
    
    vect <- terra::vect(shp_file) 
    
    df_roads <- as.data.frame(vect) %>% 
      rename(road = FULLNAME) %>% 
      mutate(geom = row_number())
    
    saveRDS(df_roads, "./data/roads.rds")
    
    df_geom <- as.data.frame(terra::geom(vect))
    saveRDS(df_geom, "./data/roads_geo.rds")
  }) 
}


road_geo <- function(name) {
  
  df_geom <- readRDS("./data/roads_geo.rds")
  df_roads <- readRDS("./data/roads.rds")
  
  ##  7096 geoms
  
  geom1 <- df_roads %>% filter(grepl(name, road)) %>% pull(geom)
  
  df_geom %>% filter(geom %in% geom1)
  
}

road_matches <- function(name) {
  
  roads() %>% filter(grepl(name, road)) %>% pull(road)
}

roads <- function() {
  readRDS("./data/roads.rds")
}

road_geoms <- function(name) {
  
  roads()%>% filter(grepl(name, road)) %>% pull(geom)
  
}

road_geos <- function(name) {
  
  df_geo <- readRDS("./data/roads_geo.rds")
}

road_geom_count <- function(name) {
  
  df_geom <- readRDS("./data/roads_geo.rds")
  df_roads <- readRDS("./data/roads.rds")
  
  geom1 <- df_roads %>% filter(grepl(name, road)) %>% pull(geom)
  
  df_geom %>% filter(geom %in% geom1) %>% pull(geom) %>% unique() %>% length()
}

entire_road_geo <- function(name) {
  
  geoms <- road_geoms(name)
  
  road_geo <- road_geos() 
  df_geos <- data.frame()   
  
  sapply(geoms, function(rdgeom) {
    df_geos <<- df_geos %>% bind_rows(road_geo %>%   filter(geom == rdgeom))
  })
  
  df_geos
}

map_road  <-  function(names) {
  
  df_geos <- data.frame()
  
  sapply(names, function(name) {
    
    df <- entire_road_geo(name)
    
    df_geos <<- df_geos %>% bind_rows(df)
  })
  
  ggplot(df_geos, aes(x=x, y=y, group = geom)) +
    geom_point() + 
    coord_map()
  
}