
source("./code/libs/lib_shpfiles.R")
source("./code/libs/lib_estates.R")
source("./code/libs/lib_subdistricts.R")
source("./code/libs/lib_subdistrict_estates.R")


## subdistricts

shp_file <- shp_filename(year = 2022, state = "VI", item = "cousub")

shpfile_map  (shp_file, geoms = c(1,2,3, 4, 5, 17, 18, 19, 20)) # STX


## tracts

shp_file <- shp_filename(year = 2022, state = "VI", item = "tract")

shp_map(shp_file)


## bg

shp_file <- shp_filename(year = 2022, state = "VI", item = "bg")

df_shp <- shp_data(shp_file) %>% mutate(geom = row_number()) %>% filter(as.integer(INTPTLAT) < 18.0)

geoms <- df_shp %>% pull(geom)

df_shp_geo <- shp_geodata(shp_file) %>% filter(geom %in% geoms)

shp_map(df_shp_geo)

## addrfeat

year <- 2022

shp_file <- shp_filename(year = 2022, 
                         state = "VI", 
                         county = "STX", 
                         item = "addrfeat", pattern = year)

file.exists(shp_file)

df_shp <- shp_data(shp_file) %>% mutate(geom = row_number()) 
geoms <- df_shp %>% pull(geom)

df_shp_geo <- shp_geodata(shp_file) %>% filter(geom %in% geoms)

shp_map(df_shp_geo)

## unsd

year <- 2022

shp_file <- shp_filename(year = 2022, 
                         state = "VI", 
                         county = NULL, 
                         item = "unsd")

file.exists(shp_file)

df_shp <- shp_data(shp_file) %>% mutate(geom = row_number()) 
geoms <- df_shp %>% pull(geom)

df_shp_geo <- shp_geodata(shp_file) %>% filter(geom %in% geoms)

shp_map(df_shp_geo)

## tabblock20

year <- 2022

shp_file <- shp_filename(year = 2022, 
                         state = "VI", 
                         county = NULL, 
                         item = "tabblock20")

file.exists(shp_file)

df_shp <- shp_data(shp_file) %>% mutate(geom = row_number()) %>% 
  filter(as.integer(INTPTLAT20) < 18)

geoms <- df_shp %>% pull(geom)

df_shp_geo <- shp_geodata(shp_file) %>% filter(geom %in% geoms)

shp_map(df_shp_geo)

