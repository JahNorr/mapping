

year <- 2022

shp_file <- paste0("./data_raw/2022/VI/roads/STX_010/tl_", year,"_78010_roads.shp")

vect <- terra::vect(shp_file) 

df_roads <- as.data.frame(vect) %>% 
  rename(road = FULLNAME) %>% 
  mutate(geom = row_number())

saveRDS(df_roads, "./data/roads.rds")

df_geom <- as.data.frame(terra::geom(vect))
saveRDS(df_geom, "./data/roads_geo.rds")

ggplot(df_geom) + geom_line(aes(x,y, group = interaction(geom, part), color = geom))+
  coord_map()

##  7096 geoms

df_geom1 <- df_geom %>% filter(between(geom, 1, 10))

geom1 <- df_roads %>% filter(grepl("Northside",road)) %>% pull(geom)
df_geom1 <- df_geom %>% filter(geom %in% geom1)

ggplot(df_geom1) + geom_point(aes(x,y, group = interaction(geom, part), color = geom))+
  coord_map()

table(df_geom1$geom)

