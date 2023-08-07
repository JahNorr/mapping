
require(dplyr)

est<-rgdal::readOGR("./data_raw/tl_2022_78_estate.shp")

df_est <- est@data %>% 
  mutate(lat = INTPTLAT)%>% 
  mutate(lon= INTPTLON) %>% 
  rename(estate = NAME) %>% 
  select(estate, island = COUNTYFP,lat, lon)

df_est_stx <- df_est %>%  filter(island == "010") %>% 
  arrange(lon,lat)

df_est_stx %>% pull(estate) %>% gsub("Estate ","", .) %>% sort()

est03 <- df_est_stx %>% slice(1:40) %>% pull(estate) 

str_est03 <-  paste0("'", paste0(est03,collapse = "', '"), "'")

str_est03

lst_est03 <-  c('A Piece of Land', 'Isaacs Bay', 'Long Point and Cotton Garden', 'Jacks Bay', 'South Grapetree Bay', 'North Grapetree Bay', 'South Slob', 'North Slob', 'Turner Hole', 'Teague Bay', 'Catherines Hope', 'Madam Carty', 'Yellow Cliff North', 'Yellow Cliff South', 'Hope and Carton Hill', 'Cotton Valley', 'Wood Cottage', 'Mount Retreat', 'Gumbs Land', 'Little Profit', 'Solitude East', 'Mount Fancy', 'Pleasant Valley', 'Cotton Grove', 'Coakley Bay', 'Seven Hills', 'Great Pond', 'Union and Mount Washington', 'Green Cay', 'Tipperary', 'All for the Better', 'Sight', 'Hartman', 'Southgate', 'Sallys Fancy', 'Marienhoj', 'Petronella', 'Mount Pleasant East', 'Fareham', 'Lowry Hill')

rm <- c('Fareham', 'Lowry Hill', 'Petronella')

inc <- c("Shoys", "Roberts Hill", "Boetz", "Altona")

lst_est03 <- c(lst_est03,inc)
lst_est03 <- lst_est03[!lst_est03 %in% rm]

plot_estate("STX",lst_est03)

saveRDS(lst_est03, file = "./data/sub_stx_03.rds")

