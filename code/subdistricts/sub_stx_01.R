
require(dplyr)
require(ggplot2)


shp_file <- "./data_raw/tl_2022_78_estate.shp"
#est<-rgdal::readOGR(shp_file )

est <- estate_data(useOGR = F) #%>% as.data.frame()

# if(class(est) == "SpatVector") 
df_est <- est %>% as.data.frame() #else
#  df_est <- est@data


df_est <- df_est %>% 
  mutate(lat = INTPTLAT)%>% 
  mutate(lon= INTPTLON) %>% 
  rename(estate = NAME) %>% 
  select(estate, island = COUNTYFP,lat, lon)

df_est_stx <- df_est %>%  filter(island == "010") %>% 
  arrange(lon,lat)

df_est_stx %>% pull(estate) %>% gsub("Estate ","", .) %>% sort()

est01 <- df_est_stx %>% slice(44:80) %>% pull(estate) 

str_est01 <-  paste0("c('", paste0(est01,collapse = "', '"), "')")

str_est01

lst_est01 <-  c('Boetzberg', 'Roberts Hill', 'Castle Nugent', 'La Presvallee', 'The Springs', 'St. Peters', 'Elizas Retreat', 'Altona', 'Mount Welcome', 'Longford', 'Holgers Hope', 'Recovery and Welcome', 'Altona Fort Louise Augusta', 'Spring Gut', 'Old Hospital Grounds', 'Recovery Hill', 'Grange South', 'Bugby Hole', 'Protestant Cay', 'Christiansted', 'Granard', 'Peters Farm', 'Corn Hill', 'Friedensthal', 'Catherines Rest', 'Contentment', 'Diamond Central', 'Richmond', 'Hermon Hill', 'LBJ Gardens', 'Retreat and Peters Minde', 'Orange Grove East', 'Grange North', 'Golden Rock', 'Cane Garden', 'Beeston Hill', 'Work and Rest')

rm <- c("Christiansted", "Richmond", 'Golden Rock', 'Old Hospital Grounds', 'Boetz')

inc <- c("Annas H","^Grange$")

lst_est01 <- c(est01,inc) %>% unique()
lst_est01 <- lst_est01[!lst_est01 %in% rm]

ggplot_estates <- function(isl=c("STX","STT","STJ"),estates = NULL,color_on="#BBBBBBFF",color_off="#00000000",verbose=FALSE, year = 2022) {
  
  #===============================================
  #
  # mapping
  #
  #
  require(maps,quietly = T)
  require(mapdata,quietly = T)
  require(maptools,quietly = T)  #for shapefiles
  require(scales,quietly = T)  #for transparency
  require(rgdal,quietly = T)
  
  
  ##############################################################
  ##
  ##      use ggplot2
  
  shp_file <- "./data_raw/tl_2022_78_estate.shp"
  #est<-rgdal::readOGR(shp_file )
  
  est <- estate_data(useOGR = T) #%>% as.data.frame()
  
  # if(class(est) == "SpatVector")
  df_est <- est %>% as.data.frame() #else
  
  islands<-c("St. Croix","St. Thomas","St. John")
  isls<-c("STX","STT","STJ")
  fips<-c("010","030","020")
  
  isl<-isl[1]
  isln<-which(isls==isl)
  
  island <- islands[isln]
  fips<-fips[isln]
  
  df_est <- est %>% as.data.frame() %>%  select(NAME,COUNTYFP)
  
  est_nums<-
    which(
      mapply(function(nm,fp) {
        
        x <- sapply(estates, function(est_in) {
          grepl(est_in,nm) && fp == fips
        })
        x <- any(x)
      }, df_est$NAME, df_est$COUNTYFP)
      
    )
  
  lim<-c(
    c(17.67,17.8,-64.90,-64.57),
    c(18.3,18.4,-65.047,-64.823),
    c(18.298,18.373,-64.81,-64.64)
  )
  
  limits<-matrix(lim,4,3)
  colnames(limits)<- isls
  rownames(limits)<- c("minlat","maxlat","minlon","maxlon")
  
  lim_tmp<-as.data.frame(limits[,isl])
  
  maplims<-numeric(4)
  names(maplims)<-rownames(limits)
  maplims["minlat"]<-min(lim_tmp["minlat",])
  maplims["minlon"]<-min(lim_tmp["minlon",])
  maplims["maxlat"]<-max(lim_tmp["maxlat",])
  maplims["maxlon"]<-max(lim_tmp["maxlon",])
  
  colors<-rep(color_off,length(est))
  
  colors[est_nums]<-color_on
  browser()
  show.axes<-FALSE
  
  ggplot2::ggplot() + geom_polygon(data = est, aes(x = long, y = lat, group = est@polygons, fill = est@data$COUNTYFP),
                                   colour = "black") +
    coord_map() +
    
    ylim(maplims["minlat"],maplims["maxlat"]) +
    xlim(maplims["minlon"],maplims["maxlon"]) +
    theme(panel.background = element_blank())
  
}

ggplot_estates("STX",lst_est01)

length(est@polygons)
