
require(dplyr)
require(ggplot2)

source("./code/libs/lib_estates.R")

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

stx_est01 <-  paste0("c('", paste0(est01,collapse = "', '"), "')")

stx_est01

lst_stx_est01 <-  c('Boetzberg', 'Roberts Hill', 'Castle Nugent', 'La Presvallee', 'The Springs', 'St. Peters', 'Elizas Retreat', 'Altona', 'Mount Welcome', 'Longford', 'Holgers Hope', 'Recovery and Welcome', 'Altona Fort Louise Augusta', 'Spring Gut', 'Old Hospital Grounds', 'Recovery Hill', 'Grange South', 'Bugby Hole', 'Protestant Cay', 'Christiansted', 'Granard', 'Peters Farm', 'Corn Hill', 'Friedensthal', 'Catherines Rest', 'Contentment', 'Diamond Central', 'Richmond', 'Hermon Hill', 'LBJ Gardens', 'Retreat and Peters Minde', 'Orange Grove East', 'Grange North', 'Golden Rock', 'Cane Garden', 'Beeston Hill', 'Work and Rest')

rm <- c("Christiansted", "Richmond", 'Golden Rock', 'Old Hospital Grounds', 
        'Altona Fort Louise Augusta', 'Protestant Cay','Boetzberg', 'Beeston Hill',
        'Orange Grove East', 'LBJ Gardens', "Roberts ", "Prospect")

inc <- c("Annas H","^Grange$", "Humbug", "^Retreat", "Fareham", 
         "Prospect Hill$", "Petronella", "Lowry", "Carina")

lst_stx_est01 <- c(est01,inc) %>% unique()
lst_stx_est01 <- lst_stx_est01[!lst_stx_est01 %in% rm]

maplims["minlat"]<-17.665
maplims["minlon"]<- -64.75
maplims["maxlat"]<-17.79
maplims["maxlon"]<- -64.65

print(ggplot_estates("STX",lst_stx_est01, maplim = maplims))

saveRDS(lst_stx_est01, file = "./data/sub_stx_01.rds")

# length(est@polygons)
# 
# 
# n <- 50
# 
# 
# polyg <- est@polygons[n]
# porder <- est@plotOrder[n] 
# 
# df <- est@data[n,]
# 
# df %>% mutate(index = 1:nrow(est@data))%>% 
#   filter(as.numeric(INTPTLAT)>17.7206 & as.numeric(INTPTLAT)<17.72075)
# 
# str(polyg)
# polyg[[1]]@labpt
# 
# rdrr.io::   est@ptr$add_column_factor()

