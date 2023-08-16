
require(dplyr)
require(units)


df <- estate_data() %>% 
  filter(county_fips == "010") %>% 
  filter(grepl("^VI |Body|Clifton", estate)) 

estates <- df %>% pull(estate)

maplims <- numeric()
maplims["minlat"] <- 17.69
maplims["maxlat"] <- 17.76

maplims["minlon"] <- -64.83
maplims["maxlon"] <- -64.77

pi_rad <- as_units(pi, "radians")
pi_deg <- set_units(pi_rad, "degrees")
set_units(pi_deg, "radians")

ggplot_estates("STX",estates, show_others = F, maplims = maplims)

df_geo <- estate_geodata() %>% filter(geom %in% df$geom) #

#%>% 
# mutate(next_x = c(.$x[2:nrow(.)],.$x[1]))%>% 
# mutate(next_y = c(.$y[2:nrow(.)],.$y[1])) %>% 
# mutate(dy = (next_y - y)*1000) %>% 
# mutate(dx = (next_x - x)*1000) %>% 
# mutate(dir = atan(dy/dx)) %>% 
# mutate(deg = set_units(as_units(dir,"radian"), "degrees"))

df_geo_01 <- df_geo %>% filter(geom == 174) 
df_geo_bslob <- df_geo %>% filter(geom == 413) 
df_geo_vi <- df_geo %>% filter(geom == 359) 

# df_geo %>% mutate(rn = row_number()) %>% filter(y == min(.$y))
# df_geo %>% mutate(rn = row_number()) %>% filter(x == min(.$x))


##############################################
##
##    Body Slob

data_bs <- df_geo_bslob %>% 
  slice(36:109)  #109 rows 

  
  ggplot2::ggplot() + 
  geom_polygon(data = data_bs, aes(x = x, y = y, group = geom, fill =  geom),
               colour = "black") +  coord_map(clip = "on") 

