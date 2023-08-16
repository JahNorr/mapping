
require(dplyr)
require(units)
require(ggplot2)


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

geom1 <- df_estates %>% filter(grepl("Clifton",estate)) %>% pull(geom)
geom2 <- df_estates %>% filter(grepl("Body S",estate)) %>% pull(geom)
geom3 <- df_estates %>% filter(grepl("Clifton",estate)) %>% pull(geom)

df_est_01 <- df_estates %>% filter(geom == geom1) 
df_est_02 <- df_estates %>% filter(geom == geom2) 
df_est_03 <- df_estates %>% filter(geom == geom3) 


df_geo_01 <- df_geo %>% filter(geom == 174) 
df_geo_02 <- df_geo %>% filter(geom == 413) 
df_geo_vi <- df_geo %>% filter(geom == 359) 

# df_geo %>% mutate(rn = row_number()) %>% filter(y == min(.$y))
# df_geo %>% mutate(rn = row_number()) %>% filter(x == min(.$x))


##############################################
##
##    VI Corporation Land 1
# ========= vic test  =======================

data_vic1 <- df_geo_vi %>% 
  slice(180:225) 

data_01 <- data_vic1 %>% 
  rbind(df_geo_01,df_geo_02)

ggplot() + 
  geom_polygon(data = data_vic1, aes(x = x, y = y, group = geom, fill =  geom),
               colour = "black") +
  geom_point(data = data_vic1, aes(x = x, y = y, group = geom),
               colour = "yellow", fill =  "red") + coord_map(clip = "on") 
  

ggplot() + 
  geom_polygon(data = data_01, aes(x = x, y = y, group = geom, fill =  geom),
               colour = "black") +  coord_map(clip = "on") 

# ========= vic 01  =======================

data_vic1 <- df_geo_vi %>% 
  slice(18:261) 

data_01 <- data_vic1 %>% 
  rbind(df_geo_01,df_geo_02)

ggplot() + 
  geom_polygon(data = data_vic1, aes(x = x, y = y, group = geom, fill =  geom),
               colour = "black") + coord_map(clip = "on") 

ggplot() + 
  geom_polygon(data = data_01, aes(x = x, y = y, group = geom, fill =  geom),
               colour = "black") +  coord_map(clip = "on") 

# ========= vic 02 =======================

data_vic2 <- df_geo_vi %>% 
  slice(262:304) 

data_02 <- data_vic2 %>% 
  rbind(df_geo_01,df_geo_02)


ggplot() + 
  geom_polygon(data = data_vic2, aes(x = x, y = y, group = geom, fill =  geom),
               colour = "black") +  coord_map(clip = "on") 

ggplot() + 
  geom_polygon(data = data_02, aes(x = x, y = y, group = geom, fill =  geom),
               colour = "black") +  coord_map(clip = "on") 


# ========= vic 03 =======================

data_vic3 <- df_geo_vi %>% 
  slice(312:530) 

data_03 <- data_vic3 %>% 
  rbind(df_geo_01,df_geo_02)


ggplot() + 
  geom_polygon(data = data_vic3, aes(x = x, y = y, group = geom, fill =  geom),
               colour = "black") +  coord_map(clip = "on") 

ggplot() + 
  geom_polygon(data = data_03, aes(x = x, y = y, group = geom, fill =  geom),
               colour = "black") +  coord_map(clip = "on") 

ggplot() + 
  geom_polygon(data = df_geo, aes(x = x, y = y, group = geom, fill =  geom),
               colour = "black") + coord_map(clip = "on") 


# ========= vic 04 =======================

data_vic4 <- df_geo_vi %>% 
  slice(552:601) 

data_04 <- data_vic4 %>% 
  rbind(df_geo_01,df_geo_02)


ggplot() + 
  geom_polygon(data = data_vic4, aes(x = x, y = y, group = geom, fill =  geom),
               colour = "black") +  coord_map(clip = "on") 

ggplot() + 
  geom_polygon(data = data_04, aes(x = x, y = y, group = geom, fill =  geom),
               colour = "black") +  coord_map(clip = "on") 

ggplot() + 
  geom_polygon(data = df_geo, aes(x = x, y = y, group = geom, fill =  geom),
               colour = "black") + coord_map(clip = "on") 


#=====================================================================




ggplot() + 
  geom_polygon(data = df_geo_01, aes(x = x, y = y, group = geom, fill =  geom),
               colour = "black") + coord_map(clip = "on") 

ggplot() + 
  geom_polygon(data = df_geo_02, aes(x = x, y = y, group = geom, fill =  geom),
               colour = "black") + coord_map(clip = "on") 


# ======= everything  ===========

ggplot() + 
  geom_polygon(data = df_geo, aes(x = x, y = y, group = geom, fill =  geom),
               colour = "black") + coord_map(clip = "on") 



estates <- estate_data() %>%  filter(grepl("VI Corp", estate)) %>% pull(estate)

ggplot_estates(isl = "stx", estates = estates)


#ggplot_subdistricts
