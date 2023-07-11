library(ncdf4)
library(dplyr)
library(ggplot2)
library(ggmap)
library(dplyr)
library(lubridate)
library(viridis)

#code for making a map of the ship's position whilst at the sea ice and the sea ice and snow coverage

# Importing cruise data ---------------------------------------------------

Sys.setenv(TZ = 'UTC')

SEANA_dat = read.csv("data/all_data_flagged_one_min.csv") %>% 
  rename(lon = long) %>% 
  mutate(date = ymd_hms(date),
         lon = -lon,
         doy = yday(date)) %>% 
  filter(station == "Sea ice")

# Importing snow depth data -----------------------------------------------

snow_dat = read.csv("data/seaice_snow.csv")

snow_dat1 = snow_dat %>% 
  filter(
    # snow_depth != 130,
    # snow_depth != 120,
    # snow_depth != 110,
    lat > 68.3 & lat < 69.1,
    lon > -57.7 & lon < -56) %>%  
  mutate(snow = case_when(snow_depth == 150 ~ "Variability in snow depth",
                          snow_depth == 160 & sea_ice != 0 ~ "Snowmelt",
                          snow_depth < 100 ~ "Snow on sea ice"))



# Sorting snow and ship data ----------------------------------------------

SEANA_test = SEANA_dat %>% 
  mutate(test = round(lat,digits = 3))
snow_test = snow_dat1 %>% 
  mutate(test = round(lat,digits = 3))
ice_test = new_dat %>% 
  mutate(test = round(lat,digits = 3))

test = left_join(SEANA_test,snow_test,by = "test") %>% 
  # left_join(ice_test,by = "test") %>% 
  mutate(doy_ship = yday(date.x),
         doy_snow = yday(date.y),
         lat.y = ifelse(doy_ship == doy_snow,lat.y,NA),
         lon.y = ifelse(doy_ship == doy_snow,lon.y,NA),
         # lat = ifelse(doy_ship == 159,lat,NA),
         # lon = ifelse(doy_ship == 159,lon,NA),
         traj = ifelse(doy_ship == 159,"yes","no"))

# Importing sea ice data --------------------------------------------------

setwd("D:/Cruise/Sea ice and snow/Sea ice")

ice_var = c("GeolocationData/latitude","GeolocationData/longitude","SeaIceCover_Data/SeaIceCover_Map","SeaIceCover_Data/Algorithm_QA_Flags","SeaIceCover_Data/SeaIceCover_Basic_QA")

nc = nc_open("VNP29.A2022159.0930.001.2022164163225.nc")
#names(nc$var)

for (i in 1:length(ice_var)) {
  vname <- ice_var[i]
  raw <- as.vector(ncdf4::ncvar_get(nc,vname,collapse_degen=FALSE))
  if(i==1){
    dat <- data.frame(raw)
    names(dat) <- vname
  } 
  else {
    dat <- cbind(dat,raw)
    names(dat)[ncol(dat)] <- vname
  }
}

new_dat = dat %>% 
  rename(lat = `GeolocationData/latitude`,
         lon = `GeolocationData/longitude`,
         sea_ice = `SeaIceCover_Data/SeaIceCover_Map`) %>% 
  filter(lat > 68.3 & lat < 69.1,
         lon > -57.7 & lon < -56,
         sea_ice < 101) %>% 
  mutate(sea_ice_flag = case_when(sea_ice == 0 ~ "Open ocean",
                                  sea_ice == 100 ~ "Sea ice"))
unique(new_dat$sea_ice)

# Plotting map ------------------------------------------------------------

bb = c(min(SEANA_dat$lon,na.rm = TRUE),min(SEANA_dat$lat,na.rm = TRUE),max(SEANA_dat$lon,na.rm = TRUE),max(SEANA_dat$lat,na.rm = TRUE))
bb = c(-57.55,68.35,-56.25,68.95)

map = get_stamenmap(bb, zoom = 4)

ggmap(map)+
  geom_point(dat = new_dat,
             aes(lon,lat,col = sea_ice_flag)) +
  scale_colour_manual(values = c("Open ocean" = "#99b3cc","Sea ice" = "white"),guide = "none") +
  geom_path(dat = test,
            aes(lon.x, lat.x),size = 1) +
  geom_point(dat = test,
             aes(lon.y,lat.y,shape = snow),size = 3) +
  theme_bw() +
  labs(shape = "Snow",
       x = NULL,
       y = NULL)

ggsave('sea_ice.png',
       path = 'output',
       units = 'cm')
