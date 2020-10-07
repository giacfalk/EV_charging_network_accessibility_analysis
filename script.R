# Electric vehicle charging stations in the EU #

#https://www.overleaf.com/project/5f6b17627995310001c147d4

library(sf)
library(tidyverse)
library(raster)
library(rasterVis)
library(mapdata)
library(maptools)
library(rgdal)
library(rgis)
library(viridis)
library(googledrive)

# devtools::install_version("velox", version = "0.2.0")
# devtools::install_github("Pakillo/rgis")

setwd('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility charging stations')

# create correct ISOs dictionary

states <- read_sf("NUTS_RG_10M_2021_4326_LEVL_0.shp")
nuts <- read_sf("NUTS_RG_10M_2021_4326_LEVL_3.shp")

states$iso2c <- countrycode::countrycode(states$CNTR_CODE, 'iso2c', 'iso2c')
states$iso2c <- ifelse(states$CNTR_CODE=="EL", "GR", states$iso2c)
states$iso2c <- ifelse(states$CNTR_CODE=="UK", "GB", states$iso2c)

states_dict <- states %>% dplyr::select(CNTR_CODE, iso2c)
states_dict$geometry=NULL

##########
# Prepare shapefiles
##########

# Per tipo di uso (cum. annuale)

# Legend of UsageTypeID
#4: Public - Membership, 6: Private, 1: Public, 0: NA

# Filter out "Planned", "Non Operational" and "Removed"

for (i in 2015:2020){
  
  stations <- read.csv('input_data/OpenChargeMap_Europe_stations.csv')
  
  stations$year = substr(stations$DateCreated, 1, 4)
  stations$year = as.numeric(stations$year)
  
  stations <- filter(stations, StatusTypeID!=100 & StatusTypeID!=150 & StatusTypeID!=200)
  
  stations <- filter(stations, year<=i)
  
  stations <- st_as_sf(stations, coords = c("AddressInfo.Longitude", "AddressInfo.Latitude"))
  
  write_sf(stations, paste0("all_stations_", i, "_.shp"))
  
  stations_public <- filter(stations, UsageTypeID==1)
  stations_publicmembership <- filter(stations, UsageTypeID==4)
  stations_private <- filter(stations, UsageTypeID==6)
  
  write_sf(stations_public, paste0("all_stations_public_", i, "_.shp"))
  write_sf(stations_publicmembership, paste0("all_stations_publicmembership_", i, "_.shp"))
  write_sf(stations_private, paste0("all_stations_private_", i, "_.shp"))
}


# Per potenza (cum. annuale)

for (i in 2015:2020){
  
  stations <- read.csv('input_data/OpenChargeMap_Europe_columns.csv')
  
  stations$year = substr(stations$DateCreated, 1, 4)
  stations$year = as.numeric(stations$year)
  
  stations <- filter(stations, StatusTypeID!=100 & StatusTypeID!=150 & StatusTypeID!=200)
  
  stations <- filter(stations, year<=i)
  
  stations <- st_as_sf(stations, coords = c("AddressInfo.Longitude", "AddressInfo.Latitude"))
  
  stations_tier1 <- filter(stations, PowerKW>0)
  stations_tier2 <- filter(stations, PowerKW>10)
  stations_tier3 <- filter(stations, PowerKW>22)
  stations_tier4 <- filter(stations, PowerKW>=50)
  
  write_sf(stations_tier1, paste0("all_stations_tier1_", i, "_.shp"))
  write_sf(stations_tier2, paste0("all_stations_tier2_", i, "_.shp"))
  write_sf(stations_tier3, paste0("all_stations_tier3_", i, "_.shp"))
  write_sf(stations_tier4, paste0("all_stations_tier4_", i, "_.shp"))
  
}


##########
# Process data in GEE
##########

lista <- list.files(pattern = "\\.shp$")
lista <- lista[1:48]

source("accessibility_call_gee.R", echo = T)

##########
# Download files from Google Drive
##########

lista_gd = googledrive::drive_find(pattern = "stations_", n_max=150, orderBy = "createdTime desc")

for (i in lista_gd$name){
  googledrive::drive_download(i)
}

old_files <- list.files(pattern = "traveltime_all_stations", full.names = F)

new_names <- gsub("\\__[^\\]]*\\.tif", "", old_files, perl=TRUE)
new_names <- paste0(new_names, ".tif")

file.rename(old_files, new_names)


##########
# Create accessibility maps 
##########

for (i in as.character(2015:2020)){

s <- raster(paste0("traveltime_all_stations_", i, ".tif"))
s2 <- raster(paste0("traveltime_all_stations_public_", i, ".tif"))
s3 <- raster(paste0("traveltime_all_stations_publicmembership_", i, ".tif"))
s4 <- raster(paste0("traveltime_all_stations_private_", i, ".tif"))

state <- read_sf("NUTS_RG_10M_2021_4326_LEVL_3.shp")

ext <- c(-27, 34, 33, 72)

s <- crop(s, ext)
s <- fast_mask(ras=s, mask=state)

s2 <- crop(s2, ext)
s2 <- fast_mask(ras=s2, mask=state)

s3 <- crop(s3, ext)
s3 <- fast_mask(ras=s3, mask=state)

s4 <- crop(s4, ext)
s4 <- fast_mask(ras=s4, mask=state)

s2 <- projectRaster(s2, s)
s3 <- projectRaster(s3, s)
s4 <- projectRaster(s4, s)

stack <- stack(s, s2, s3, s4)

writeRaster(stack, paste0("rasters", i, ".tif"), overwrite=T)

# Plot with country boundaries at high resolution #

my.at <- c(5, 10, 15, 20, 30, 60, Inf)

myColorkey <- list(at=my.at, ## where the colors change
                   labels=list(
                     at=my.at ## where to print labels
                   ))

ext <- c(-27, 34, 33, 72)
boundaries <- map('worldHires', fill=TRUE,
                  xlim=ext[1:2], ylim=ext[3:4],
                  plot=FALSE)

IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
bPols <- map2SpatialPolygons(boundaries, IDs=IDs,
                             proj4string=CRS(projection(s)))

idx=c("All", "Public", "Public_membership", "Private")
stack <- setZ(stack, idx)
names(stack) <- idx

# png(paste0('maps_tt', i, '.png'), width=2800*2, height=1920*2, res=350, units="px")
# print(levelplot(stack, layout = c(2, 2), at=my.at, colorkey=myColorkey,  par.settings = YlOrRdTheme, main=paste0("Travel minutes to the nearest charging station (", i, ")"), xlab="Longitude", ylab="Latitude", maxpixels = 2e13)+ layer(sp.polygons(bPols)))
# dev.off()
}

# animation

library(magick)
library(magrittr)

# list.files(pattern = 'maps_tt', full.names = TRUE) %>% 
#   image_read() %>% # reads each path file
#   image_join() %>% # joins image
#   image_animate(fps=0.5) %>% # animates, can opt for number of loops
#   image_write("animation.gif") # write to current dir
# 

####

# NUTS-level population weighted TT maps

ext <- c(-27, 34, 33, 72)

vectoreu <- c("AT","BE","BG","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","NL","PL","PT","RO","SK","SI","ES","SE","GB","IS","NO","CH", "UK")

state <- read_sf("NUTS_RG_10M_2021_4326_LEVL_3.shp")
state = merge(state, states_dict, by="CNTR_CODE")
state <- state[state$iso2c %in% vectoreu, ]

s <- raster("traveltime_all_stations_2020.tif")
s <- crop(s, ext)
s <- fast_mask(ras=s, mask=state)

pop <- raster("GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0.tif")
pop <- crop(pop, ext)
pop <- projectRaster(pop, s)

sf <- read_sf("NUTS_RG_10M_2021_4326_LEVL_3.shp")
vectoreu <- c("AT","BE","BG","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","NL","PL","PT","RO","SK","SI","ES","SE","GB","IS","NO","CH", "UK")

sf = merge(sf, states_dict, by="CNTR_CODE")
sf <- sf[sf$iso2c %in% vectoreu, ]

library(exactextractr)
sf$pop <- exact_extract(pop, sf, 'sum', progress=TRUE)
pop_total <- fasterize::fasterize(sf, pop, "pop", fun = "first")
pop_share = pop / pop_total
pop_share_times_tt = pop_share * s

sf$popweighteds = exact_extract(pop_share_times_tt, sf, 'sum', progress=TRUE)

theme_set(theme_bw())

basemap = rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
st_crs(basemap)<-4326

sf$popweighteds_f = ifelse(sf$popweighteds>=0 & sf$popweighteds<=5, "<5", ifelse(sf$popweighteds>5 & sf$popweighteds<=15, "5-15", ifelse(sf$popweighteds>15 & sf$popweighteds<=30, "15-30", ifelse(sf$popweighteds>30 & sf$popweighteds<=60, "30-60", ">60"))))

sf$popweighteds_f <- factor(sf$popweighteds_f, 
                          levels = c('<5', '5-15', '15-30', '30-60', '>60'), 
                          ordered = T)

levels(sf$popweighteds_f) <-c('<5', '5-15', '15-30', '30-60', '>60')

sf = subset(sf, sf$CNTR_CODE!="TR")

fig1_maps = ggplot() + 
  geom_sf(data=basemap, fill="grey", colour="black", lwd=0.001)+
      geom_sf(data = sf, aes(fill = popweighteds_f), colour="black", lwd=0.001)+
  theme_classic()+
  scale_fill_viridis(discrete=TRUE, name="Population weighted \naverage travel time (min)", na.translate=FALSE, direction=-1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_text(angle = 45, vjust = -0.0025))+
  ggtitle('Accessibility to charging stations in 2020 at NUTS-3 level')+
  xlim(c(-28, 32))+
  ylim(c(37.5, 72))

accessibility = dplyr::select(sf, popweighteds)

ggsave("fig1_maps.png", fig1_maps, device="png", width = 4.5, height = 3.3, scale=1.8)

## For tiers of power

list = intersect(list.files(pattern = "tier"), list.files(pattern = "2020.tif"))

listone <- list()
f = 1

for (i in list){
  
  l = substr(i, 25, 29)
  
  ext <- c(-27, 34, 33, 72)
  state <- read_sf("NUTS_RG_10M_2021_4326_LEVL_3.shp")
  
  vectoreu <- c("AT","BE","BG","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","NL","PL","PT","RO","SK","SI","ES","SE","GB","IS","NO","CH", "UK")
  
  state = merge(state, states_dict, by="CNTR_CODE")
  state <- state[state$iso2c %in% vectoreu, ]
  
  s <- raster(i)
  s <- crop(s, ext)
  s <- fast_mask(ras=s, mask=state)
  
  pop <- raster("GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0.tif")
  pop <- crop(pop, ext)
  pop <- projectRaster(pop, s)
  
  sf <- read_sf("NUTS_RG_10M_2021_4326_LEVL_3.shp")
  
  vectoreu <- c("AT","BE","BG","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","NL","PL","PT","RO","SK","SI","ES","SE","GB","IS","NO","CH", "UK")
  
  sf = merge(sf, states_dict, by="CNTR_CODE")
  sf <- sf[sf$iso2c %in% vectoreu, ]
  
  
  library(exactextractr)
  sf$pop <- exact_extract(pop, sf, 'sum', progress=TRUE)
  pop_total <- fasterize::fasterize(sf, pop, "pop", fun = "first")
  pop_share = pop / pop_total
  pop_share_times_tt = pop_share * s
  
  sf$popweighteds = exact_extract(pop_share_times_tt, sf, 'sum', progress=TRUE)
  
  theme_set(theme_bw())
  
  basemap = rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
  st_crs(basemap)<-4326
  
  sf$popweighteds = ifelse(sf$popweighteds>=0 & sf$popweighteds<=5, "<5", ifelse(sf$popweighteds>5 & sf$popweighteds<=15, "5-15", ifelse(sf$popweighteds>15 & sf$popweighteds<=30, "15-30", ifelse(sf$popweighteds>30 & sf$popweighteds<=60, "30-60", ">60"))))
  
  sf$popweighteds <- factor(sf$popweighteds, 
                            levels = c('<5', '5-15', '15-30', '30-60', '>60'), 
                            ordered = T)
  
  levels(sf$popweighteds) <-c('<5', '5-15', '15-30', '30-60', '>60')
  
  sf = subset(sf, sf$CNTR_CODE!="TR")
  
  sf$tier = l
  
  listone[[f]] <- sf
  f = f+1
}

sf <- do.call(rbind, listone)

sf <- st_as_sf(sf)

sf$tier2 <- ifelse(sf$tier=="tier1", "All power tiers", ifelse(sf$tier=="tier2", ">10 KW", ifelse(sf$tier=="tier3", ">22 KW", ifelse(sf$tier=="tier4", ">50 KW", NA))))

sf$tier2  <- factor(sf$tier2 , levels = c("All power tiers", ">10 KW", ">22 KW", ">50 KW"))


fig1_maps = ggplot() + 
  geom_sf(data=basemap, fill="grey", colour="black", lwd=0.001)+
  geom_sf(data = sf, aes(fill = popweighteds), colour="black", lwd=0.001)+
  theme_classic()+
  scale_fill_viridis(discrete=TRUE, name="Population weighted \naverage travel time (min)", na.translate=FALSE, direction=-1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_text(angle = 45, vjust = -0.0025))+
  ggtitle('Accessibility to charging stations in 2020 at NUTS-3 level, by power tier')+
  facet_wrap(~ tier2)+
  xlim(c(-28, 32))+
  ylim(c(37.5, 72))

ggsave("fig1_maps_tiers.png", fig1_maps, device="png", width = 4.5*1.5, height = 3.3*1.5, scale=1.8)


##########
# Maps by mode
##########

list = list.files(pattern = "2020.tif")[2:5]

listone <- list()
f = 1

for (i in list){
  
  l = ifelse(f==1, "All", ifelse(f==2, "Private", ifelse(f==3, "Public", "Public with membership")))
  
  ext <- c(-27, 34, 33, 72)
  state <- read_sf("NUTS_RG_10M_2021_4326_LEVL_3.shp")
  
  vectoreu <- c("AT","BE","BG","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","NL","PL","PT","RO","SK","SI","ES","SE","GB","IS","NO","CH", "UK")
  
  state = merge(state, states_dict, by="CNTR_CODE")
  state <- state[state$iso2c %in% vectoreu, ]
  
  s <- raster(i)
  s <- crop(s, ext)
  s <- fast_mask(ras=s, mask=state)
  
  pop <- raster("GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0.tif")
  pop <- crop(pop, ext)
  pop <- projectRaster(pop, s)
  
  sf <- read_sf("NUTS_RG_10M_2021_4326_LEVL_3.shp")
  
  vectoreu <- c("AT","BE","BG","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","NL","PL","PT","RO","SK","SI","ES","SE","GB","IS","NO","CH", "UK")
  
  sf = merge(sf, states_dict, by="CNTR_CODE")
  sf <- sf[sf$iso2c %in% vectoreu, ]
  
  
  library(exactextractr)
  sf$pop <- exact_extract(pop, sf, 'sum', progress=TRUE)
  pop_total <- fasterize::fasterize(sf, pop, "pop", fun = "first")
  pop_share = pop / pop_total
  pop_share_times_tt = pop_share * s
  
  sf$popweighteds = exact_extract(pop_share_times_tt, sf, 'sum', progress=TRUE)
  
  theme_set(theme_bw())
  
  basemap = rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
  st_crs(basemap)<-4326
  
  sf$popweighteds = ifelse(sf$popweighteds>=0 & sf$popweighteds<=5, "<5", ifelse(sf$popweighteds>5 & sf$popweighteds<=15, "5-15", ifelse(sf$popweighteds>15 & sf$popweighteds<=30, "15-30", ifelse(sf$popweighteds>30 & sf$popweighteds<=60, "30-60", ">60"))))
  
  sf$popweighteds <- factor(sf$popweighteds, 
                            levels = c('<5', '5-15', '15-30', '30-60', '>60'), 
                            ordered = T)
  
  levels(sf$popweighteds) <-c('<5', '5-15', '15-30', '30-60', '>60')
  
  sf = subset(sf, sf$CNTR_CODE!="TR")
  
  sf$tier = l
  
  listone[[f]] <- sf
  f = f+1
}

sf <- do.call(rbind, listone)

sf <- st_as_sf(sf)

sf$mode  <- factor(sf$tier , levels = c("All", "Private", "Public", "Public with membership"))

fig1_maps = ggplot() + 
  geom_sf(data=basemap, fill="grey", colour="black", lwd=0.001)+
  geom_sf(data = sf, aes(fill = popweighteds), colour="black", lwd=0.001)+
  theme_classic()+
  scale_fill_viridis(discrete=TRUE, name="Population weighted \naverage travel time (min)", na.translate=FALSE, direction=-1)+
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_text(angle = 45, vjust = -0.0025))+
  ggtitle('Accessibility to charging stations in 2020 at NUTS-3 level, by access mode')+
  facet_wrap(~ mode)+
  xlim(c(-28, 32))+
  ylim(c(37.5, 72))

ggsave("fig1_maps_mode.png", fig1_maps, device="png", width = 4.5*1.5, height = 3.3*1.5, scale=1.8)

##########
# Create ECDF curves
##########

sf <- read_sf("NUTS_RG_10M_2021_4326_LEVL_0.shp")

vectoreu <- c("AT","BE","BG","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","NL","PL","PT","RO","SK","SI","ES","SE","GB","IS","NO","CH", "UK")

sf = merge(sf, states_dict, by="CNTR_CODE")
sf <- sf[sf$iso2c %in% vectoreu, ]

sf$id = 1:nrow(sf)

id <- fasterize::fasterize(sf, pop, "id", fun = "first")

list = intersect(list.files(pattern = "tier"), list.files(pattern = "2020.tif"))
s1 <- raster(list[1])
s1 <- crop(s1, ext)
s1 <- fast_mask(ras=s1, mask=state)

s2 <- raster(list[2])
s2 <- crop(s2, ext)
s2 <- fast_mask(ras=s2, mask=state)

s3 <- raster(list[3])
s3 <- crop(s3, ext)
s3 <- fast_mask(ras=s3, mask=state)

s4 <- raster(list[4])
s4 <- crop(s4, ext)
s4 <- fast_mask(ras=s4, mask=state)

pop_tt <- stack(s1, s2, s3, s4, pop, id)

pop_tt <- as.data.frame(pop_tt)

pop_tt <- pop_tt[complete.cases(pop_tt), ]

pop_tt <- subset(pop_tt, pop_tt$GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0>0)

sfsf = sf %>% dplyr::select(id, CNTR_CODE) %>% mutate(geometry=NULL)

pop_tt <- merge(pop_tt, sfsf, by.x="layer.5", by.y="id", all.x=T)

pop_tt_eu <- pop_tt
pop_tt_eu$CNTR_CODE="EU-wide"

pop_tt <- rbind(pop_tt, pop_tt_eu)

pop_tt <- subset(pop_tt, pop_tt$CNTR_CODE=="EU-wide" | pop_tt$CNTR_CODE=="DE" | pop_tt$CNTR_CODE=="IT" | pop_tt$CNTR_CODE=="FR" | pop_tt$CNTR_CODE=="ES" | pop_tt$CNTR_CODE=="UK")

stat_ecdf <- function(mapping = NULL, data = NULL,
                      geom = "step", position = "identity",
                      weight =  NULL, 
                      ...,
                      n = NULL,
                      pad = TRUE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatEcdf,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      pad = pad,
      na.rm = na.rm,
      weight = weight,
      ...
    )
  )
}


# #' @rdname ggplot2-ggproto
# #' @format NULL
# #' @usage NULL
# #' @export

StatEcdf <- ggplot2::ggproto("StatEcdf", ggplot2::Stat,
                             compute_group = function(data, scales, weight, n = NULL, pad = TRUE) {
                               # If n is NULL, use raw values; otherwise interpolate
                               if (is.null(n)) {
                                 x <- unique(data$x)
                               } else {
                                 x <- seq(min(data$x), max(data$x), length.out = n)
                               }
                               
                               if (pad) {
                                 x <- c(-Inf, x, Inf)
                               }
                               y <- spatstat::ewcdf(data$x, weights=data$weight / sum(data$weight))(x)
                               
                               data.frame(x = x, y = y)
                             },
                             
                             default_aes = ggplot2::aes(y = ggplot2::stat(y)),
                             
                             required_aes = c("x")
)

a = ggplot(pop_tt, aes(x=layer.1,  weight = GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0, group=CNTR_CODE, colour=CNTR_CODE)) +
  stat_ecdf()+
  coord_cartesian(xlim=c(0, 120))+
  xlab('Travel time (minutes)')+
  ylab('Cumulative fraction \n of the population')+
  scale_x_continuous(breaks = c(0, 5, 15, 30, 60, 120))+
  geom_vline(xintercept=c(0, 5, 15, 30, 60, 120), linetype="dotted")+
  theme_classic()+
  ggtitle("ECDF (all)")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_colour_brewer(name="Country", palette = "Set1")

b = ggplot(pop_tt, aes(x=layer.2,  weight = GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0, group=CNTR_CODE, colour=CNTR_CODE)) +
  stat_ecdf()+
  coord_cartesian(xlim=c(0, 120))+
  xlab('Travel time (minutes)')+
  ylab('Cumulative fraction \n of the population')+
  scale_x_continuous(breaks = c(0, 5, 15, 30, 60, 120))+
  geom_vline(xintercept=c(0, 5, 15, 30, 60, 120), linetype="dotted")+
  theme_classic()+
  ggtitle("ECDF (>10 KW)")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_colour_brewer(name="Country", palette = "Set1")

c = ggplot(pop_tt, aes(x=layer.3,  weight = GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0, group=CNTR_CODE, colour=CNTR_CODE)) +
  stat_ecdf()+
  coord_cartesian(xlim=c(0, 120))+
  xlab('Travel time (minutes)')+
  ylab('Cumulative fraction \n of the population')+
  scale_x_continuous(breaks = c(0, 5, 15, 30, 60, 120))+
  geom_vline(xintercept=c(0, 5, 15, 30, 60, 120), linetype="dotted")+
  theme_classic()+
  ggtitle("ECDF (>22 KW)")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_colour_brewer(name="Country", palette = "Set1")

d = ggplot(pop_tt, aes(x=layer.4,  weight = GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0, group=CNTR_CODE, colour=CNTR_CODE)) +
  stat_ecdf()+
  coord_cartesian(xlim=c(0, 120))+
  xlab('Travel time (minutes)')+
  ylab('Cumulative fraction \n of the population')+
  scale_x_continuous(breaks = c(0, 5, 15, 30, 60, 120))+
  geom_vline(xintercept=c(0, 5, 15, 30, 60, 120), linetype="dotted")+
  theme_classic()+
  ggtitle("ECDF (>50 KW)")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_colour_brewer(name="Country", palette = "Set1")

ecdfs <- cowplot::plot_grid(a, b, c, d, labels = "AUTO")

ggsave("ecdfs.png", ecdfs, device="png", width = 4.5*1.5, height = 3.3*1.5, scale=1.8)


##########
# Plot descriptive graphs
##########

columns <- read_sf("all_stations_tier1_2020_.shp")

library(splitstackshape )

columns$NmbrOfP = ifelse(columns$NmbrOfP<=0 | is.na(columns$NmbrOfP), 1, columns$NmbrOfP)

columns <- expandRows(columns, "NmbrOfP", count.is.col = TRUE, drop = TRUE)

sf <- read_sf("NUTS_RG_10M_2021_4326_LEVL_3.shp")

vectoreu <- c("AT","BE","BG","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","NL","PL","PT","RO","SK","SI","ES","SE","GB","IS","NO","CH", "UK")

sf = merge(sf, states_dict, by="CNTR_CODE")
sf <- sf[sf$iso2c %in% vectoreu, ]

st_crs(columns) <- st_crs(sf)

sf$columns_count <- lengths(st_intersects(sf, columns))

library(exactextractr)
pop <- raster("GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0.tif")
sf$pop = exact_extract(pop, sf, 'sum', progress=TRUE)

theme_set(theme_bw())

basemap = rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
st_crs(basemap)<-4326

sf = subset(sf, sf$CNTR_CODE!="TR")

sf$colcapita <- (sf$columns_count/sf$pop)*10000

sf$colcapita <- ifelse(sf$colcapita==0, NA, sf$colcapita)

sf$colcapita = ifelse(sf$colcapita>0 & sf$colcapita<1, "<1", ifelse(sf$colcapita>=1 & sf$colcapita<3, "1-3", ifelse(sf$colcapita>=3 & sf$colcapita<=6, "3-6", ">6")))

sf$colcapita  <- factor(sf$colcapita , 
                        levels = c('<1', '1-3', '3-6', '>6'), 
                        ordered = T)

levels(sf$colcapita) <-c('<1', '1-3', '3-6', '>6')


fig1_maps = ggplot() + 
  geom_sf(data=basemap, fill="grey", colour="black", lwd=0.001)+
  geom_sf(data = sf, aes(fill = colcapita), colour="black", lwd=0.001)+
  theme_classic()+
  scale_fill_viridis(discrete=T, name="Charging points per 10,000 inhab.", na.translate=FALSE)+
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_text(angle = 45, vjust = -0.0025))+
  xlim(c(-28, 32))+
  ylim(c(37.5, 72))+
  ggtitle('Charging points to population ratio in 2020 at NUTS-3 level')

ggsave("fig_percapita_maps.png", fig1_maps, device="png", width = 4.5, height = 3.3, scale=1.8)


#(1) grafico x-y con tempi medi per nazione e colonnine per abitante

sf <- read_sf("NUTS_RG_10M_2021_4326_LEVL_0.shp")
vectoreu <- c("AT","BE","BG","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","NL","PL","PT","RO","SK","SI","ES","SE","GB","IS","NO","CH", "UK")

sf = merge(sf, states_dict, by="CNTR_CODE")
sf <- sf[sf$iso2c %in% vectoreu, ]


library(exactextractr)
sf$pop <- exact_extract(pop, sf, 'sum', progress=TRUE)
pop_total <- fasterize::fasterize(sf, pop, "pop", fun = "first")
pop_share = pop / pop_total
pop_share_times_tt = pop_share * s

sf$popweighteds = exact_extract(pop_share_times_tt, sf, 'sum', progress=TRUE)

st_crs(columns) <- st_crs(sf)

sf$columns_count <- lengths(st_intersects(sf, columns))

sf = subset(sf, sf$columns_count>0)

a = ggplot(sf, aes(x=popweighteds, y=(columns_count/pop)*10000, colour=CNTR_CODE))+
  geom_point()+
  xlab("Population-weighted country-wide average travel minutes to the nearest charging station")+
  ylab("Charging columns per 10,000 inhabitants")+
  scale_x_log10()+
  scale_y_log10()

#(2) provare a vedere se EV per abitante sono più correlati con colonnine/abitante o tempi medi

EV_fleet <- read.csv('input_data/ACEA_EV_sales.csv', stringsAsFactors = F)

EV_fleet <- data.frame(lapply(EV_fleet, function(x) {
  gsub("-", NA, x)
}))

EV_fleet[4:10] <- data.frame(lapply(EV_fleet[4:10], as.numeric))

EV_fleet$cumulative <- rowSums(EV_fleet[4:10], na.rm = T)

EV_fleet <- subset(EV_fleet, EV_fleet$Type=="BEV")

sf <- merge(sf, EV_fleet, by.x="CNTR_CODE", by.y="Code", all.X=T)

cor(sf$cumulative/sf$pop, (sf$columns_count/sf$pop)*10000)

cor(sf$cumulative/sf$pop, sf$popweighteds)

#(3) confrontare con flotte di EV al variare degli anni

b = ggplot(zzz, aes(x=a, y=b, colour=CNTR_CODE))+
  geom_point()+
  xlab("Population-weighted country-wide average travel minutes to the nearest charging station")+
  ylab("EV fleet per-capita")+
  facet_wrap(~year)


## competition map

sf <- read_sf("NUTS_RG_10M_2021_4326_LEVL_3.shp")
stations <- read_sf("all_stations_tier1_2020_.shp")

st_crs(stations)<-4326
st_crs(sf)<-4326

vectoreu <- c("AT","BE","BG","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","NL","PL","PT","RO","SK","SI","ES","SE","GB","IS","NO","CH", "UK")

sf = merge(sf, states_dict, by="CNTR_CODE")
sf <- sf[sf$iso2c %in% vectoreu, ]

stations <- split(stations, stations$OprtrID)  # split points into list by id

res <- lapply(stations, function(x) 
  lengths(st_intersects(sf, x))
)

res2 <- data.frame(matrix(unlist(res), nrow=length(res), byrow=T))
df_transpose = as.data.frame(t(res2))
df_transpose$providers <- rowSums(df_transpose > 0)

sf$providers = df_transpose$providers

theme_set(theme_bw())

basemap = rnaturalearth::ne_countries(scale = 10, returnclass = "sf")
st_crs(basemap)<-4326

sf = subset(sf, sf$CNTR_CODE!="TR")

sf$providers = ifelse(sf$providers==0, NA, sf$providers)

sf$providers_f = ifelse(sf$providers==1, "1", ifelse(sf$providers==2, "2", ifelse(sf$providers>2 & sf$providers<5, "3-4", ifelse(sf$providers>4 & sf$providers<11, "5-10", ">10"))))

sf$providers_f  <- factor(sf$providers_f , 
                          levels = c('1', '2', '3-4', '5-10', '>10'), 
                          ordered = T)

levels(sf$providers_f) <-c('1', '2', '3-4', '5-10', '>10')

fig1_maps = ggplot() +
  geom_sf(data=basemap, fill="grey", colour="black", lwd=0.001)+
  geom_sf(data = sf, aes(fill = (providers_f)), colour="black", lwd=0.001)+
  theme_classic()+
  scale_fill_viridis(discrete=TRUE, name="Number of unique operators", na.translate=FALSE)+
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x = element_text(angle = 45, vjust = -0.0025), legend.key.size = unit(0.5, "cm"))+
  ggtitle("Competition in the EV charging market at NUTS-3 level")+
  xlim(c(-28, 32))+
  ylim(c(37.5, 72))

providers = dplyr::select(sf, providers)

ggsave("fig3_maps.png", fig1_maps, device="png", width = 4.5, height = 3.3, scale=2)

###
# Correlation between competition and accessibility#
###

bind <- bind_cols(providers, accessibility %>% mutate(geometry=NULL))

cor(bind$providers, bind$popweighteds, , use="complete.obs")

## Cities

cities <- read.csv('D:/OneDrive - FONDAZIONE ENI ENRICO MATTEI/Current papers/Accessibility charging stations/worldcities.csv')

cities = st_as_sf(cities, coords = c("lng", "lat"), crs=4326)

top20citieseu <- c("London", "Berlin", "Madrid", "Rome", "Bucharest", "Paris", "Vienna", "Warsaw", "Hamburg", "Budapest", "Barcelona", "Munich", "Milan", "Prague" , "Sofia", "Cologne", "Stockholm", "Naples", "Turin", "Amsterdam")

cities <- cities[cities$city %in% top20citieseu, ]

cities <- group_by(cities, city) %>% filter(population == max(population))

cities <- st_transform(cities, 3395) %>% st_buffer(dist = 25000) %>% st_transform(4326)

# pop
library(exactextractr)
pop <- raster("GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0.tif")
cities$pop = exact_extract(pop, cities, 'sum', progress=TRUE)

# charging points
columns <- read_sf("all_stations_tier1_2020_.shp")
library(splitstackshape )

columns$NmbrOfP = ifelse(columns$NmbrOfP<=0 | is.na(columns$NmbrOfP), 1, columns$NmbrOfP)

columns <- expandRows(columns, "NmbrOfP", count.is.col = TRUE, drop = TRUE)

st_crs(columns) <- st_crs(cities)

cities$columns_count <- lengths(st_intersects(cities, columns))

# charging points per capita

cities$columns_count_capita <- (cities$columns_count / cities$pop)* 10000

# pop-weighted tt 

ext <- c(-27, 34, 33, 72)

s <- raster("traveltime_all_stations_2020.tif")

cities$popweighteds = exact_extract(s, cities, 'mean', progress=TRUE)

cities <- dplyr::select(cities, pop, columns_count, columns_count_capita, popweighteds)
cities$geometry=NULL

cities$pop <- cities$pop/1000000

colnames(cities) <- c("City", "Population (25 km radius buffer), million", "Charging points", "Charging points per 10,000 inhabs.", "Average travel time")

is.num <- sapply(cities, is.numeric)
cities[is.num] <- lapply(cities[is.num], round, 2)

# generate latex table
library(xtable)
print(xtable(cities), include.colnames=T, booktabs=TRUE)


