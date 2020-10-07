library(rgee)
ee_Initialize(email="giacomo.falchetta@gmail.com", gcs = TRUE)

sf <- read_sf("NUTS_RG_10M_2021_4326_LEVL_0.shp")

#https://github.com/r-spatial/rgee/issues/105
#https://console.cloud.google.com/storage/browser/accessibility;tab=objects?forceOnBucketsSortingFiltering=false&project=sonic-wonder-244312&prefix=&forceOnObjectsSortingFiltering=false

for (i in lista){
print(match(c(i),lista)/length(lista))
# a = read_sf(i)
# 
# st_crs(a) <- st_crs(sf)
# 
# stations_private <- sf_as_ee(
#   x=a,
#   via = "gcs_to_asset",
#   bucket = "accessibility",
#   assetId = sprintf("%s/%s", ee_get_assethome(), gsub("_.shp", "", i)),
#   proj = "EPSG:4326",
#   overwrite = TRUE)
# 

image = ee$Image("Oxford/MAP/friction_surface_2015_v1_0")

geometry = ee$FeatureCollection('users/giacomofalchetta/NUTS_RG_10M_2021_4326_LEVL_0')

paste0(sprintf("%s/%s", ee_get_assethome(), gsub(".tif", "", i)), "_")

inputPoints = ee$FeatureCollection(sprintf("%s/%s", ee_get_assethome(), gsub(".shp", "", i)))

black = ee$Image(0)$byte()

sources = black$paint(inputPoints, 1)

sources = sources$updateMask(sources)

distance = image$cumulativeCost(sources, 1500000)  

distance = ee$Image(distance)$toInt()

distance = distance$clip(geometry)

task_img <- ee_image_to_drive(
  image = distance,
  fileFormat = "GEO_TIFF",
  region = geometry$geometry(),
  fileNamePrefix = paste0("traveltime_", gsub(".shp", "", i)),
  scale = 4638.312,
  maxPixels = 49995643,
  crs = 'EPSG:4326'
)

task_img$start()
}
