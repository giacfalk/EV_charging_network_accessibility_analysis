var geometry = ee.FeatureCollection('users/giacomofalchetta/NUTS_RG_10M_2021_4326_LEVL_0')

var inputPoints = ee.FeatureCollection('users/giacomofalchetta/all_stations')

var black = ee.Image(0).byte();

// Paint the city points, essentially converting them to a raster.
// Theoretically this will merge any points that fall within the same pixel (of the resulting 30-arc-second resolution).
var sources = black.paint(inputPoints, 1);
sources = sources.updateMask(sources);

// Compute the min cost path distance map, with a horizon of 1500 km.
// This can be reduced for high-latitude areas and/or to shorten processing time.
var distance = image.cumulativeCost(sources, 1500000);  // The function accepts meters rather than km.
var distance = ee.Image(distance).toInt();  // Here we convert the output to integer to make the output .tif smaller (and the code more likely to run successfully).


var distance = distance.clip(geometry)

Export.image.toDrive({
  image: distance, 
  description: 'traveltime_chargingstation',
  crs: 'EPSG:4326',
  region: geometry,
  scale: 927.662423820733, // This forces the resulting raster to match the resolution of the friction surface (i.e., 30 arc seconds).
                           // Without this GEE could resample strangely and 'break' links (i.e., roads) in the friction surface.
						   // Thus if an accessibility map at a different resolution is required, it is more appropriate to create the map and 30-arc-seconds and subsequently resample it.
  maxPixels: 40000000000,
});


var viz = {
  min: 0,
  max: 180,
  palette: ['00A600','63C600','E6E600','E9BD3A','ECB176','EFC2B3','F2F2F2']
};

//Map.setCenter(11.266, 45.4, 5)
Map.addLayer(distance, viz, 'All')

// set position of panel
var legend = ui.Panel({
style: {
position: 'bottom-left',
padding: '8px 15px'
}
});
 
// Create legend title
var legendTitle = ui.Label({
value: 'Travel time to the nearest charging station (minutes)',
style: {
fontWeight: 'bold',
fontSize: '18px',
margin: '0 0 4px 0',
padding: '0'
}
});
 
// Add the title to the panel
legend.add(legendTitle);
 
// create the legend image
var lon = ee.Image.pixelLonLat().select('latitude');
var gradient = lon.multiply((viz.max-viz.min)/100.0).add(viz.min);
var legendImage = gradient.visualize(viz);
 
// create text on top of legend
var panel = ui.Panel({
widgets: [
ui.Label(viz['max'])
],
});
 
legend.add(panel);
 
// create thumbnail from the image
var thumbnail = ui.Thumbnail({
image: legendImage,
params: {bbox:'0,0,10,100', dimensions:'10x200'},
style: {padding: '1px', position: 'bottom-center'}
});
 
// add the thumbnail to the legend
legend.add(thumbnail);
 
// create text on top of legend
var panel = ui.Panel({
widgets: [
ui.Label(viz['min'])
],
});
 
legend.add(panel);
 
Map.add(legend);

///
 
 var inputPoints = ee.FeatureCollection('users/giacomofalchetta/stations_public')

var black = ee.Image(0).byte();

// Paint the city points, essentially converting them to a raster.
// Theoretically this will merge any points that fall within the same pixel (of the resulting 30-arc-second resolution).
var sources = black.paint(inputPoints, 1);
sources = sources.updateMask(sources);

// Compute the min cost path distance map, with a horizon of 1500 km.
// This can be reduced for high-latitude areas and/or to shorten processing time.
var distance = image.cumulativeCost(sources, 1500000);  // The function accepts meters rather than km.
var distance = ee.Image(distance).toInt();  // Here we convert the output to integer to make the output .tif smaller (and the code more likely to run successfully).


var distance = distance.clip(geometry)

Map.addLayer(distance, viz, 'Public')


Export.image.toDrive({
  image: distance, 
  description: 'traveltime_chargingstation_public',
  crs: 'EPSG:4326',
  region: geometry,
  scale: 927.662423820733, // This forces the resulting raster to match the resolution of the friction surface (i.e., 30 arc seconds).
                           // Without this GEE could resample strangely and 'break' links (i.e., roads) in the friction surface.
						   // Thus if an accessibility map at a different resolution is required, it is more appropriate to create the map and 30-arc-seconds and subsequently resample it.
  maxPixels: 40000000000,
});

///
 
 var inputPoints = ee.FeatureCollection('users/giacomofalchetta/stations_private')

var black = ee.Image(0).byte();

// Paint the city points, essentially converting them to a raster.
// Theoretically this will merge any points that fall within the same pixel (of the resulting 30-arc-second resolution).
var sources = black.paint(inputPoints, 1);
sources = sources.updateMask(sources);

// Compute the min cost path distance map, with a horizon of 1500 km.
// This can be reduced for high-latitude areas and/or to shorten processing time.
var distance = image.cumulativeCost(sources, 1500000);  // The function accepts meters rather than km.
var distance = ee.Image(distance).toInt();  // Here we convert the output to integer to make the output .tif smaller (and the code more likely to run successfully).


var distance = distance.clip(geometry)

Map.addLayer(distance, viz, 'Private')


Export.image.toDrive({
  image: distance, 
  description: 'traveltime_chargingstation_private',
  crs: 'EPSG:4326',
  region: geometry,
  scale: 927.662423820733, // This forces the resulting raster to match the resolution of the friction surface (i.e., 30 arc seconds).
                           // Without this GEE could resample strangely and 'break' links (i.e., roads) in the friction surface.
						   // Thus if an accessibility map at a different resolution is required, it is more appropriate to create the map and 30-arc-seconds and subsequently resample it.
  maxPixels: 40000000000,
});

///
 
 var inputPoints = ee.FeatureCollection('users/giacomofalchetta/stations_publicmembership')

var black = ee.Image(0).byte();

// Paint the city points, essentially converting them to a raster.
// Theoretically this will merge any points that fall within the same pixel (of the resulting 30-arc-second resolution).
var sources = black.paint(inputPoints, 1);
sources = sources.updateMask(sources);

// Compute the min cost path distance map, with a horizon of 1500 km.
// This can be reduced for high-latitude areas and/or to shorten processing time.
var distance = image.cumulativeCost(sources, 1500000);  // The function accepts meters rather than km.
var distance = ee.Image(distance).toInt();  // Here we convert the output to integer to make the output .tif smaller (and the code more likely to run successfully).


var distance = distance.clip(geometry)

Map.addLayer(distance, viz, 'Public (membership)')


Export.image.toDrive({
  image: distance, 
  description: 'traveltime_chargingstation_publicmembership',
  crs: 'EPSG:4326',
  region: geometry,
  scale: 927.662423820733, // This forces the resulting raster to match the resolution of the friction surface (i.e., 30 arc seconds).
                           // Without this GEE could resample strangely and 'break' links (i.e., roads) in the friction surface.
						   // Thus if an accessibility map at a different resolution is required, it is more appropriate to create the map and 30-arc-seconds and subsequently resample it.
  maxPixels: 40000000000,
});