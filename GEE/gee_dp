// the geometry of the objects (point/ polygon) whether were imported as an asset in the shapefile format
// or the user defined them himself by clicking on the interactive map in the GEE working environment


//Predefined parameters for clouds and cloud shadows masking
var CLOUD_FILTER = 60;
var CLD_PRB_THRESH = 40;
var NIR_DRK_THRESH = 0.15;
var CLD_PRJ_DIST = 1;
var BUFFER = 45;


//Load S2 cloud probability collection
var s2_cloudless_col = ee.ImageCollection('COPERNICUS/S2_CLOUD_PROBABILITY')
        .filterBounds(habr)
        .filterDate('2017-03-28','2020-11-30');
// print('Cloud probability collection:',s2_cloudless_col);        

//Load S2 collection
var filtered = S2.filterDate('2017-03-28','2020-11-30')
.filterBounds(habr)
.filter(ee.Filter.lte('CLOUDY_PIXEL_PERCENTAGE', 50));
// print('Filtered collection:',filtered);

//Join the collections 
var col = ee.ImageCollection(ee.Join.saveFirst('s2cloudless').apply
({'primary': filtered,
    'secondary': s2_cloudless_col,
    'condition': ee.Filter.equals({
    'leftField': 'system:index',
    'rightField': 'system:index'
    })
  }));
print('Joined collection:',col);

//Define cloud mask functions
function add_cloud_bands(img){
  //Get s2cloudless image, subset the probability band.
  var cld_prb = ee.Image(img.get('s2cloudless')).select('probability');
  
  //Condition s2cloudless by the probability threshold value.
  var is_cloud = cld_prb.gt(CLD_PRB_THRESH).rename('clouds');
  
  //Add the cloud probability layer and cloud mask as image bands.
  return img.addBands(ee.Image([cld_prb, is_cloud]));
}  

function add_shadow_bands(img){
  //Identify water pixels from the SCL band.
  var not_water = img.select('SCL').neq(6);
  
  //Identify dark NIR pixels that are not water (potential cloud shadow pixels).
  var SR_BAND_SCALE = 1e4;
  var dark_pixels = img.select('B8').lt(NIR_DRK_THRESH*SR_BAND_SCALE).multiply(not_water).rename('dark_pixels');
  
  //Determine the direction to project cloud shadow from clouds (assumes UTM projection).
  var shadow_azimuth = ee.Number(90).subtract(ee.Number(img.get('MEAN_SOLAR_AZIMUTH_ANGLE')));
  
  //Project shadows from clouds for the distance specified by the CLD_PRJ_DIST input.
  var cld_proj = img.select('clouds').directionalDistanceTransform(shadow_azimuth, CLD_PRJ_DIST*10)
  .reproject({'crs': img.select(0).projection(), 'scale': 100})
  .select('distance')
  .mask()
  .rename('cloud_transform');

  //Identify the intersection of dark pixels with cloud shadow projection.
  var shadows = cld_proj.multiply(dark_pixels).rename('shadows');

  //Add dark pixels, cloud projection, and identified shadows as image bands.
  return img.addBands(ee.Image([dark_pixels, cld_proj, shadows]));
}

function add_cld_shdw_mask(img){
  //Add cloud component bands.
  var img_cloud = add_cloud_bands(img);

  //Add cloud shadow component bands.
  var img_cloud_shadow = add_shadow_bands(img_cloud);

  //Combine cloud and shadow mask, set cloud and shadow as value 1, else 0.
  var is_cld_shdw0 = img_cloud_shadow.select('clouds').add(img_cloud_shadow.select('shadows')).gt(0);

  //Remove small cloud-shadow patches and dilate remaining pixels by BUFFER input.
  //20 m scale is for speed, and assumes clouds don't require 10 m precision.
  var is_cld_shdw = is_cld_shdw0.focal_min(2).focal_max(BUFFER*2/20)
  .reproject({'crs': img.select([0]).projection(), 'scale': 20})
  .rename('cloudmask');

  //Add the final cloud-shadow mask to the image.
  //return img_cloud_shadow.addBands(is_cld_shdw);
  return img.addBands(is_cld_shdw).updateMask(is_cld_shdw.eq(0));
}

//Define NDVI function
function addNDVI(image) {
  return image.addBands(image.normalizedDifference(['B8', 'B4']).rename('ndvi'));
  }

//Define RENDVI function (variable in this code is named ndvi_re)
function addNDVI_re(image) {
  return image.addBands(image.normalizedDifference(['B6', 'B5']).rename('ndvi_re'));
  }
   
// Define NDMI function
function addNDMI(image) {
  return image.addBands(image.normalizedDifference(['B8A', 'B11']).rename('ndmi'));
  }
 

//Define MCARI function  https://www.indexdatabase.de/db/i-single.php?id=42    https://www.indexdatabase.de/db/i-single.php?id=41
function addMCARI(image) {
  var evi = image.expression('((B5-B4)-0.2*(B5-B3))*(B5/B4)', {
  // var evi = image.expression('1.2*(2.5*(B8-B4)-1.3*(B8-B3))',{
    'B3' : image.select('B3'),
    'B5' : image.select('B5'),
    'B4' : image.select('B4')
  }).float();
  return image.addBands(evi.rename('mcari'));
} 

//Define LCI function  https://www.indexdatabase.de/db/i-single.php?id=109
function addLCI(image) {
  var evi = image.expression('(B8-B5)/(B8+B4)', {
    'B8' : image.select('B8'),
    'B5' : image.select('B5'),
    'B4' : image.select('B4')
  }).float();
  return image.addBands(evi.rename('lci'));
} 
  
//Define Chl RE function  https://www.indexdatabase.de/db/i-single.php?id=252
function addClRE(image) {
  var evi = image.expression('B5/B7', {
    'B7' : image.select('B7'),
    'B5' : image.select('B5')
  }).float();
  return image.addBands(evi.rename('cl_re'));
}

//Define Chl Index RE function  https://www.indexdatabase.de/db/i-single.php?id=131
function addCliRE(image) {
  var evi = image.expression('(B8/B5)-1', {
    'B8' : image.select('B8'),
    'B5' : image.select('B5')
  }).float();
  return image.addBands(evi.rename('clidre'));
}

//Define Chl Green function  https://www.indexdatabase.de/db/i-single.php?id=251
function addClgreen(image) {
  var evi = image.expression('B3/B7', {
    'B7' : image.select('B7'),
    'B3' : image.select('B3')
  }).float();
  return image.addBands(evi.rename('cl_green'));
}

//Define Chl Index Green function  https://www.indexdatabase.de/db/i-single.php?id=128
function addCligreen(image) {
  var evi = image.expression('(B8/B3)-1', {
    'B8' : image.select('B8'),
    'B3' : image.select('B3')
  }).float();
  return image.addBands(evi.rename('clidgr'));
}


//Add cloudmask and ndvi bands to the final collection  
var cl = col.map(add_cld_shdw_mask)
.map(addNDVI)
.map(addNDVI_re)
.map(addNDRE)
.map(addNDMI)
.map(addClRE)
.map(addCliRE)
.map(addLCI)
.map(addClgreen)
.map(addCligreen)
.map(addMCARI);
print('Final collection s2cloudless:',cl);


//Set color palette for ndvi band
var ndvi_palette = ['d73027','f46d43','fdae61','fee08b','ffffbf',
'd9ef8b','a6d96a','66bd63','1a9850','006837'];

//Add layers to the map
var img1 = cl.first();
Map.addLayer(img1, rgbVisParam, 'RGB');
Map.addLayer(img1, {bands:'ndvi', min:0, max:1, palette:ndvi_palette}, 'ndvi_coll');
Map.addLayer(img1, {bands:'ndmi', min:-1, max:1, palette:ndvi_palette}, 'ndmi_coll');
Map.addLayer(img1, {bands:'ndvi_re', min:-1, max:1, palette:ndvi_palette}, 'ndvi_re_coll');
Map.addLayer(img1, {bands:'ndre', min:-1, max:1, palette:ndvi_palette}, 'ndre_coll');
Map.addLayer(img1, {bands:'mcari', min:0, max:2500, palette:ndvi_palette}, 'mcari_coll');
// Map.addLayer(img1,{bands:'cloudmask'},'cloudmask');

// var redf = cl.select(['B4','ndvi']).reduce({
//   reducer: ee.Reducer.linearRegression((cl.select('ndvi'),1))
// })
// print('Focal stdDev',redf);
// Map.addLayer(redf,null,'stdDev');


// Adjust data for export
var triplets = cl.map(function(image) {
  return image.select('mcari')
  .reduceRegions({
    collection: habr, 
    reducer: ee.Reducer.mean().setOutputs(['mcari']),
    scale: 10,
  }).filter(ee.Filter.neq('mcari', null))
    .map(function(feature) {
      return feature.set({'imageID': image.id()})
  })
}).flatten();  
print('Triplets: ', triplets);

var format = function(table, rowId, colId) {
  var rows = table.distinct(rowId); 
  var joined = ee.Join.saveAll('matches').apply({
    primary: rows, 
    secondary: table, 
    condition: ee.Filter.equals({
      leftField: rowId, 
      rightField: rowId
    })
  });
         
  return joined.map(function(row) {
      var values = ee.List(row.get('matches'))
        .map(function(feature) {
          feature = ee.Feature(feature);
          return [feature.get(colId), feature.get('mcari')];
        });
      return row.select([rowId]).set(ee.Dictionary(values.flatten()));
    });
};
var sentinelResults = format(triplets, 'id', 'imageID');

print('S2 merged triplets:',sentinelResults);

// Eliminate overlapping pixels by taking the maximum VI value
var merge = function(table, rowId) {
  return table.map(function(feature) {
    var id = feature.get(rowId)
    var allKeys = feature.toDictionary().keys().remove(rowId)
    var substrKeys = ee.List(allKeys.map(function(val) { 
        return ee.String(val).slice(0,8)}
        ))
    var uniqueKeys = substrKeys.distinct()
    var pairs = uniqueKeys.map(function(key) {
      var matches = feature.toDictionary().select(allKeys.filter(ee.Filter.stringContains('item', key))).values()
      var val = matches.reduce(ee.Reducer.max())
      return [key, val]
    })
    return feature.select([rowId]).set(ee.Dictionary(pairs.flatten()))
  })
}
var sentinelMerged = merge(sentinelResults, 'id');
print("S merged:", sentinelMerged);


////// TREND ////////

//Add time field
var addTime = function(image) {
  var date = ee.Date(image.get('system:time_start'));
  var days = date.difference(cl.first().date().millis(), 'day');
  return image
    // Add a time band.
    .addBands(ee.Image(days).rename('t').float())
    // Add a constant band.
    .addBands(ee.Image.constant(1).rename('constant'));
};

var trend_coll = cl.map(addTime);
print('Trend collection',trend_coll);
// List of the independent variable names
var independents = ee.List(['constant', 't']);
// Name of the dependent variable
var dependent = ee.String('ndvi');
// Compute a linear trend.  This will have two bands: 'residuals' and coefficients 
var trend = trend_coll.select(independents.add(dependent))
    .reduce(ee.Reducer.linearRegression(2, 1));
var bandNames = [['constant', 'time'], // 0-axis variation.
              ['ndvi']]; // 1-axis variation.
var lr = trend.select(['coefficients']).arrayFlatten(bandNames);
print(lr)
var trendcolor = ['401dff','0eff15','fbff16','ff3d12'];    
Map.addLayer(lr.select('constant_ndvi'), {palette:trendcolor}, 'trend array image');

////////////////////


//////////////////////////////////////////////////////////
//Generate Time-Series plots
var plotNDVI = ui.Chart.image.seriesByRegion({
      imageCollection: cl.select('ndvi'), 
      regions: habr,
      reducer: ee.Reducer.mean(),
      scale: 10,
      // xProperty:'system:time_start',
      seriesProperty:'system:index'})
              .setOptions({
                title: 'NDVI time series',
                interpolateNulls: true,
                lineWidth: 1,
                pointSize: 3,
                hAxis: {title: 'Date'},
                vAxis: {title: 'NDVI'},
                trendlines: {
                  0:{
                    type: 'linear',
                    color: 'CC0000',
                    showR2: true,
                    visibleInLegend: true
                  }
                }
      });
print(plotNDVI);

var plotNDVI_re = ui.Chart.image.seriesByRegion({
      imageCollection: cl.select('ndvi_re'), 
      regions: habr,
      reducer: ee.Reducer.mean(),
      scale: 10,
      // xProperty:'system:time_start',
      seriesProperty:'system:index'})
              .setOptions({
                title: 'RENDVI time series',
                interpolateNulls: true,
                lineWidth: 1,
                pointSize: 3,
                hAxis: {title: 'Date'},
                vAxis: {title: 'NDVI RE'},
                trendlines: {
                  0:{
                    type: 'linear',
                    color: 'CC0000',
                    showR2: true,
                    visibleInLegend: true
                  }
                }
      });
print(plotNDVI_re);

var plotNDRE = ui.Chart.image.seriesByRegion({
      imageCollection: cl.select('ndre'), 
      regions: habr,
      reducer: ee.Reducer.mean(),
      scale: 10,
      // xProperty:'system:time_start',
      seriesProperty:'system:index'})
              .setOptions({
                title: 'NDRE time series',
                interpolateNulls: true,
                lineWidth: 1,
                pointSize: 3,
                hAxis: {title: 'Date'},
                vAxis: {title: 'NDRE'},
                trendlines: {
                  0:{
                    type: 'linear',
                    color: 'CC0000',
                    showR2: true,
                    visibleInLegend: true
                  }
                }
      });
print(plotNDRE);

var plotNDMI = ui.Chart.image.seriesByRegion({
      imageCollection: cl.select('ndmi'), 
      regions: habr,
      reducer: ee.Reducer.mean(),
      scale: 10,
      // xProperty:'system:time_start',
      seriesProperty:'system:index'})
              .setOptions({
                title: 'NDMI time series',
                interpolateNulls: true,
                lineWidth: 1,
                pointSize: 3,
                hAxis: {title: 'Date'},
                vAxis: {title: 'NDMI'},
                trendlines: {
                  0:{
                    type: 'linear',
                    color: 'CC0000',
                    showR2: true,
                    visibleInLegend: true
                  }
                }
      });
print(plotNDMI);



var plotMCARI = ui.Chart.image.seriesByRegion({
      imageCollection: cl.select('mcari'), 
      regions: habr,
      reducer: ee.Reducer.mean(),
      scale: 10,
      // xProperty:'system:time_start',
      seriesProperty:'system:index'})
              .setOptions({
                title: 'MCARI time series',
                interpolateNulls: true,
                lineWidth: 1,
                pointSize: 3,
                hAxis: {title: 'Date'},
                vAxis: {title: 'MCARI'},
                trendlines: {
                  0:{
                    type: 'linear',
                    color: 'CC0000',
                    showR2: true,
                    visibleInLegend: true
                  }
                }
      });
print(plotMCARI);

// Export results to CSV
Export.table.toDrive({
    collection: sentinelMerged,
    description: 'jasan_MCARI',
    folder: 'earthengine',
    fileNamePrefix: 'jasan_MCARI',
    fileFormat: 'CSV'
});
