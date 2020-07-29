library(sf)
library(mapview)
library(raster)
myproj <- "+proj=lcc +lat_1=45.827  +lat_2=45.827  +lat_0=45.827 +lon_0=11.625 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"


grid.points <- st_read( "data/points.shp" )

grid.points.myproj <-   grid.points %>% st_transform(myproj)
#grid.points.myproj2 <-  st_transform(grid.points, myproj)

nodes.buffered<-st_buffer(grid.points.myproj, 333.33, nQuadSegs = 1)

st_bbox_by_feature = function(geom) {
  
  geom2 = st_geometry(geom)
  
  f <- function(single.geom) { 
    st_as_sfc( st_bbox(single.geom,), crs=myproj)
  }
  
  do.call("c", lapply(geom2, f))
}

tiles <- st_bbox_by_feature( nodes.buffered )
tiles <- tiles %>% st_set_crs(myproj)

tiles.latlng <- tiles %>% st_transform("+init=epsg:4326")

st_write(tiles.latlng, "data/tiles.shp" )


## after google earth map-reduce to percentiles
grid.points.gee <- st_read( "data/tiles_withData.shp" )
grid.points.gee$iqr<-grid.points.gee$p75 - grid.points.gee$p25

pal = mapviewPalette("mapviewSpectralColors")
mapview( grid.points.gee, col.regions = pal(100), zcol = "p50", 
         color=NULL  ) 


 
