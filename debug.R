
st_bbox_by_feature = function(geom) {
  
  geom2 = st_geometry(geom)
  
  browser()
  
  f <- function(single.geom) { 
    st_as_sfc( st_bbox(single.geom,), crs=myproj)
  }
  
  do.call("c", lapply(geom2, f))
}


tiles <- st_bbox_by_feature( nodes.buffered )

