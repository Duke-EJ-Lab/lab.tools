#' Combining multiple shapefiles
#' 
#' A frequent data munging problem is simplifying and combining a bunch of shapefiles. This function takes a list of SPDF's, and returns you one large one, dealing with ID issues, and making sure that the data frame stays attached.
#'
#' @param shape a vector of paths to where your shapefiles you want to simplify and combine are
#' @param id What column in the data to use as a UNIQUE id for the shapefiles. Must be unique values across all shapefiles
#' @param tol What your tolerance is for simplification, smaller number means more similar output and less of a change in object size. Larger number means less similar output, larger change in object size. More info in gSimplify docs. 
#' @param topologyPreserve see gSimplify
#' @import rgdal
#' @import rgeos
#' @import magrittr
#' @importFrom data.table rbindlist
#' @export
simplify_and_combine_shapefiles = function(shape_paths, id, tol, 
                                           topologyPreserve = T) {
  
  # create list to store SPDF's in
  shape_list = as.list(rep(NA, length(shape_paths)))
  
  for (i in 1:length(shape_paths)) {
    
    # read in shapefile
    cur = readOGR(shape_paths[i])
    cur_data = cur@data
    
    # simplify
    cur = gSimplify(cur, tol = tol, topologyPreserve = topologyPreserve)
    cur = SpatialPolygonsDataFrame(cur, cur_data)
    row.names(cur) = cur@data[, id] # make sure they have unique ID's for merging
    
    shape_list[[i]] = cur
  }
  
  # combine the list to one big Spatial Polygon Data Frame
  shapes_df = rbindlist(lapply(shape_list, function(x){x@data}), fill=T) %>% as.data.frame()
  row.names(shapes_df) = shapes_df[, id]
  shapes_sp = unlist(lapply(shape_list, function(x){x@polygons}))
  
  
  shapes = SpatialPolygonsDataFrame(SpatialPolygons(shapes_sp), shapes_df)
  
  return(shapes)
  
}
