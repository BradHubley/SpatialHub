#' @title planar2lonlat
#' @description inverse projection: planar to lon/lat using projection. convert
#' planar coord systems (length scale defined in the CRS as +units=km or m)  to
#' lon-lat
#' @param x = undocumented
#' @param proj.type = undocumented
#' @param planar.coord.scale = undocumented
#' @param newnames = undocumented
#' @export

  planar2lonlat = function (x, proj.type, planar.coord.scale=NULL, newnames = c("lon", "lat")  ) {

     ## use of this is deprecated
    if ( !is.null( planar.coord.scale)) {
      # planar.coord.scale is the multiplier applied upon the planar coords
      # when planar.coord.scale=1000 .. it is converting incoming data (km) to m ## deprecated use +units=km
      # when planar.coord.scale=1 .. it means no conversion as it is already in m ## deprecated use +units=m
      stop( "Use of planar.coord.scale is deprecated, use +units=km (default) in proj4 CRS")
      #x$plon = x$plon * planar.coord.scale
      #x$plat = x$plat * planar.coord.scale
    }

  # first try an internal conversion /lookup for CRS
    proj4.params = NULL
    pcrs = lookup.projection.params(proj.type)
    if (!is.null(pcrs)) proj4.params = try( CRS( pcrs ), silent=TRUE )

    # if internal lookup does not work then try to directly pass to CRS
    if ( is.null(proj4.params) | inherits(proj4.params, "try-error")) {
      if (!is.null(proj.type))  proj4.params = try( CRS( proj.type ), silent=TRUE )
    }
    if ( inherits(proj4.params, "try-error") ) {
      print( proj.type )
      warning( "Projection not recognised")
    }

    crsX = CRSargs( proj4.params)
    if ( ! grepl("units", crsX) ) {
      print (crsX)
      stop( "The proj4 CRS requires an explicit +units=km ")
    }
    y = project( cbind(x$plon, x$plat), crsX, inv=T )
    colnames(y) = newnames
    for (i in 1:length( newnames)) {
      if ( newnames[i] %in% colnames(x) ) x[, newnames[i]] = NULL
    }
    x = cbind(x,y)
    return (x)
  }


