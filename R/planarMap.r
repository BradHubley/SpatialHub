#' @title planarMap
#' @description a  mapping function that uses levelplot to plot data in planar coordinates 
#' @param xyz data to be plotted 
#' @param corners 2x2 dataframe that specify the corners of the map i.e. corners = data.frame(lon=c(-67.54,-56.5),lat=c(41,47.2)) 
#' @param projection to be used, see: lookup.projection.params
#' @param datascale for legend that describes the z variable
#' @param interpolation logical, does thin plate spline interpolation of the z variable and plots the result
#' @param add.zeros uses zeroInflate() to add zeros where there is no data before the interpolation (assumes zero outside the sampled area)
#' @param theta interpolation parameter
#' @param pts data points to be overlayed on the map
#' @param save logical TRUE saves the map as a .png, FALSE prints to screen 
#' @author Brad Hubley (mostly copied from Jae Choi's bio.spacetime::map)
#' @examples
#' planarMap(xyz,depths=F)
#' @export

  planarMap = function( xyz, depths=T, pts=NULL, colpts=F, annot=NULL, annot.cex=2.2, scalebar = T, projection = "utm20", col.regions=T, datascale=seq(0,1,l=50), at=datascale, fn=paste("map", trunc(runif(1)*1e8), sep=""), loc=tempdir(), corners=NULL, rez=c(1,1), save=F, pt.cex=0.5, pt.pch=16, pt.col='black',colorkey=NULL, fill=T, log.variable=F, interpolation=F, add.zeros=F, theta=50, rev=F, ... ) {

  options(stringsAsFactors=F)
    # map using levelplot 
    
    # spatial coordinates
    xlim =ylim = NULL
    
    if ( !is.null(corners) ) {
      if ( is.null(corners$plon) ) corners = lonlat2planar( corners,  proj.type= projection )
    }
    if ( is.null( xyz$plon) ) {
      names( xyz ) = c( "lon", "lat", "z" )
      xyz = lonlat2planar( xyz,   proj.type=projection)
      xyz = xyz[ , c( "plon", "plat", "z" ) ]
    } else {
      names( xyz ) = c( "plon", "plat", "z" )
    }
#browser()
    if ( !is.null(corners) ) {
      xlim = range( corners$plon)
      ylim = range( corners$plat)
    } else {
      xlim = range( xyz$plon )
      ylim = range( xyz$plat )
    }

    if ( ! is.null(pts) ) {
      if ( is.null( pts$plon) ) {
        pts = lonlat2planar(xyz,  proj.type=projection)
        pts = pts[, c("plon", "plat")]
      }
    }

    # interpolation

      if(log.variable){
        ds = datascale
        at = seq(log(min(at)),log(max(at)),l=length(at))
        datascale = log(datascale)
        xyz$z = log(xyz$z+ds[1])
        eff = datascale[1]
      } else {
        eff = 0
      }
    if(interpolation){

      at = at[-1]
      if(add.zeros) {
        #browser()
        xyz =na.omit( zeroInflate(xyz,corners=corners,type=2,type.scaler=0.2,eff=eff) )
      } 
      #browser()
      u = fastTps(x=xyz[,c("plon","plat")] , Y=xyz[,'z'], theta=theta )
      

      xyz = data.frame( baseLine, z = predict(u, xnew=baseLine))
    }


    # legend color
    if(is.logical( col.regions)){
      cols = colorRampPalette(c("darkblue","cyan","green", "yellow", "orange","darkred", "black"), space = "Lab")
      col.regions = cols(length(at)+1)
      if(rev)col.regions = rev(col.regions)
    }
    if(is.null(colorkey)){
      colorkey=list(space="right", labels=list(cex=3)) # these are lattice options
      if(log.variable){
        # create labels for legend on the real scale
        labs=as.vector(c(1,2,5)%o%10^(-4:5))
        labs=labs[which(labs>exp(min(at))&labs<exp(max(at)))]
        colorkey=list(labels=list(at=log(labs+ds[1]),labels=labs,cex=2))
      }
    }
    if (ncol( xyz) == 2) { # assuming points
      xyz = cbind( xyz, 1)
      colorkey=F
      pts = xyz
    }
    if(fill){
      xyz$z[xyz$z>max(datascale)]=max(datascale)
      xyz$z[xyz$z<min(datascale)]=min(datascale)
    }

    lp = levelplot( z ~ plon+plat, data=xyz, aspect="iso", pts=pts, colpts=colpts, annot=annot, 
      annot.cex=annot.cex, xlab="", ylab="", scales=list(draw=F), col.regions=col.regions, at=at, xlim=xlim, ylim=ylim,
      colorkey=colorkey , rez=rez,  
      panel = function(x, y, z, rez=rez,  ...) {

        panel.levelplot (x, y, z, aspect="iso", rez=rez, ...)

        if ( !is.null(pts) ) {
          if (colpts) {  # overlay above with larger sized points, esp if there is colour
            colb = findInterval( z, at)
            for ( ii in 1:length(z) ) {
              panel.xyplot( pts$plon[ii], pts$plat[ii],  aspect="iso", col=col.regions[colb[ii]],
                  panel = panel.rect, height = rez[1], width = rez[2], cex=pt.cex,... )
            }
          } else {
              panel.xyplot( pts$plon, pts$plat,  aspect="iso", col = pt.col, pch=pt.pch,
                  panel = panel.rect, height = rez[1], width = rez[2], cex=pt.cex, ... )
          }
        }

        # depth isobaths
        if (!is.null(depths)) {
          sp.lines( isoBaths , col = rgb(0.2,0.2,0.2,0.5), cex=0.6 )
          #for ( i in depths ) sp.lines( isoBaths[as.character(i) ] , col = rgb(0.2,0.2,0.2,0.5), cex=0.6 )
        }

        #coastline
         sp.polygons( coastLine, col = "black", cex=1 ,fill='grey')

        if (scalebar) {
          lx = xlim[2]-xlim[1]
          ly = ylim[2]-ylim[1]
				  leg = c( xlim[2]-0.1*lx, ylim[1] + 0.1*ly )
          panel.arrows( x0=leg[1]-100, y0=leg[2], x1=leg[1], y1=leg[2],	angle=90, length=0.06, ends="both", lwd=3, col="black", ...)
          panel.text( x=leg[1]-50 , y=leg[2]+0.05*ly , "100 km", cex=1.7 )
        }

    } # end panel
    ) # end levelplot

    if(save){

      dir.create (loc, showWarnings=FALSE, recursive =TRUE)
      fn = file.path( loc, paste(fn, "png", sep="." ) )
      png(  filename=fn, width=3072, height=2304, pointsize=40, res=300 )
      print(lp)
      dev.off()
      print(fn)
    } 
    else{
      print(lp)
    }


  }


