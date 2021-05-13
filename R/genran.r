#' @title genran
#' @description Generates random points in a polygon (PBSmapping format), called
#' by allocPoly
#' @param npoints = total number of stations to be selected
#' @param bounding.poly = PBSmapping::PolySet describing the area to be surveyed
#' @param projection = PBSmapping::PolySet attribute, default is Lat. and Lon.
#' @param mindist = minimum distance (km) or buffer between stations
#' @importFrom PBSmapping convUL
#' @importFrom splancs csr
#' @importFrom spatstat.geom nndist
#' @importFrom spatstat.geom as.ppp
#' @importFrom spatstat.geom owin
#' @importFrom splancs as.points
#' @author Brad Hubley
#' @export

genran<-function(npoints,bounding.poly,projection="LL",mindist=NULL){

	# create pool of random points
	bound.pts<-as.points(list(x=bounding.poly$X,y=bounding.poly$Y))


	if(is.null(mindist)){
		pool.EventData<-data.frame(1:npoints,csr(bound.pts,npoints))
		attr(pool.EventData,"projection")<-projection
		names(pool.EventData)<-c("EID","X","Y")
	}

	if(!is.null(mindist)){
		pool.EventData<-data.frame(1:npoints,csr(bound.pts,npoints))
		attr(pool.EventData,"projection")<-projection
		names(pool.EventData)<-c("EID","X","Y")

		if(projection=="LL")pool.EventData<-convUL(pool.EventData)
		W<-owin(range(pool.EventData$X),range(pool.EventData$Y))
		pool.ppp<-as.ppp(subset(pool.EventData,select=c('X','Y')),W)
		pool.EventData$nndist<-nndist(pool.ppp)
		if(projection=="LL")pool.EventData<-convUL(pool.EventData)


		for(i in 1:npoints){
			if(pool.EventData$nndist[i]>mindist) next
			else if(pool.EventData$nndist[i]<mindist){
				repeat{
					pool.EventData[i,c("X","Y")]<-csr(bound.pts,1)
					if(projection=="LL")pool.EventData<-convUL(pool.EventData)
					W<-owin(range(pool.EventData$X),range(pool.EventData$Y))
					pool.ppp<-as.ppp(subset(pool.EventData,select=c('X','Y')),W)
					pool.EventData$nndist<-nndist(pool.ppp)
					if(projection=="LL")pool.EventData<-convUL(pool.EventData)
					if(pool.EventData$nndist[i]>mindist) break
				}
			}
		}
	}

	pool.EventData

}
