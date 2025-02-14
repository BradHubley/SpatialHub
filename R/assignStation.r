#' @title assignStation
#' @description Assigns station number to tows based on proximity.
#' @param events = PBSmapping::EventData of tows
#' @param maxdist = maximum distance between points considered to be the same statiom
#' @param res = resolution of the spatstat.geom::distmap
#' @param expwin = expansion window beyond supplied points that defines the area examined
#' @param map = preset location to pass to bioMap
#' @param lines = logical, if TRUE X1, Y1, X2, Y2 must be supplied to describe the tows
#' @param ... additional arguments passed to bioMap
#' @importFrom grDevices contourLines
#' @importFrom grDevices rgb
#' @importFrom PBSmapping as.EventData
#' @importFrom PBSmapping calcCentroid
#' @importFrom PBSmapping convCP
#' @importFrom spatstat.geom as.psp
#' @importFrom spatstat.geom as.ppp
#' @importFrom spatstat.geom distmap
#' @importFrom spatstat.geom owin
#' @author Brad Hubley
#' @export
assignStation <- function(events,maxdist=0.01,res=0.005,expwin=0.05,map=NULL,lines=F,...){

	if(lines==T){
		events$X<-with(events,apply(cbind(X1,X2),1,mean))
		events$Y<-with(events,apply(cbind(Y1,Y2),1,mean))
	}

	events<-as.EventData(events)

	xmax <- max(events$X)+diff(range(events$X))*expwin
	xmin <- min(events$X)-diff(range(events$X))*expwin

	ymax <- max(events$Y)+diff(range(events$Y))*expwin
	ymin <- min(events$Y)-diff(range(events$Y))*expwin

	W <- owin(c(xmin,xmax),c(ymin,ymax))
	if(lines==F)events.ppp <- as.ppp(subset(events,select=c('X','Y')),W)
	if(lines==T){
		events.ppp <- as.psp(subset(events,select=c('X1','Y1','X2','Y2')),window=W)
		d1<-data.frame(PID=1,SID=1:nrow(events),POS=1,subset(events,select=c("X1","Y1")))
		names(d1)[4:5]<-c("X","Y")
		d2<-data.frame(PID=1,SID=1:nrow(events),POS=2,subset(events,select=c("X2","Y2")))
		names(d2)[4:5]<-c("X","Y")
		towlines<-merge(d1,d2,all=T)
	}
	events.dist<-with(distmap(events.ppp,eps=res),list(xcol,yrow,t(v)))
	names(events.dist)<-c('x','y','z')

	CL <- contourLines(events.dist,levels=maxdist)
	CP <- convCP(CL)
	polys <- CP$PolySet

	if(!is.null(map)){
		bioMap(map,...)
		addPolys(polys,col=rgb(0,0,1,0.3))
		if(lines==F)addPoints(events,pch='.',col='red')
		if(lines==T)addLines(towlines,col=rgb(1,0,0,0.3))
	}

	key<-findPolys(events,polys)
	events<-merge(events,key[c('EID','SID')],all=T)
	stations<-calcCentroid(polys)[,-1]

	return(list(events=events,stations=stations,polys=polys))


}
