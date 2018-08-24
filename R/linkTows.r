#' @title assignStation
#' @description Assigns station number to tows based on proximity. 
#' @param events = PBSmapping::EventData of tows
#' @param maxdist = maximum distance between points considered to be the same statiom
#' @param res = resolution of the spatstat::distmap
#' @param expwin = expansion window beyond supplied points that defines the area examined 
#' @param map = preset location to pass to bioMap
#' @param lines = logical, if TRUE X1, Y1, X2, Y2 must be supplied to describe the tows
#' @param ... additional arguments passed to bioMap
#' @author Brad Hubley 
#' @export
linkTows <- function(events,tows,expwin=0.05,mindist=10,...){


		tows$EID = 1:nrow(tows)
		tows<-as.EventData(tows)
		attr(tows,"projection") = "LL"
		tows = convUL(tows)
		
		events$EID = 1:nrow(events)
		events<-as.EventData(events)
		attr(events,"projection") = "LL"
		attr(events,"zone") = attr(tows,'zone')
		events = convUL(events)



	xmax <- max(tows$X)+diff(range(tows$X))*expwin
	xmin <- min(tows$X)-diff(range(tows$X))*expwin

	ymax <- max(tows$Y)+diff(range(tows$Y))*expwin
	ymin <- min(tows$Y)-diff(range(tows$Y))*expwin

	W <- owin(c(xmin,xmax),c(ymin,ymax))

	events = events[inside.owin(events$X,events$Y,W),]

	NNdist = c()
	NNwhich = c()

	for(i in 1:nrow(events)){

		tmp = rbind(subset(events,select=c('X','Y'))[i,],subset(tows,select=c('X','Y')))
		ppp<-as.ppp(subset(tmp,select=c('X','Y')),W)
		NNdist[i] = nndist(ppp)[1]
		NNwhich[i] = nnwhich(ppp)[1]-1

	}
	events$NNdist = NNdist
	events$NNwhich = NNwhich
	events$Tdiff = NA
	lts = unique(events$NNwhich)
	for(i in 1:length(lts)){
		events$Tdiff[events$NNwhich==lts[i]] = tows$SET_DATE[tows$EID==lts[i]] - events$TOW_DATE[events$NNwhich==lts[i]]
	}

	events = subset(events,NNdist<mindist)	

	return(list(events=events,tows=tows))
}

