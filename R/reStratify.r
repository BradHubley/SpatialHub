#' @title reStratify
#' @description Assigns station number to tows based on proximity.
#' @param events = PBSmapping::EventData of sets
#' @param strata = PBSmapping::PolyData of strata
#' @param lines = logical, if TRUE X1, Y1, X2, Y2 must be supplied to describe the sets
#' @importFrom PBSmapping findPolys
#' @author Brad Hubley
#' @export
reStratify <- function(events,polys,lines=F){


  if(lines==T){
    events$X<-with(events,apply(cbind(X1,X2),1,mean))
    events$Y<-with(events,apply(cbind(Y1,Y2),1,mean))
  }

  if(!"SID"%in%names(polys)){polys$SID<-polys$PID}

  key<-findPolys(events,polys)
  events<-merge(events,key[c('EID','SID')],all=T)

  return(events)
}

