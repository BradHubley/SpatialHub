#' blanking
#' @description incorporates blanking distance by including zeros spaced eqully at the average nearest nieghbour distance, default blanking distance is the nearest neighbour distance of the most isolated point
#' @export

blanking<-function(surv.dat,blank.dist,aspr=aspr, type=1, eff=0,scale=0.5,type.scaler=0.5){
	
	require(spatstat)
#    browser()
    surv.pts<-subset(surv.dat,select=c('X','Y'))
    xmin<-min(surv.pts$X)
    xmax<-max(surv.pts$X)
    ymin<-min(surv.pts$Y)
    ymax<-max(surv.pts$Y)
    W<-owin(c(xmin-scale,xmax+scale),c(ymin-scale,ymax+scale))
    surv.ppp<-as.ppp(surv.pts,W)
    if(missing(blank.dist))blank.dist<-max(nndist(surv.ppp))
    if(type==1)dims<-c(round((ymax-ymin)/(mean(nndist(surv.ppp))*type.scaler)*aspr),round((xmax-xmin)/(mean(nndist(surv.ppp))*type.scaler)))
    if(type==2)dims<-c(round((ymax-ymin)/(blank.dist*type.scaler)*aspr),round((xmax-xmin)/(blank.dist*type.scaler)))
    blank.map<-distmap(surv.ppp,dim=dims)
    blank.dat<-data.frame(X=sort(rep(blank.map$xcol,blank.map$dim[1])),Y=rep(blank.map$yrow,blank.map$dim[2]),dist=as.vector(blank.map$v))
    blank.dat<-subset(blank.dat,dist>blank.dist,c('X','Y'))
    blank.dat<-merge(surv.dat,data.frame(EID=1:nrow(blank.dat)+1000,blank.dat,Z=eff),all=T)
    print(paste("blanking distance",blank.dist))

    blank.dat
    
}    
