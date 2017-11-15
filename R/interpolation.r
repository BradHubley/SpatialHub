#' @title interpolation
#' @description preforms a spatial interpolation either inverse distance weighted or kriging.
#' @param ticks = contour lines or strata definitions, 'define' will determine them according to the f(x) rule from Cochran 1977 (see Hubley et al. 2009)
#' @param nstrata = number of strata when ticks='define'
#' @param str.max = maximum value for stratifying variable (all values greater will be set at maximum)
#' @param str.min = minimum value for stratifying variable (all values lesser will be set to NA )
#' @param place = rounding place for defining strata
#' @param aspr = aspect ratio for for creating square grid, will determine from data if missing
#' @param interp.method = 'gstat' = gstat (idw) function from gstat library, 'krige' = krige function from gstat library, 'none' = no interpolation function
#' @param res = resolution for interpolation
#' @param maxdist, nmax, idp, mod.type = arguments to be passed to interpolation function (idp is inverse distance power)
#' @param smooth = logical, TRUE calls smoothing function
#' @param smooth.fun = applies smooth.fun to data over a grid
#' @param sres = resolution for smoothing (grid)
#' @param no.data = how to treat missing data when smoothing, default is to assume zero
#' @param blank = TRUE calls blanking function, included blanking distance, beyond which if no data are present zeros are assigned or blank.eff
#' @param blank.dist = blanking distance if missing will select the shortest distance to the most isolated point
#' @param blank.type = how spaced out the zeros are, 1 = avg. nearest neighbour distance, 2 = blanking distance
#' @param log.dat = logical, whether to log data
#' @param covariate.dat = covariate data typically used in kriging
#' @param subset.poly = inclusion polygon to subset spatially, 'square' used to close polygons (useful when not smoothing or blanking)
#' @param subset.eff = sets values to this outside the subset.poly
#' @param subscale = size of inset when subset.poly = 'square'
#' @author Brad Hubley 
#' @export





interpolation<-function(contour.dat,ticks,nstrata,str.max,str.min,place=0,aspr,interp.method='gstat',res=0.01,maxdist=Inf,nmax=8,idp=0.5,mod.type="Sph",smooth=F,smooth.fun=median,smooth.procedure=1,sres=1/60.1,no.data='0',blank=T,blank.dist,blank.eff=0,blank.type=2,log.dat=F,covariate.dat=NULL,subset.poly=NULL,regrid=F,subset.eff=NA,subscale=res){
	

	print("contour start")
	print(Sys.time())
	image.mod<-NULL

	names(contour.dat)[1:4]<-c("EID","X","Y","Z")
	if(!is.null(covariate.dat))names(contour.dat)[5]<-"CoV"
	dataPoints1<-contour.dat[,1:3]
	if(is.numeric(dataPoints1$EID)==F)dataPoints1$EID<-1:nrow(dataPoints1)
	
	if(interp.method=='krige')blank=F
	

	# Aspect ratio
	if(missing(aspr)){
		aspr=1/cos(rad(mean(contour.dat$Y)))
		print(paste('Aspect ratio',aspr))
	}
	
	
	# SMOOTHING	
	if(smooth==T){
		if(interp.method!='none'){
			contour.dat<-smoothing(contour.dat,fun=smooth.fun,res=sres,aspr=aspr,no.data=no.data,subset.poly=subset.poly,procedure=smooth.procedure)
			contour.dat<-contour.dat[!is.na(contour.dat$Z),]
		}
		if(interp.method=='none'){
			image.dat<-smoothing(contour.dat,fun=smooth.fun,res=sres,aspr=aspr,matrix=T,no.data=no.data,subset.poly=subset.poly,procedure=smooth.procedure)
			names(image.dat)<-c('x','y','z')
		}
	}
	
	# BLANKING
	if(blank==T) {
		if(missing(blank.dist))contour.dat<-blanking(contour.dat,aspr=aspr,type=blank.type,eff=blank.eff)
		if(!missing(blank.dist))contour.dat<-blanking(contour.dat,blank.dist=blank.dist,aspr=aspr,type=blank.type,eff=blank.eff)
	}
		
	dataPoints2<-contour.dat[,1:3]
	dataPoints2$EID<-1:nrow(dataPoints2)
	# INTERPOLATION	
	
	if(!missing(ticks))if(ticks[1]=='define'){
		ticks<-unique(defineStrata(contour.dat$Z,nstrata,str.min,str.max,place))
		nstrata<-length(ticks)-1
	}
	if(missing(nstrata)){
		if(missing(ticks))print("ticks or nstrata must be specified")
		nstrata<-length(ticks)-1
	}
	
	if(!is.null(subset.poly)){
		if(subset.poly=='square'){
			inset=subscale*0.8
			subset.poly<-with(contour.dat,data.frame(X=sort(rep(c(min(X)-inset,max(X)+inset),2)),Y=c(min(Y)-inset,rep(max(Y)+inset,2),min(Y)-inset)))
		}
	}

	if(interp.method!='none'){
		image.lst<-imagePrep(dat=contour.dat,method=interp.method,nmax=nmax,idp=idp,log.dat=log.dat,res=res,aspr=aspr,covariate.dat=covariate.dat,regrid=regrid,mod.type=mod.type,subscale=subscale, subset.poly=subset.poly)
		image.dat<-image.lst[[1]]
		image.var<-image.lst[[2]]
		image.mod<-image.lst[[3]]
		if(!missing(ticks)){
			if(missing(str.min))str.min<-min(ticks)
			if(missing(str.max))str.max<-max(ticks)
		}
		if(missing(ticks)){
			if(missing(str.min))str.min<-min(image.dat$z,na.rm=T)
			if(missing(str.max))str.max<-max(image.dat$z,na.rm=T)
			ticks<-seq(str.min,str.max,length=nstrata+1)
		}
		image.dat$z[image.dat$z>str.max]<-str.max
		image.dat$z[image.dat$z<str.min]<-subset.eff		##### not tested for other applications!!!
	}
	
		
	# SUBSET POLYGON
	if(!is.null(subset.poly)){
		if(subset.poly=='square'){
			inset=subscale*0.8
			subset.poly<-with(contour.dat,data.frame(X=sort(rep(c(min(X)-inset,max(X)+inset),2)),Y=c(min(Y)-inset,rep(max(Y)+inset,2),min(Y)-inset)))
		}
		Y<-sort(rep(image.dat$y,length(image.dat$x)))
		X<-rep(image.dat$x,length(image.dat$y))
		tmp<-data.frame(X,Y,Z=as.vector(image.dat$z))
		tmp$Z[!with(tmp, inout(cbind(X,Y), subset.poly[c("X","Y")], bound = T))]<-subset.eff
		image.dat$z<-matrix(tmp$Z,length(image.dat$x),length(image.dat$y))
	}

	

	print("contour end")
	print(Sys.time())

	
	output<-list(contour.dat=contour.dat,image.dat=image.dat,image.var=image.var,image.mod=image.mod,str.def=ticks)
	
	
	return(output)
}
