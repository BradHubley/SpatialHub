#' @title imagePrep
#' description Called by interpolation to preform the interpolation and return image data
#' @param dat = a dataframe with 3 columns (longitude, latitude, variable to be mapped)
#' @param aspr = aspect ratio for a given latitude (default is for 45 deg.)
#' @param res = resolution of image in decimal degrees
#' @param method = 'gstat' = gstat (idw) function from gstat library, 'krige' = krige function from gstat library, 'none' = no interpolation function
#' @param maxdist, nmax, idp, mod.type = arguments to be passed to gstat function (idp is inverse distance power)
#' @param log.dat = logical, whether to log data
#' @param covariate.dat = covariate data typically used in kriging
#' @param subset.poly = inclusion polygon to subset spatially, 'square' used to close polygons (useful when not smoothing or blanking)
#' @param subset.eff = sets values to this outside the subset.poly
#' @param subscale = size of inset when subset.poly = 'square'
#' @author Brad Hubley 
#' @export

imagePrep<-function(X,Y,Z,dat,aspr=1.345640,res=0.02,summary.dat=F,log.dat=T,method='gstat',matrix.dat=T,idp=0.5,nmax=7,maxdist=Inf, subset.poly=NULL, covariate.dat=NULL,regrid=F,mod.type="Sph",subscale=0.01){

	print("imagePrep start")
	print(Sys.time())

	if(missing(dat))dat<-data.frame(X,Y,Z)
#	if(ncol(dat)==4)names(dat)[4]<-"co.v"

	if(log.dat)dat$Z<-log(dat$Z+0.0000000001)
   	# get grid for prediction
	if(is.null(covariate.dat)&&is.null(subset.poly)){
		Xs<-seq(min(dat$X)-subscale,max(dat$X)+subscale,res*aspr)
		Ys<-seq(min(dat$Y)-subscale,max(dat$Y)+subscale, res)
	}
	if(!is.null(subset.poly)){
		Xs<-seq(min(subset.poly$X)-subscale,max(subset.poly$X)+subscale,res*aspr)
		Ys<-seq(min(subset.poly$Y)-subscale,max(subset.poly$Y)+subscale, res)
	}
	if(!is.null(covariate.dat)){
		names(covariate.dat)<-c("X","Y","CoV")
		Xs<-seq(min(covariate.dat$X),max(covariate.dat$X),res*aspr)
		Ys<-seq(min(covariate.dat$Y),max(covariate.dat$Y), res)
	}
	tow.xy <- data.frame(X = dat$X, y = dat$Y)
	poly <- tow.xy[chull(tow.xy), ]
	names(poly) <- c("X", "Y")
	grid.dat <- with(poly, expand.grid(X = Xs, Y = Ys))
		
	if(!is.null(covariate.dat)){
		if(regrid==T)grid.dat<-grid.data(covariate.dat,grid.dat)
		if(regrid==F)grid.dat<-covariate.dat
	}	

	# interpolation methods
	if(method=='gstat'){
		Z.gstat <- gstat(id = "Z", formula = Z ~ 1, locations = ~ X + Y, data = dat,maxdist=maxdist, nmax = nmax, set = list(idp = idp))
		Z.dat<- predict(Z.gstat, grid.dat)
		image.data<-makeTopography(Z.dat[c('X','Y','Z.pred')],digits=5)
		var.data<-NULL
		if(summary.dat)print(summary(Z.dat$Z.pred))
		if(matrix.dat==F)image.data<-data.frame(X=Z.dat[,1],Y=Z.dat[,2],Z=Z.dat[,4])
		spatial.model<-Z.gstat
	}
		
	if(method=='o.krige'){
		browser()
		v <- variogram(Z ~ 1, locations = ~ X + Y, data = dat)
		v.fit <- fit.variogram(v, model = vgm(max(v$gamma), mod.type, median(v$dist), min(v$gamma)))
		Z.krige <- krige(formula = Z ~ 1, locations = ~ X + Y, data = dat, newdata = grid.dat, model=v.fit)
		image.data<-makeTopography(Z.krige[,-4])
		var.data<-makeTopography(Z.krige[,-3])
		if(matrix.dat==F)image.data<-data.frame(X=Z.krige[,1],Y=Z.krige[,2],Z=Z.krige[,3])
		spatial.model<-v.fit
	}
	if(method=='u.krige'){
		v <- variogram(Z ~ CoV, locations = ~ X + Y, data = dat)
		v.fit <- fit.variogram(v, model = vgm(max(v$gamma), mod.type, median(v$dist), min(v$gamma)))
		Z.krige <- krige(formula = Z ~ CoV, locations = ~ X + Y, data = dat, newdata = grid.dat, model=v.fit)
		image.data<-makeTopography(Z.krige[,-4])
		var.data<-makeTopography(Z.krige[,-3])
		if(matrix.dat==F)image.data<-data.frame(X=Z.krige[,1],Y=Z.krige[,2],Z=Z.krige[,3])
		spatial.model<-v.fit
	}
		

	
	if(log.dat)image.data$z<-exp(image.data$z)
	print("image.prep end")
	print(Sys.time())

	return(list(image.data,var.data,spatial.model))
	
}
