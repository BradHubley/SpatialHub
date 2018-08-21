#' @title createBathyLines
#' @description this function was used to create bathy lines from xyz data from
#' topex bathymetry downloaded from  \url{http://topex.ucsd.edu/cgi-bin/get_data.cgi}
#' specify extent in website to get data)
#' @param input.fp = undocumented
#' @param input.fn = undocumented
#' @param redo = undocumented
#' @param interval = undocumented
#' @param batch = undocumented
#' @param range = undocumented
#' @param save.by = undocumented
#' @param digits = undocumented
#' @param dneg = undocumented
#' @param output.fn = undocumented
#' @importFrom utils read.table
#' @importFrom grDevices contourLines
#' @importFrom PBSmapping convCP
#' @importFrom PBSmapping makeTopography
#' @export
createBathyLines<-function(input.fp=file.path( project.datadirectory("bio.lobster"), "data", "maps","topex"),
                           input.fn="topex.xyz",redo=T,interval=10,batch=100,range=c(10,5000),save.by=1000,
                           digits=3,dneg=T,output.fn="bathyPoly"){


	if(redo){
		bathy.dat<-read.table(file.path( input.fp, input.fn),header=F)
		names(bathy.dat)<-c("X","Y","Z")
		bathy.dat$X<-bathy.dat$X-360
		bathy.lst<-makeTopography(bathy.dat,digits=digits)
		save( bathy.lst, file=file.path(input.fp,paste0(input.fn,".rdata")))
	}
	else load(file.path(input.fp,paste0(input.fn,".rdata")))

	tmp.lst<-list()
	for(i in 1:(range[2]/batch)){

		print(i/(range[2]/batch)*100)

		isobath=seq(interval+batch*(i-1),batch*i,interval)*ifelse(dneg,-1,1)
		bathy.cl<-contourLines(bathy.lst,levels=isobath)
		bathy.cp <- convCP(bathy.cl)
		tmp.lst[[i]] <- bathy.cp$PolySet
		tmp.lst[[i]]$PID <- tmp.lst[[i]]$PID+batch*(i-1)/interval
		tmp.lst[[i]]$Z <- tmp.lst[[i]]$PID*interval

	}

	for(i in 1:(range[2]/save.by)){
		x<-save.by/batch
		bathy.poly<-do.call(rbind,tmp.lst[(1+x*(i-1)):x*i])
		save( bathy.poly, file=file.path( input.fp, paste0(output.fn,i,".rdata")))
	}
}

