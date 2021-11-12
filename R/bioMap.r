#' @title bioMap
#' @description a customizable mapping function that uses PBSmapping to plot fisheries data, bathymetry and mangement boundaries.
#' @param area = 'custom' where xlim & ylim are specified or select from area list below

#' @param ylim = y limits
#' @param xlim = x limits
#' @param mapRes = coastline detail ('LR' = low resolution, 'MR' = medium resolution, 'HR' = high resolution, 'UR' = ultra resolution)
#' @param land.col = colour for the land
#' @param nafo = undocumented
#' @param pt.cex = undocumented
#' @param labels = undocumented
#' @param labcex = undocumented
#' @param LT = undocumented
#' @param plot.rivers = undocumented
#' @param addsubareas = undocumented
#' @param subsetSurveyStrata = undocumented
#' @param addbasemap = undocumented
#' @param title = plot title
#' @param boundaries = for ploting specific management boundaries ("lobster", "scallop", "snowcrab", "SummerSurveyStrata", "GeorgesSurveyStrata", "AmericanSurveyStrata")
#' @param isobaths = plots bathymetry lines for specified depths from topex data
#' @param bathcol = isobath line color, default is transparent blue
#' @param points.lst = points to overlay on map in PBSmapping format - list with 2 elements: 1st element is eventSet (EID, POS, X, Y), 2nd element is eventData (EID, pch, col, etc.)
#' @param lines.lst = lines to overlay on map in PBSmapping format - list with 2 elements: 1st element is polySet (PID, SID, POS, X, Y), 2nd element is polyData (PID, SID, lty, col, etc.)
#' @param poly.lst = polygons to overlay on map in PBSmapping format - list with 2 elements: 1st element is polySet (PID, SID, POS, X, Y), 2nd element is polyData (PID, SID, border, col, etc.)
#' @param contours = plots overlaping polygons as contours (same format as poly.lst)
#' @param image.lst = image to overlay on map - list with 3 elements (x, y, z), 'bathymetry' produces image from bathymetry data
#' @param color.fun = color function for image
#' @param zlim = zlim for image
#' @param grid = size of grid in degrees, default is no grid
#' @param stippling = adds stippling to land (purely for visual effect)
#' @param lol = adds water colored border to coastline (purely for visual effect)
#' @param ... = undocumented
#' @importFrom PBSmapping plotMap
#' @importFrom PBSmapping addPoints
#' @importFrom PBSmapping addLines
#' @importFrom PBSmapping addPolys
#' @importFrom PBSmapping addStipples
#' @importFrom PBSmapping addLabels
#' @importFrom PBSmapping calcCentroid
#' @importFrom PBSmapping makeGrid
#' @importFrom grDevices adjustcolor
#' @importFrom graphics box
#' @importFrom graphics image
#' @importFrom graphics text
#' @importFrom grDevices rgb
#' @importFrom utils read.csv
#' @author Brad Hubley
#' @examples
#' bioMap(area='lfa34')
#' @export
bioMap<-function(area='custom',ylim=c(40,52),xlim=c(-74,-47),mapRes='HR',land.col='wheat',title='',nafo=NULL,boundaries='LFAs',isobaths=seq(100,1000,100),bathcol=rgb(0,0,1,0.1),points.lst=NULL,pt.cex=1,lines.lst=NULL,poly.lst=NULL,contours=NULL,image.lst=NULL,color.fun=tim.colors,zlim,grid=NULL,stippling=F,lol=F,labels='lfa',labcex=1.5,LT=T,plot.rivers=T,addsubareas=F,subsetSurveyStrata=NULL,addbasemap=F,...){


	# Custom area
	if(area=='custom')	{ ylim=ylim; 			xlim=xlim			}

	## Area List: preset map extents
	if(area=='lfas')		{ ylim=c(42.5,48); 		xlim=c(-67.4,-57.8)	}
	if(area=='west')		{ ylim=c(42.5,46); 		xlim=c(-67.8,-64)	}
	if(area=='SS')			{ ylim=c(41,47); 		xlim=c(-68,-57)		}
	if(area=='ESS')			{ ylim=c(43,45.4); 		xlim=c(-62.5,-57.4)	}
	if(area=='WSS')			{ ylim=c(41,44); 		xlim=c(-67.3,-64)	}
	if(area=='BBn')			{ ylim=c(42.4,43); 		xlim=c(-66.6,-65.6)	}
	if(area=='BBs')			{ ylim=c(42.25,42.75); 	xlim=c(-66,-65.25)	}
	if(area=='BB')			{ ylim=c(42.25,43);		xlim=c(-66.5,-65.25)}
	if(area=='GB')			{ ylim=c(41.1,42.3); 	xlim=c(-67.3,-65.6)	}
	if(area=='GBb')			{ ylim=c(41.6,42.3); 	xlim=c(-66.7,-65.6)	}
	if(area=='Ger')			{ ylim=c(42.8,43.8); 	xlim=c(-67,-65)		}
	if(area=='Sab')			{ ylim=c(42.8,44.5); 	xlim=c(-62.5,-58.8)	}
	if(area=='West')		{ ylim=c(43,44.1); 		xlim=c(-62.2,-60.4)	}
	if(area=='Mid')			{ ylim=c(44.2,44.9);	xlim=c(-61.3,-60.1) }
	if(area=='Ban')			{ ylim=c(43.7,45.2); 	xlim=c(-60.5,-57)	}
	if(area=='SPB')			{ ylim=c(44.5,47.5);	xlim=c(-58,-55)		}
	if(area=='Grand')		{ ylim=c(43,46.5); 		xlim=c(-51.5,-48.5)	}
	if(area=='Grand2')		{ ylim=c(42.5,48); 		xlim=c(-55,-47)		}
	if(area=='lfa27')		{ ylim=c(44.9,47.9); 	xlim=c(-61,-57.8)	}
	if(area=='lfa28')		{ ylim=c(45.3,46);	 	xlim=c(-61.6,-60.3)	}
	if(area=='lfa29')		{ ylim=c(45.3,46); 		xlim=c(-61.6,-60.3)	}
	if(area=='lfa30')		{ ylim=c(44.6,45.9); 	xlim=c(-60.8,-59.6)	}
	if(area=='lfa31a')		{ ylim=c(44.4,45.7); 	xlim=c(-61.8,-60)	}
	if(area=='lfa31b')		{ ylim=c(44.1,45.3); 	xlim=c(-62.2,-60.5)	}
	if(area=='lfa32')		{ ylim=c(43.8,45);	 	xlim=c(-63.5,-61.5)	}
	if(area=='lfa33')		{ ylim=c(42.5,44.8); 	xlim=c(-65.8,-62.2)	}
	if(area=='lfa34')		{ ylim=c(42.5,45);	 	xlim=c(-67.8,-65)	}
	if(area=='lfa35')		{ ylim=c(44.5,46);	 	xlim=c(-66,-63.2)	}
	if(area=='lfa36')		{ ylim=c(44.5,45.7); 	xlim=c(-67.2,-65)	}
	if(area=='lfa37')		{ ylim=c(44,45);		xlim=c(-67.3,-66.4) }
	if(area=='lfa38')		{ ylim=c(44,45);		xlim=c(-67.3,-66.4) }
	if(area=='lfa40')		{ ylim=c(42.25,43);		xlim=c(-66.5,-65.25)}
	if(area=='lfa41')		{ ylim=c(41.1,44); 		xlim=c(-68,-63.5)	}
	if(area=='NENS') 		{ xlim=c(-61,-58.2); 	ylim=c(45.9,47.5) 	}
	if(area=='SENS') 		{ xlim=c(-63.5,-57); 	ylim=c(42.5,46.1)   }
	if(area=='4X')   		{ xlim=c(-67,-63.1); 	ylim=c(42.5,45)     }
	if(area=='23')   		{ xlim=c(-60.5,-57); 	ylim=c(43,46.2)   	}
	if(area=='24')   		{ xlim=c(-63.5,-59); 	ylim=c(42.5,45.5)   }
  if(area=='not4X')  		{ xlim=c(-63.5,-57); 	ylim=c(42.5,47.5)   }
  if(area=='halibut')  	{ xlim=c(-68,-47); 	ylim=c(40,48)   }

	options(stringsAsFactors=F,warn=-1)

	coast<-get(paste0("coast",mapRes))
	rivers<-get(paste0("rivers",mapRes))
	attr(coast,"projection")<-"LL"
	attr(rivers,"projection")<-"LL"


	plotMap(coast,xlim=xlim,ylim=ylim,border=NA,...)
	#addLines(rivers)
	if(addbasemap){
		  addPolys(basemap, col="royalblue2", border="royalblue2")
		  addPolys(dm200, col="steelblue2", border="steelblue2")
		  addPolys(dm100, col="lightblue1", border="lightblue1")

	}

	if(lol)addPolys(coast,border=bathcol,lwd=6)

	# Image
	if(!is.null(image.lst)){
		if(missing(zlim))zlim<-range(image.lst$z,na.rm=T)
		image(image.lst,add=T,col=color.fun(100),zlim=zlim)
	}

	# plot polygons
	if(!is.null(contours)){
		contours[[2]]<-subset(contours[[2]],PID%in%contours[[1]]$PID)
		junk<-data.frame(PID=1,POS=1:4,X=c(162,161,161,162),Y=c(-41,-41,-40,-40)) # KLUDGE!
		for(i in unique(contours[[2]]$PID)){
			addPolys(joinPolys(subset(contours[[1]],PID==i),junk,operation="DIFF"),polyProps=contours[[2]])
		}
	}
	if(!is.null(poly.lst)){
		addPolys(poly.lst[[1]],polyProps=poly.lst[[2]])
	}


	# Bathymetry
		if(!is.null(isobaths)){
			bath.lst<-list()
			for(i in unique(ceiling(isobaths/1000))){
	 			bath.lst[[i]]<-get(paste0("bathy.poly",i))
	 		}
 			bathy.poly<-do.call(rbind,bath.lst)
			bathy.poly<-subset(bathy.poly,Z%in%isobaths)
			attr(bathy.poly,"projection") <- "LL"
			addLines(bathy.poly,polyProps=data.frame(PID=unique(bathy.poly$PID),col=bathcol))
		}

	# NAFO
	if(!is.null(nafo)){
	  if(nafo==TRUE){
	    #NAFO <- rgdal::readOGR("data/NAFODivisions/Divisions.shp")

	    sp::plot(NAFO,add=T,border='grey',col=NULL)
	    text(sp::coordinates(NAFO)[,1], sp::coordinates(NAFO)[,2],NAFO$ZONE,col=rgb(0.5,0.5,0.5,0.5),cex=1)

	  }else{
       # nafo.xy<-read.csv(file.path( project.datadirectory("bio.polygons"), "data","Management_Areas","Fisheries","NAFO","nafo.csv"))
        if(nafo[1]=='all')nafo<-unique(nafo.xy$label)
        nafo.sel<-subset(nafo.xy,label%in%nafo)
        nafo.dat<-merge(calcCentroid(nafo.sel),nafo.sel[c("PID","label")])[!duplicated(nafo.sel[c("PID","label")]),]
        nafo.dat$label[nafo.dat$label=="5ZC"]<-"5ZEM"

		addPolys(nafo.xy,border='grey',col=NULL)
		addLabels(nafo.dat,col=rgb(0.5,0.5,0.5,0.5),cex=2)

	  }

	}

  # Boundries

	#groundfish survey strata
	if('SummerSurveyStrata' %in% boundaries) {

		if(!is.null(subsetSurveyStrata))  SummerStrata = subset(SummerStrata,PID %in% subsetSurveyStrata)
		addPolys(SummerStrata,lty=1,border='lightblue',col=adjustcolor('blue',alpha.f=0.15))
	}
	if('GeorgesSurveyStrata' %in% boundaries) {

		if(!is.null(subsetSurveyStrata))  GeorgesStrata = subset(GeorgesStrata,PID %in% subsetSurveyStrata)
		addPolys(GeorgesStrata,lty=1,border='lightblue',col=adjustcolor('blue',alpha.f=0.15))
	}
	if('AmericanSurveyStrata' %in% boundaries) {

		if(!is.null(subsetSurveyStrata))  AmericanStrata = subset(AmericanStrata,PID %in% subsetSurveyStrata)
		addPolys(AmericanStrata,lty=1,border='lightblue',col=adjustcolor('blue',alpha.f=0.15))
	}

	# LFAs
	if('lobster' %in% boundaries){

		if(area=='31a')area<-311
		if(area=='31b')area<-312
		if(addsubareas)addPolys(subset(subareas,SID==1),lty=3)

		lfa<-as.numeric(area)
		if(lfa%in%LFAgrid$PID){
			if(!is.na(lfa)){
				grids<-subset(LFAgrid,PID==lfa)
				#browser()
				addPolys(grids,border=rgb(0,0,0,0.2),col=NULL)
				if(labels=='grid'){
					grids$label<-grids$SID
	        		grids.dat<-merge(calcCentroid(grids),grids[c("PID","SID","label")])
					#addLabels(subset(grids.dat,!duplicated(label)),col=rgb(0.5,0.5,0.5,0.8),cex=1)
				}
			}
			else {
				addPolys(LFAgrid,border=rgb(0,0,0,0.2),col=NULL)
			}
		}
		#browser()
		addPolys(LFAs)
		if(labels=='lfa'){
			LFAgrid$label<-LFAgrid$PID
			LFAgrid$label[LFAgrid$label==311]<-'31A'
			LFAgrid$label[LFAgrid$label==312]<-'31B'
    		LFAgrid.dat<-merge(calcCentroid(LFAgrid,1),LFAgrid[c("PID","label")])
    		LFAgrid.dat <- subset(LFAgrid.dat,!duplicated(label))
    		il = which(LFAgrid.dat$label==36)
    		LFAgrid.dat$Y[il] = 45.02
    		il = which(LFAgrid.dat$label==35)
			LFAgrid.dat$Y[il] = 45.23

   			LFAgrid.dat = as.data.frame(rbind(LFAgrid.dat,c(41,-66,41.9,41)))	#add in lfa41 label
			#addLabels(subset(LFAgrid.dat,!duplicated(label)),col=rgb(0,0,0,0.8),cex=labcex)
		}

	}

	if('scallop' %in% boundaries){

		addLines(SFA)
		addPolys(SPA,col=NULL)
	}

	if('snowcrab' %in% boundaries){

	    text("CFA 23", x=-58.05, y=44.55, font=2, cex=1.0)
	    text("CFA 24", x=-60.9, y=43.75, font=2, cex=1.0)
	    text("CFA 4X", x=-64.2, y=43.25, font=2, cex=1.0)
	    text("N-ENS", x= -59.15, y=46.65, font=2, cex=1.0)
        addLines(zones, col="darkgoldenrod1", lwd=2)
      }


	addLines(EEZ,lty=4,lwd=2)

	# plots land
	if(LT){
		addPolys(coast,col=land.col,...)
		if(plot.rivers)addLines(rivers,...)
	}

	if(stippling)addStipples (coast, pch='.')



	# plot points
	if(!is.null(points.lst)){
		addPoints(points.lst[[1]],polyProps=points.lst[[2]],cex=pt.cex)
	}

	# plot lines
	if(!is.null(lines.lst)){
		addLines(lines.lst[[1]],polyProps=lines.lst[[2]])
	}

	# add grid lines
	if(!is.null(grid)){
		x<-seq(floor(xlim[1]),ceiling(xlim[2]),grid)
		y<-seq(floor(ylim[1]),ceiling(ylim[2]),grid)
		gridlines<-makeGrid(x,y,byrow=TRUE,addSID=TRUE,projection="LL",zone=NULL)
		addLines(gridlines,col='grey80',lwd=1)
	}
	if('lobster' %in% boundaries){
		if('lfa'%in%labels) addLabels(subset(LFAgrid.dat,!duplicated(label)),col=rgb(0,0,0,0.5),cex=labcex,font=2)
		if('grid'%in%labels) addLabels(subset(grids.dat,!duplicated(label)),col=rgb(0.5,0.5,0.5,0.5),cex=labcex)
		if('subarea'%in%labels) addLabels(subset(grids.dat,!duplicated(label)),col=rgb(0.5,0.5,0.5,0.5),cex=labcex)
	}
	if(is.list(labels)) addLabels(labels[[1]],polyProps=labels[[2]])


	box(lwd=2)


	title(main=title)
	options(warn=0)


}

