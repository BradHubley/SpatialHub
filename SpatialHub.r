devtools::load_all(".")

		setwd("/home/hubleyb/bio/Guru_spatial/Examples")

        devtools::install_github("BradHubley/SpatialHub")
		library(SpatialHub)
		library(PBSmapping)
		bioMap()

		load("/home/hubleyb/bio/Guru_spatial/Examples/data/SetAllocation.rdata")
		load("/home/hubleyb/bio/Guru_spatial/Examples/data/polygons.rdata")
		load("/home/hubleyb/bio/Guru_spatial/Examples/data/predspace.rdata") # temp and depth

		SurveyStrataPolys = b
		PolyData = d

		PolyData$label = PolyData$PID
		PolyData$PID = as.numeric(PolyData$PID)
		PolyData$col = rgb(0,0,1,0.2)
		PolyData$border = 'green'


		bioMap("SS",poly.lst = list(SurveyStrataPolys,PolyData))

		addLabels(PolyData,placement="CENTROID",polys=SurveyStrataPolys)

		addPolys(subset(SurveyStrataPolys,PID==481),col='red')
		addPolys(subset(SurveyStrataPolys,PID==480),col='blue')
		addPolys(subset(SurveyStrataPolys,PID==476),col='red')
		addPolys(subset(SurveyStrataPolys,PID==474),col='blue')

		junk<-data.frame(PID=1,POS=1:4,X=c(162,161,161,162),Y=c(-41,-41,-40,-40))

		SurveyStrataPolys = joinPolys(SurveyStrataPolys,junk,operation="DIFF")

		bioMap("SS",poly.lst = list(SurveyStrataPolys,PolyData))

		addPolys(subset(SurveyStrataPolys,PID==476),col='red')

		SurveyStrata476Polys = joinPolys(subset(SurveyStrataPolys,PID==476),subset(SurveyStrataPolys,PID==474),operation="DIFF")
		addPolys(SurveyStrata476Polys,col='blue')

		SurveyStrataPolys = rbind(subset(SurveyStrataPolys,PID!=476),SurveyStrata476Polys)

		bioMap("SS",poly.lst = list(SurveyStrataPolys,PolyData))

       	# Depth
		xyz = predSpace[c('plon','plat','z')]
		datascale = seq(10,1000,l=50)
		corners = data.frame(lon=c(-67.54,-56.5),lat=c(41,47.2))

		planarMap( xyz, fn="Depth", loc="output", datascale=datascale , corners=corners, log.variable=T,rev=T,save=F)



		# Climatology mean
		xyz = predSpace[c('plon','plat','tmean.climatology')]

		datarange = quantile(xyz[,3],probs=c(0.01,0.99),na.rm=T)
		datascale = seq(datarange[1],datarange[2],l=50)
		corners = data.frame(lon=c(-67.54,-56.5),lat=c(41,47.2))

		load("data/Sim1.rdata")
		planarMap( xyz, fn="tmean.climatology", loc="output", datascale=datascale , corners=corners,save=F,pts=simData)


		## Sim Data
		load("data/Sim1.rdata")

		simData$EID=1:nrow(simData)
		simData$Z = simData$H.V
		data = simData[,c("EID","X","Y","Z")]
		levels = seq(0,1,0.2)

		# grid map
        grids = gridData(data, lvls=levels,grid.size=10)

        bioMap("SS",poly.lst=grids)

        contLegend('bottomleft',lvls=grids$lvls,Cont.data=grids,title="legend",inset=0.02,cex=0.8,bg='white')

		################ needs work

		PolyData$allocation = PolyData$Nsets
		PolyData$PName = PolyData$PID

		PolyData = subset(PolyData,PID%in%SurveyStrataPolys$PID)

  		towlst = allocPoly(poly.lst=list(SurveyStrataPolys, PolyData),pool.size=3,mindist=5,map='SS',UTMzone=20)
