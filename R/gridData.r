#' @title gridData
#' @description This function summarizes data onto a grid, creates PBSmapping polySet and polyData
#' @param Data = PBSmapping::EventData
#' @param domain.poly = polygon the defines the total area from which to construct the grid, dervied from data if missing
#' @param lvls = levels
#' @param bcol = RColorBrewer color palette
#' @param border = polygon border
#' @param FUN = summary function to apply to data within a grid cell
#' @param grid.size = size of the grid cells (km)
#' @param aspr = aspect ratio for for creating square grid, default is to calculate from data
#' @param sx = defines total extent to grid, defaults to the extent of the data
#' @param sy = defines total extent to grid, defaults to the extent of the data
#' @param ex = defines total extent to grid, defaults to the extent of the data
#' @param ey = defines total extent to grid, defaults to the extent of the data
#' @importFrom RColorBrewer brewer.pal
#' @importFrom PBSmapping combineEvents
#' @importFrom PBSmapping findCells
#' @importFrom PBSmapping findPolys
#' @importFrom PBSmapping makeGrid
#' @importFrom PBSmapping makeProps
#' @importFrom gstat fit.variogram
#' @author Brad Hubley
#' @export

gridData <- function(Data,domain.poly,lvls,bcol="YlGnBu",border=1,FUN=mean,grid.size=1,aspr="calculate",sx,sy,ex,ey) {

	names(Data)[1:4]<-c("EID","X","Y","Z")

	# Domain polygon to select data
	if(!missing(domain.poly))Data <- subset(Data,EID%in%findPolys(Data,domain.poly)$EID)

	if(missing(sx)) sx = floor(min(Data$X))
	if(missing(sy))	sy = floor(min(Data$Y))
	if(missing(ex)) ex = ceiling(max(Data$X))
	if(missing(ey))	ey = ceiling(max(Data$Y))

	# Aspect ratio
	if(aspr=="calculate"){
		aspr=1/cos(rad(mean(c(sy,ey))))
	}

	# Make grid
	gx = grid.size/111.12 * aspr
	gy = grid.size/111.12
	grid   <- makeGrid(x=seq(sx,ex,gx),y=seq(sy,ey,gy),projection="LL")


	# locate EventData in grid and create polyData with summary stats
	locData<- findCells(Data, grid)
	pdata  <- combineEvents(Data, locData, FUN=FUN)

	cols   <- brewer.pal(length(lvls),bcol)
	pdata  <- makeProps(pdata, c(lvls,max(lvls)*100), "col", cols)
	pdata$border<-border

	return(list(polys=grid, polyData=pdata, lvls=lvls, col=cols))

}
