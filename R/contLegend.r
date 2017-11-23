#' @title ContLegend
#' @description Creates a legend for contour data
#' @param lvls = undocumented
#' @param Cont.data = undocumented
#' @param ... = undocumented
#' @importFrom graphics legend
#' @export
contLegend<-function(...,lvls,Cont.data){
	legend(...,legend=c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep='')),fill=Cont.data$col)
}
