#' @title smoothing
#' description applies a function on spatial data over a grid
#' @author Brad Hubley 
#' @export

smoothing<-function(dat,fun=mean,res=0.01,aspr=1.345640,no.data='0',matrix=F,procedure=1,subset.poly=NULL,expand=0.1){
	
	print("smoothing start")
	print(Sys.time())
	if(is.null(subset.poly)){
	   	Xs<-seq(min(dat$X)-expand,max(dat$X)+expand,res*aspr)
		Ys<-seq(min(dat$Y)-expand,max(dat$Y)+expand, res)
	}
	
   	if(!is.null(subset.poly)){
	   	Xs<-seq(min(subset.poly$X)-res,max(subset.poly$X)+res,res*aspr)
		Ys<-seq(min(subset.poly$Y)-res,max(subset.poly$Y)+res, res)
	}
	Z<-matrix(NA,length(Xs),length(Ys))
	CoV<-matrix(NA,length(Xs),length(Ys))
	
	for(i in 1:(length(Xs)-1)){
		for(j in 1:(length(Ys)-1)){
			square<-data.frame(X=c(Xs[i],Xs[i+1],Xs[i+1],Xs[i],Xs[i]),Y=c(Ys[j],Ys[j],Ys[j+1],Ys[j+1],Ys[j]))
			sq.dat<-dat[with(dat, inout(cbind(X,Y), square, bound = T)),]
			if(procedure==1){
				if(no.data=='0') Z[i,j]<-sum(fun(sq.dat$Z),na.rm=T)
				if(no.data=='NA') Z[i,j]<-fun(sq.dat$Z)
			}
			if(procedure==2){
				if(no.data=='0') Z[i,j]<-sum(sum(sq.dat$Z,na.rm=T)/sum(sq.dat$CoV,na.rm=T),na.rm=T)
				if(no.data=='NA') Z[i,j]<-sum(sq.dat$Z,na.rm=T)/sum(sq.dat$CoV,na.rm=T)
			}
			if(procedure==3){
				Z[i,j]<-sum(sq.dat$Z,na.rm=T)
			}
			if(procedure==4){
				Z[i,j]<-sum(sq.dat$CoV,na.rm=T)
			}
			if(procedure==5){
				n<-nrow(sq.dat)
				if(no.data=='0') Z[i,j]<-sum(mean(sapply(1:nrow(sq.dat),function(j){nrow(sq.dat)*sum(sq.dat$catch,na.rm=T)/sum(sq.dat$effort,na.rm=T) - (nrow(sq.dat)-1)*(sum(sq.dat$catch[-j])/sum(sq.dat$effort[-j]))})),na.rm=T)
				if(no.data=='NA') Z[i,j]<-mean(sapply(1:nrow(sq.dat),function(j){nrow(sq.dat)*sum(sq.dat$catch,na.rm=T)/sum(sq.dat$effort,na.rm=T) - (nrow(sq.dat)-1)*(sum(sq.dat$catch[-j])/sum(sq.dat$effort[-j]))}))
			}
			Y<-sort(rep(Ys,length(Xs)))
			X<-rep(Xs,length(Ys))
			if(matrix==F)	result<-data.frame(X=X+0.5*res,Y=Y+0.5*res,Z=as.vector(Z))
			if(matrix==T)	result<-list(X=Xs,Y=Ys,Z=Z)
			if(procedure==6){
				Z[i,j]<-fun(sq.dat$Z)
				CoV[i,j]<-fun(sq.dat$CoV)
				if(matrix==F)	result<-data.frame(X=X+0.5*res,Y=Y+0.5*res,Z=as.vector(Z),CoV=as.vector(CoV))
				if(matrix==T)	result<-list(X=Xs+0.5*res,Y=Ys+0.5*res,Z=Z,CoV=CoV)

			}
		}
		if(i %in% round(seq(length(Xs)/100,length(Xs),length(Xs)/100)))print(paste(round(i/length(Xs)*100),"%"))
	}

	
	print("smoothing end")
	print(Sys.time())
	
	return(result)
}

