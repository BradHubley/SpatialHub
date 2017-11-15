#' @title defineStrata
#' @description defines strata boundaries using the sqrt(f(y)) rule (Cochran 1977)
#' @param char = vector of values of the variable used to define strata
#' @param nstrata = number of strata desired
#' @param min.str = minimum value of stratifing variable considered
#' @param max.str = maximum value of stratifing variable considered
#' @param place = decimal places used to define strata boundaries
#' @author Brad Hubley 
#' @export

defineStrata<-function(char,nstrata=4,min.str=0,max.str,place=0){
	
	
	bin<-round(char,place)
	if(missing(max.str))max.str<-max(bin)

	bins<-sort(unique(bin[bin>0]))
	vars<-c()
	avg<-c()
	N<-c()
	srN<-c()
 
 
 	for(i in 1:length(bins)){
	 	
		avg[i]<-mean(char[bin==bins[i]])
		vars[i]<-var(char[bin==bins[i]])
		N[i]<-length(char[bin==bins[i]])
		srN[1]<-sqrt(N[1])
		if(i>1) srN[i]<-sqrt(N[i])+srN[i-1]
 	}
 
  	ideal.div<-max(srN)/nstrata*(1:nstrata)
  	ind<-c()	
  	for(i in 1:length(ideal.div)){
		ind[i] <- which(srN==srN[abs(srN-ideal.div[i])==min(abs(srN-ideal.div[i]))])
	}	

	str<-c(min.str,bins[ind]) 
	str[length(str)]<-max.str
	
	str
	
}
