#' @export

# tick.def function: ("Y:/Development/Georges/Survey Design/r/fn/tick.def.r"): 
# defines ticks or strata boundaries using the sqrt(f(y)) rule

define.strata<-function(char,nstrata=4,min.str=0,max.str,place=0){
	
	
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
# blank.bank.r ("Y:/Development/Georges/Survey Design/r/fn/blank.bank.r"): 
# incorporates blanking distance by including zeros spaced eqully at the average nearest nieghbour distance, default blanking distance is the nearest neighbour distance of the most isolated point
