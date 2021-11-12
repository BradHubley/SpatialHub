#' @title convertDecDeg
#' @description This function can convert various formats to decimal degrees or to degrees-minutes
#' @param x = coordinates
#' @param format = dec.deg Acceptable values include dec.deg, deg.min
#' @author Brad Hubley
#' @examples
#' convertDecDeg(4730.3)
#' 47.505
#' convertDecDeg(47.505, 'deg.min')
#' 4730.3
#' @family coordinate converters
#' @export
convertDecDeg<-function(x,format='dec.deg'){

  if(format=='dec.deg'){
    dat<-data.frame(ddmm.mm=x,dd.dddd=NA)

    #degrees-minutes-seconds -> degrees
    ddmmss<-dat$ddmm.mm[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)>9000]
    ddmm.ss<-ddmmss/100
    ddmm<-trunc(ddmm.ss)
    ss<-(ddmm.ss-ddmm)*100
    dd.mm<-ddmm/100
    dd<-trunc(dd.mm)
    mm<-(dd.mm-dd)*100
    dat$dd.dddd[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)>9000]<-dd+mm/60+ss/3600

    #degrees-minutes -> degrees
    dd.mmmm<-dat$ddmm.mm[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)>90&abs(dat$ddmm.mm)<9000]/100
    dd<-trunc(dd.mmmm)
    mm.mm<-(dd.mmmm-dd)*100
    dat$dd.dddd[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)>90&abs(dat$ddmm.mm)<9000]<-dd+mm.mm/60

    #degrees -> degrees
    dat$dd.dddd[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)<90]<-dat$ddmm.mm[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)<90]

    return(dat$dd.dddd)
  }

  if(format=='deg.min'){
    dat<-data.frame(ddmm.mm=NA,dd.dddd=x)

    #degrees-minutes-seconds -> degrees-minutes
    ddmmss<-dat$dd.dddd[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>9000]
    ddmm.ss<-ddmmss/100
    ddmm<-trunc(ddmm.ss)
    ss<-(ddmm.ss-ddmm)*100
    dat$ddmm.mm[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>9000]<-ddmm+ss/60

    #degrees-minutes -> degrees-minutes
    dat$ddmm.mm[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>90&abs(dat$dd.dddd)<9000]<-dat$dd.dddd[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>90&abs(dat$dd.dddd)<9000]

    #degrees -> degrees-minutes
    dd.dddd<-dat$dd.dddd[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)<90]
    dd<-trunc(dd.dddd)
    mm.mm<-(dd.dddd-dd)*60
    dat$ddmm.mm[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)<90]<-dd*100+mm.mm

    return(dat$ddmm.mm)
  }

  if(format=='deg.sec'){
    dat<-data.frame(ddmmss=NA,dd.dddd=x)

    #degrees-minutes-seconds -> degrees-minutes-seconds
    dat$ddmmss[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>9000]<-dat$dd.dddd[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>9000]


    #degrees-minutes -> degrees-minutes-seconds
    ddmm.mm<-dat$dd.dddd[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>90&abs(dat$dd.dddd)<9000]
    ddmm<-trunc(dd.dddd)
    ss<-(ddmm.mm-ddmm)*60
    dat$ddmmss[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)>90&abs(dat$dd.dddd)<9000]<-ddmm*100+ss


    #degrees -> degrees-minutes-seconds
    dd.dddd<-dat$dd.dddd[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)<90]
    dd<-trunc(dd.dddd)
    mm.mm<-(dd.dddd-dd)*60

    dat$ddmm.mm[!is.na(dat$dd.dddd)&abs(dat$dd.dddd)<90]<-dd*100+mm.mm
    ddmmss<-dat$ddmm.mm[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)>9000]
    ddmm.ss<-ddmmss/100
    ddmm<-trunc(ddmm.ss)
    ss<-(ddmm.ss-ddmm)*100
    dd.mm<-ddmm/100
    dd<-trunc(dd.mm)
    mm<-(dd.mm-dd)*100
    dat$dd.dddd[!is.na(dat$ddmm.mm)&abs(dat$ddmm.mm)>9000]<-dd+mm/60+ss/3600

    dat$ddmm.mm
  }

}

