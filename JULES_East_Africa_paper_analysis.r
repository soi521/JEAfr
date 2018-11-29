
#NOTE: if I change this code significantly in the future I should regenerate the file(s) in D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/ again (just delete them and run the script as normal once to regenerate, but it does seem to take ~10 min).

#Check domarmap=TRUE for the final versions of these plots (regenerate the big .RData file if you change this).

#How to use this script:
#e.g. for precip against ET plots.
#Run through first time selecting 2 (for real run) then 52 then 10. Copy the arrows plots and plotprobs=FALSE spatial plots, but ignore the return time plots
#Run inverted for the return time plots: choose 2 then 54 then 44. I used the plotprobs=TRUE spatial plots from here and the return time plots for ET (I manually scrubbed the minus signs from the y-axes on those). The plotprobs=TRUE arrows plot I liked here: I manually changed the axes in Paint to say "Prob( precipitation<30 mm/mo )" and "Prob( evapotranspiration<30 mm/mo )"

#For Fig. 5 it was 36-5 and Fig. 6 53-46.

#For the Table of return times the thresholds are in printts (numbers for the table will only appear if choice8=54 is one of the axes). Get the 4 related spatial plots like this: Run 54-44 from scratch. When complete, change the threshold using rttspatialy=-10/multipliery;plotprobs=TRUE and rerun the spatial plots bit ("indx1=which ... rtp=rtspatial") from the end of this script.





newmemlimit=8191		#The default memory limit is probably 5922Mb. My machine allows it to be changed up to any amount, though the man page says there's a limit of 8 Tb (=8192 Mb)
cat("Changing memory limit from",memory.limit(),"Mb to",newmemlimit,"Mb (only has an effect on Windows R, so if you are on Linux/UNIX just ignore the \"'memory.limit()' is Windows-specific\" warnings this generates).\n")
memory.limit(size=newmemlimit)

library(mapdata)
frgmap=map('worldHires',plot=FALSE)
library(sp)
gadmSSD=readRDS("D:/D.DRIVE/programs/otherprogs/SSD_adm0.rds")	#Download this file from http://gadm.org/download (South Sudan)
gadmERI=readRDS("D:/D.DRIVE/programs/otherprogs/ERI_adm0.rds")	#Download this file from http://gadm.org/download (Eritrea)
gadmYEM=readRDS("D:/D.DRIVE/programs/otherprogs/YEM_adm0.rds")	#Download this file from http://gadm.org/download (Yemen)
gadmOMN=readRDS("D:/D.DRIVE/programs/otherprogs/OMN_adm0.rds")	#Download this file from http://gadm.org/download (Oman)
gadmSAU=readRDS("D:/D.DRIVE/programs/otherprogs/SAU_adm0.rds")	#Download this file from http://gadm.org/download (Saudi)
library(rgdal)
glwd1ogr=readOGR(dsn="D:/GISwork/MAPFILES_worldwide/GLWD",layer="glwd_1")	#i.e. file D:/GISwork/MAPFILES_worldwide/GLWD/glwd_1.shp
lakepolygons=subset(glwd1ogr,TYPE=="Lake")	#i.e. lakes from the GLWD
library(rgeos)

sortborderpts=function(borderlons,borderlats){

nn=length(borderlons);allpts=1:nn
borderlonsmod=c(borderlons[1],rep(NA,times=nn-1));borderlatsmod=c(borderlats[1],rep(NA,times=nn-1))
newpts=1;checklist=c(0,rep(1,times=nn-1))
currentptnum=1;currentptlon=borderlons[1];currentptlat=borderlats[1]
repeat {
 mindist=360;possptnum=NA
 for (ii in allpts) {
  if (checklist[ii]>0) {
   if (!is.finite(borderlons[ii])) {checklist[ii]=0} else {
    dist=sqrt(((currentptlon-borderlons[ii])^2)+((currentptlat-borderlats[ii])^2))
    if (dist<=mindist) {
     if (dist!=mindist) {
      possptnum=ii;mindist=dist
     } else {
      if (abs(currentptnum-ii)<abs(currentptnum-possptnum)) {possptnum=ii}
     }
    }
   }
  }
 }
 if (is.finite(possptnum)) {
  if (mindist>2) {break}	#For a JSON dataset 2 seems about right here: this chops off thin 'arms' of the country outline that weren't captured by the loop because they were undersampled
  checklist[possptnum]=0
  currentptnum=possptnum;currentptlon=borderlons[currentptnum];currentptlat=borderlats[currentptnum]
  newpts=newpts+1;borderlonsmod[newpts]=currentptlon;borderlatsmod[newpts]=currentptlat
 }
 if (sum(checklist)==0) {break}
}

return(data.frame(borderlonsmod[1:newpts],borderlatsmod[1:newpts]))
}

if (file.exists("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/bordercalcs.RData")) {
 cat("\nLoading in country border data\n")
 load("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/bordercalcs.RData")
} else {
 cat("\nSorting country border points\n")
 mapkenya=map("worldHires","Kenya",plot=FALSE)
 aa=sortborderpts(mapkenya$x,mapkenya$y);mapkenya$x=aa$borderlonsmod;mapkenya$y=aa$borderlatsmod
 mapsomalia=map("worldHires","Somalia",plot=FALSE)
 aa=sortborderpts(mapsomalia$x,mapsomalia$y);mapsomalia$x=aa$borderlonsmod;mapsomalia$y=aa$borderlatsmod
 mapuganda=map("worldHires","Uganda",plot=FALSE)
 aa=sortborderpts(mapuganda$x,mapuganda$y);mapuganda$x=aa$borderlonsmod;mapuganda$y=aa$borderlatsmod
 maprwanda=map("worldHires","Rwanda",plot=FALSE)
 aa=sortborderpts(maprwanda$x,maprwanda$y);maprwanda$x=aa$borderlonsmod;maprwanda$y=aa$borderlatsmod
 maptanzania=map("worldHires","Tanzania",plot=FALSE)
 aa=sortborderpts(maptanzania$x,maptanzania$y);maptanzania$x=aa$borderlonsmod;maptanzania$y=aa$borderlatsmod
 maplakevic=map("worldHires","Lake Victoria",plot=FALSE)
 aa=sortborderpts(maplakevic$x,maplakevic$y);maplakevic$x=aa$borderlonsmod;maplakevic$y=aa$borderlatsmod
 mapsudan=map("worldHires","Sudan",plot=FALSE)
 aa=sortborderpts(mapsudan$x,mapsudan$y);mapsudan$x=aa$borderlonsmod;mapsudan$y=aa$borderlatsmod
 mapethiopia=map("worldHires","Ethiopia",plot=FALSE)
 aa=sortborderpts(mapethiopia$x,mapethiopia$y);mapethiopia$x=aa$borderlonsmod;mapethiopia$y=aa$borderlatsmod
 rm(aa)
 save.image("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/bordercalcs.RData")

#Example of doing the border using a JSON object:
#aa=scan(file="https://raw.githubusercontent.com/mledoze/countries/bb61a1cddfefd09ad5c92ad0a1effbfceba39930/data/deu.geo.json",what="character",sep="")
#bb=scan(text=gsub("\\]","],",gsub(".\\[",",[",aa)),what="",sep=",")
#cc=bb[8:(length(bb)-5)]
#dd=as.numeric(unlist(strsplit(unlist(strsplit(cc,split="[[]")),split="[]]")))
#borderlons=dd[seq(from=1,to=length(dd)-1,by=2)]
#borderlats=dd[seq(from=2,to=length(dd),by=2)]
#However, the following doesn't work:
#lat1=50.25376;lon1=8.17775	#This point is inside Germany
#lat2=48.87146;lon2=2.355	#This point is outside Germany (in France)
#point.in.polygon(lon1,lat1,borderlons,borderlats)	#Should say 1, but says 2
#point.in.polygon(lon2,lat2,borderlons,borderlats)	#Should say 0, but says 2
#Why not? It's because the border points are not in order, as you can see from this:
#plot(x=borderlons,y=borderlats)
#for (ii in 1:length(borderlons)) {Sys.sleep(0.01);points(x=borderlons[ii],y=borderlats[ii],col=ii)}
#Sort this out using my function above and the test points will pass:
#aa=sortborderpts(borderlons,borderlats)
#borderlons=aa$borderlonsmod;borderlats=aa$borderlatsmod

}

plotcountries=TRUE;ncoe2o=FALSE;showallcells=FALSE;plotbrokenlines=TRUE
qtr=7
printts=c(-5,-10,-20,-30)	#Print out at these thresholds (negative because they are taken from the return times on inverted plots)
spavg=TRUE	#For replicating Fredi's return time plots put spavg=TRUE (i.e. plotting an average ensemble line rather than each one separate (like Otto et al. 2013, but not from bootstrapping)). Alternatively, though I have not used this in the JEAfr paper, spavg=FALSE is for plotting each ensemble separately (i.e. as in Pall et al. 2011:Fig. 3).
plotprobs=TRUE	#Retain a small amount of extra functionality by keeping this in: plotprobs=FALSE I may use again in the future ...
#For the state space plots:
cpal1=c("violet","violet","royalblue","navy","cyan","blue","violet","violet","violet","darkseagreen3","green3","olivedrab")

cat("In this script I specify the y variable then the x variable, e.g. for the JEAfr arrows plot I should choose 52 then 10.\n")

choice2=menu(c(paste("QUICK TEST RUN: Read only the first few ensemble members from CF and F ensembles (",qtr," CF and ",qtr," F)",sep=""),"REAL RUN: Read in all JULES results (100 CF and 100 F)"))

choice6=2	#choice6=menu(c("Log-transform the y axis on the return-time plot","No log-transformation of the y axis on the return-time plot (all examples found so far had an un-transformed y)"))

cat("\n\nSELECT THE VARIABLE FOR THE Y AXIS FROM THE MENU BELOW\n\n")
source("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/QuickHadDataStats_part1.r")
zCF[!is.finite(zCF)]=NA;zF[!is.finite(zF)]=NA
yyCF=zCF;rm(zCF);yyF=zF;rm(zF);ytext=vartxt
choice8y=choice8;multipliery=multiplier;axisunitsy=axisunits;rttsy=rtts;rttspatialy=rttspatial

cat("\nReading in TAMSAT data.\n")
LongRainsTAMvec=rep(NA,times=length(lonindexTAM)*length(fliplatindexTAM));lonsTAMvec=rep(NA,times=length(lonindexTAM)*length(fliplatindexTAM));latsTAMvec=rep(NA,times=length(lonindexTAM)*length(fliplatindexTAM))
for (ii in 1:length(lonindexTAM)) {
 for (jj in 1:length(fliplatindexTAM)) {
  stckindx=((ii-1)*length(fliplatindexTAM))+jj
  LongRainsTAMvec[stckindx]=LongRainsTAM[ii,jj]/(60*60*24*(365.25/12)*3)	#LongRainsTAM is in mm/(3 month period) and I need to convert to mm/s
  lonsTAMvec[stckindx]=lons[ii];latsTAMvec[stckindx]=lats[jj]
 }
}
if (doinitialplots) {dev.new();print(levelplot(LongRainsTAMvec~lonsTAMvec*latsTAMvec,asp=1))}
tamdf=data.frame(LongRainsTAMvec,lonsTAMvec,latsTAMvec)

cat("\nReading in details of grid and subregions to be used.\n")
source("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/specifyareas.r")
ssdf=data.frame(cellnumtxt,cpal2,areanames,MMs,latmins,latmaxs,lonmins,lonmaxs)
areanamesu=unique(ssdf$areanames)

cat("\nMapping array rows to specified regions.\n")
#I can't use the subset command if I need to retain the ensemble members so I need a 'map' to get the row numbers of each region:
coords=data.frame(Longitude,Latitude)
rownummap=matrix(NA,nrow=length(areanamesu),ncol=length(Longitude))
for (uu in 1:length(areanamesu)) {
 if (areanamesu[uu]!="TC") {
  ssdfreg=subset(ssdf,areanames==areanamesu[uu])	#Pick out all cells in one region
  counter=1
  for (pixel1 in 1:nrow(coords)) {
   inside=FALSE
   for (subarea1 in 1:nrow(ssdfreg)) {
    if (coords$Longitude[pixel1]>=ssdfreg$lonmins[subarea1] & coords$Longitude[pixel1]<=ssdfreg$lonmaxs[subarea1] & coords$Latitude[pixel1]>=ssdfreg$latmins[subarea1] & coords$Latitude[pixel1]<=ssdfreg$latmaxs[subarea1]) {inside=TRUE}
   }
  if (inside) {rownummap[uu,counter]=pixel1;counter=counter+1}
  }
 }
}
#After this, I can use the 'map' like this:
#For region uu I know that rownummap[uu,] will be the row numbers in coords that are contained in uu, but I need to get rid of the NAs so if rownumvec=rownummap[uu,][is.finite(rownummap[uu,])] then yyCF[rownumvec,] will pick out the right rows for region uu.

cat("\n\nSELECT THE VARIABLE FOR THE X AXIS FROM THE MENU BELOW\n\n")
source("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/QuickHadDataStats_part1.r")
zCF[!is.finite(zCF)]=NA;zF[!is.finite(zF)]=NA
xxCF=zCF;rm(zCF);xxF=zF;rm(zF);xtext=vartxt
choice8x=choice8;multiplierx=multiplier;axisunitsx=axisunits;rttsx=rtts;rttspatialx=rttspatial

cat(paste("\nCalculations finished up to the plots (copy in the code after this print statement to regenerate all plots)\n\tCalculations were for y=(var #",choice8y,") against x=(var #",choice8x,") (with ",nCF,"+",nF," ensemble members)\n\n",sep=""))
cat("Saving workspace",paste("JEAfr_",choice8y,"_",choice8x,".RData",sep=""),"\n")
save.image(paste("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/",choice8y,"_",choice8x,".RData",sep=""))

source("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/BAMSplots.r")	#Reload just in case I'm updated it since saving the .RData files loaded in before

for (plotprobs in c(TRUE,FALSE)) {
#LOSE AN INDENT

rtp=arrowplot(ssdf,rownummap,xxCF,xxF,yyCF,yyF,Longitude,Latitude,tamdf,choice8x,choice8y,rttspatialx,rttspatialy,FALSE,FALSE,TRUE,FALSE,FALSE,3,multiplierx,multipliery,plotprobs)

if (plotprobs) {	#Return-time plots with plotprobs=FALSE, to be consistent with the rest of the code, would require me replacing ifelse(xxCF[rownumvec,ensmem]>thresh,1,0) with xxCF[rownumvec,ensmem] (etc.) below and then also setting rtimexaxis=FALSE so that the reciprocal isn't taken. Although I could have done that, note that having plotprobs=TRUE doesn't mean the y-axis on these plots is made up of thresholds between 0 and 1, but having return time on the x-axis.
#LOSE INDENT

#Do the region-by-region return-time plots:
dowrongcalc=FALSE	#Putting dowrongcalc=TRUE makes crazy error bars so I can't use it. Hm: actually, NO: it's not just the crazy error bars - having dowrongcalc=TRUE is mathematically NOT CORRECT: you should only quote mean +/- SD for quantities that are approximately normal and probabilities should never be expected to be normal. Return times have units (yrs) so it is legitimate to assume that (GLM notes; though note that assumption implies that the probabilities have a reciprocal normal distribution and according to https://math.stackexchange.com/questions/646428/mean-and-variance-of-reciprocal-normal-distribution that means they don't have a mean or SD in general anyway? Or perhaps they do because we can assume them to be binomial??).
for (uu in 1:length(areanamesu)) {
 if (areanamesu[uu]!="TC") {
  placetxt=paste(" for ",areanamesu[uu],sep="")
  rownumvec=rownummap[uu,][is.finite(rownummap[uu,])]
  rtxvaluesCF=matrix(NA,nrow=nthr,ncol=nCF);rtxvaluesF=matrix(NA,nrow=nthr,ncol=nF)
  rtyvaluesCF=matrix(NA,nrow=nthr,ncol=nCF);rtyvaluesF=matrix(NA,nrow=nthr,ncol=nF)
  rtxvaluesCFmn=rep(NA,times=nthr);rtxvaluesCFsd=rep(NA,times=nthr)
  rtxvaluesFmn=rep(NA,times=nthr);rtxvaluesFsd=rep(NA,times=nthr)
  rtyvaluesCFmn=rep(NA,times=nthr);rtyvaluesCFsd=rep(NA,times=nthr)
  rtyvaluesFmn=rep(NA,times=nthr);rtyvaluesFsd=rep(NA,times=nthr)
  for (thr1 in 1:nthr) {
   threshx=rttsx[thr1];threshy=rttsy[thr1]

#Options here were:
#(a) At each pixel, take the ensemble mean (ignore ensemble spread). Then average over the pixels within each region and record the spatial variability.
#(b) For each ensemble, average over the pixels within the region (ignore spatial variability). Then average over the ensemble and record the ensemble spread.
#Additionally, this needs to be done for each of nthr thresholds (you could argue that spatial averaging of probabilities actually requires some kind of logit transformation, but I haven't coded that: decided to just go with the straightforward means)
#In my pre-June 2018 analysis I did (a) because I was far more interested in regional variation and basically willing to take ensemble means on faith with no consideration for the ensemble spread. During June/July 2018 I had to rewrite this and all related scripts completely to instate code that calculated the ensemble spread to move over to (b). I've had to cave because (a) the Climate Lab just never understood why I could possibly not want to focus the paper on ensemble spread (also perhaps they felt there was no useful information at the regional spatial scale because of the known inaccuracies of the models). my argument that even though there's uncertainty - there's always uncertainty - this regional scale is what is of interest to the actual people who live in GHoA cut no ice whatsoever and just caused confusion. I now do accept that (1) I've now got fairly small and sensible regions that presumably experience a fairly homogeneous climate each (have not proved that but as far as I can make out it seems reasonable to assume it) and (2) Perhaps because I chose my regions right, there's very little regional variation anyway.
#All the Climate Lab examples have the equivalent of rtimexaxis=TRUE, but because the probability is (a) spatial (b) across ensembles rather than temporal (1/rtime is actually the proportion of (a) pixels (b) ensembles that exceeded the threshold not the proportion of time one pixel exceeded the threshold), this is only an approximation to a return time (in a memoryless random kind of climate regime this would be OK, I guess theoretically, but is that really what climatologists assume???)

   for (ensmem in 1:nCF) {
    if (dowrongcalc) {
     rtxvaluesCF[thr1,ensmem]=mean(ifelse(xxCF[rownumvec,ensmem]>threshx,1,0),na.rm=TRUE)	#xxCF[rownumvec,ensmem] is a vector as long as the no. of pixels in region uu. You can plot it out using levelplot(xxCF[,ensmem]~Longitude+Latitude). Must take the mean before the reciprocal rather than the other way round because you can't average [1,1,Inf,Inf] but you can average [1,1,0,0]
     rtyvaluesCF[thr1,ensmem]=mean(ifelse(yyCF[rownumvec,ensmem]>threshy,1,0),na.rm=TRUE)
    } else {
     mn=mean(ifelse(xxCF[rownumvec,ensmem]>threshx,1,0),na.rm=TRUE)	#xxCF[rownumvec,ensmem] is a vector as long as the no. of pixels in region uu. You can plot it out using levelplot(xxCF[,ensmem]~Longitude+Latitude). Must take the mean before the reciprocal rather than the other way round because you can't average [1,1,Inf,Inf] but you can average [1,1,0,0]
     rtxvaluesCF[thr1,ensmem]=1/mn
     if (is.finite(mn)) {if (mn==0) {rtxvaluesCF[thr1,ensmem]=nCF}}	#A return time of >nCF in this study is arguably (and I do argue) a null result
     mn=mean(ifelse(yyCF[rownumvec,ensmem]>threshy,1,0),na.rm=TRUE)
     rtyvaluesCF[thr1,ensmem]=1/mn
     if (is.finite(mn)) {if (mn==0) {rtyvaluesCF[thr1,ensmem]=nCF}}
    }
   }
   for (ensmem in 1:nF) {
    if (dowrongcalc) {
     rtxvaluesF[thr1,ensmem]=mean(ifelse(xxF[rownumvec,ensmem]>threshx,1,0),na.rm=TRUE)
     rtyvaluesF[thr1,ensmem]=mean(ifelse(yyF[rownumvec,ensmem]>threshy,1,0),na.rm=TRUE)
    } else {
     mn=mean(ifelse(xxF[rownumvec,ensmem]>threshx,1,0),na.rm=TRUE)
     rtxvaluesF[thr1,ensmem]=1/mn
     if (is.finite(mn)) {if (mn==0) {rtxvaluesF[thr1,ensmem]=nF}}	#A return time of >nF in this study is arguably (and I do argue) a null result
     mn=mean(ifelse(yyF[rownumvec,ensmem]>threshy,1,0),na.rm=TRUE)
     rtyvaluesF[thr1,ensmem]=1/mn
     if (is.finite(mn)) {if (mn==0) {rtyvaluesF[thr1,ensmem]=nF}}
    }
   }
  }
  rtyvaluesCF[!is.finite(rtyvaluesCF)]=NA;rtyvaluesF[!is.finite(rtyvaluesF)]=NA
  rtxvaluesCF[!is.finite(rtxvaluesCF)]=NA;rtxvaluesF[!is.finite(rtxvaluesF)]=NA
  for (thr1 in 1:nthr) {	#Average over the ensemble
   rtxvaluesCFmn[thr1]=mean(rtxvaluesCF[thr1,],na.rm=TRUE)
   rtxvaluesFmn[thr1]=mean(rtxvaluesF[thr1,],na.rm=TRUE)
   rtyvaluesCFmn[thr1]=mean(rtyvaluesCF[thr1,],na.rm=TRUE)
   rtyvaluesFmn[thr1]=mean(rtyvaluesF[thr1,],na.rm=TRUE)
   numvalues=sum(is.finite(rtxvaluesCF[thr1,]),na.rm=TRUE)
   rtxvaluesCFsd[thr1]=sd(rtxvaluesCF[thr1,],na.rm=TRUE)/sqrt(numvalues)	#Take SE not SD
   numvalues=sum(is.finite(rtxvaluesF[thr1,]),na.rm=TRUE)
   rtxvaluesFsd[thr1]=sd(rtxvaluesF[thr1,],na.rm=TRUE)/sqrt(numvalues)
   numvalues=sum(is.finite(rtyvaluesCF[thr1,]),na.rm=TRUE)
   rtyvaluesCFsd[thr1]=sd(rtyvaluesCF[thr1,],na.rm=TRUE)/sqrt(numvalues)
   numvalues=sum(is.finite(rtyvaluesF[thr1,]),na.rm=TRUE)
   rtyvaluesFsd[thr1]=sd(rtyvaluesF[thr1,],na.rm=TRUE)/sqrt(numvalues)
   if (dowrongcalc) {
    mn=rtxvaluesCFmn[thr1];sd=rtxvaluesCFsd[thr1]
    lower=pmin(nCF,pmax(0,1/(mn+sd)));upper=pmin(nCF,pmax(0,1/(mn-sd)))
    rtxvaluesCFmn[thr1]=1/mn
    if (is.finite(mn)) {if (mn==0) {rtxvaluesCFmn[thr1]=nCF}}
    if (is.finite(sd)) {if (sd>0) {rtxvaluesCFsd[thr1]=(upper-lower)/2}}	#ERROR: in this case the error range is asymmetrical so even though I can assign an 'SD' like this, it's really half the width of the range and the mean value (1/mn) is not going to be the midpoint of it. I need to track both upper and lower actually. HOWEVER, I'm now convinced that dowrongcalc=TRUE is mathematically misguided anyway so have not bothered to correct this.
    mn=rtxvaluesFmn[thr1];sd=rtxvaluesFsd[thr1]
    lower=pmin(nF,pmax(0,1/(mn+sd)));upper=pmin(nF,pmax(0,1/(mn-sd)))
    rtxvaluesFmn[thr1]=1/mn
    if (is.finite(mn)) {if (mn==0) {rtxvaluesFmn[thr1]=nF}}
    if (is.finite(sd)) {if (sd>0) {rtxvaluesFsd[thr1]=(upper-lower)/2}}
    mn=rtyvaluesCFmn[thr1];sd=rtyvaluesCFsd[thr1]
    lower=pmin(nCF,pmax(0,1/(mn+sd)));upper=pmin(nCF,pmax(0,1/(mn-sd)))
    rtyvaluesCFmn[thr1]=1/mn
    if (is.finite(mn)) {if (mn==0) {rtyvaluesCFmn[thr1]=nCF}}
    if (is.finite(sd)) {if (sd>0) {rtyvaluesCFsd[thr1]=(upper-lower)/2}}
    mn=rtyvaluesFmn[thr1];sd=rtyvaluesFsd[thr1]
    lower=pmin(nF,pmax(0,1/(mn+sd)));upper=pmin(nF,pmax(0,1/(mn-sd)))
    rtyvaluesFmn[thr1]=1/mn
    if (is.finite(mn)) {if (mn==0) {rtyvaluesFmn[thr1]==nF}}
    if (is.finite(sd)) {if (sd>0) {rtyvaluesFsd[thr1]=(upper-lower)/2}}
   }
  }
  rtp=rtplot(rtyvaluesCFmn,rtyvaluesCFsd,rtyvaluesFmn,rtyvaluesFsd,rttsy,ytext,"",placetxt,axisunitsy,multipliery,accuracy,choice6,choice8y,rtimexaxis,plotpertxt[plotpervar],printts)
  if (choice8x!=choice8y) {	#No need to repeat the return time plots if have already done them above
   rtp=rtplot(rtxvaluesCFmn,rtxvaluesCFsd,rtxvaluesFmn,rtxvaluesFsd,rttsx,xtext,"",placetxt,axisunitsx,multiplierx,accuracy,choice6,choice8x,rtimexaxis,plotpertxt[plotpervar],printts)
  }
 }
}

#REGAIN INDENT
}
















#Toby's spatial plots
indx1=which(rttsy>rttspatialy)[1]	#The spatial plot is effectively a horizontal cross-section of the corresponding return time plot at y=indx1, but without the spatial averaging.
thresh=rttsy[indx1]	#n.b. if plotprobs=FALSE then thresh isn't used for this plot
rtyvaluesCFmnSP=rep(NA,times=npts);rtyvaluesFmnSP=rep(NA,times=npts)
for (ww in 1:npts) {
#For these plots, I only have one threshold, which is chosen by me (thresh) and then for each pixel I take an ensemble mean (I don't calculate the ensemble spread (SD) for this plot)
 if (plotprobs) {tmp=ifelse(yyCF[ww,1:nCF]>thresh,1,0)} else {tmp=yyCF[ww,1:nCF]*multipliery}
 rtyvaluesCFmnSP[ww]=mean(tmp,na.rm=TRUE)	#In the case of plotprobs=TRUE, I'm ignoring the way I should probably use a logit transformation when taking the mean of probabilities
 if (plotprobs) {tmp=ifelse(yyF[ww,1:nF]>thresh,1,0)} else {tmp=yyF[ww,1:nF]*multipliery}
 rtyvaluesFmnSP[ww]=mean(tmp,na.rm=TRUE)
}
rtyvaluesCFmnSP[!is.finite(rtyvaluesCFmnSP)]=NA;rtyvaluesFmnSP[!is.finite(rtyvaluesFmnSP)]=NA
rtp=rtspatial(rtyvaluesCFmnSP,rtyvaluesFmnSP,rttspatialy,plotprobs,ytext,"",axisunitsy,multipliery,accuracy,choice6,choice8y,plotpertxt[plotpervar])


if (choice8x!=choice8y) {	#No need to repeat the spatial plot if have already done it above
 indx1=which(rttsx>rttspatialx)[1]
 thresh=rttsx[indx1]	#n.b. if plotprobs=FALSE then thresh isn't used for this plot
 rtxvaluesCFmnSP=rep(NA,times=npts);rtxvaluesFmnSP=rep(NA,times=npts)
 for (ww in 1:npts) {
  if (plotprobs) {tmp=ifelse(xxCF[ww,1:nCF]>thresh,1,0)} else {tmp=xxCF[ww,1:nCF]*multiplierx}
  rtxvaluesCFmnSP[ww]=mean(tmp,na.rm=TRUE)	#Again, ignoring the way I should probably use a logit transformation when taking the mean of  probabilities
  if (plotprobs) {tmp=ifelse(xxF[ww,1:nF]>thresh,1,0)} else {tmp=xxF[ww,1:nF]*multiplierx}
  rtxvaluesFmnSP[ww]=mean(tmp,na.rm=TRUE)
 }
 rtxvaluesCFmnSP[!is.finite(rtxvaluesCFmnSP)]=NA;rtxvaluesFmnSP[!is.finite(rtxvaluesFmnSP)]=NA
 rtp=rtspatial(rtxvaluesCFmnSP,rtxvaluesFmnSP,rttspatialx,plotprobs,xtext,"",axisunitsx,multiplierx,accuracy,choice6,choice8x,plotpertxt[plotpervar])
}

#REGAIN INDENT
}
