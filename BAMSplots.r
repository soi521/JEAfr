
# I haven't checked (Sep 2017) but it's possible these zeroes I had to put in at '';'; place a dot at lat=lon=0 on any map that includes that point. However, if I remove them (replace with c()s) then I get packet errors on each panel.

pnl=function(x,y,z,...) {
 panel.levelplot(x,y,z,...)
 if (domarmap) {sp.polygons(atldf,fill="darkslategray1",add=TRUE,col=NA)}	#This is a different way of doing it (not using the contour lines trick I used before), but outside a panel function I need something like plot(atl,lwd=3,deep=min(atl),shallow=-20,step=1,drawlabel=FALSE,col="darkslategray1",add=TRUE)

 source("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/specifyareas.r")
 gcellsall=SpatialPolygons(list(Polygons(list(Polygon(cbind(c(lonmins,lonmaxs,lonmaxs,lonmins,lonmins),c(latmins,latmins,latmaxs,latmaxs,latmins)))),"rect1")))
 for (regs in 1:length(areanames)) {
  gcell=SpatialPolygons(list(Polygons(list(Polygon(cbind(c(lonmins[regs],lonmaxs[regs],lonmaxs[regs],lonmins[regs],lonmins[regs]),c(latmins[regs],latmins[regs],latmaxs[regs],latmaxs[regs],latmins[regs])))),"rect1")))
  if (regs==1) {outline=gcell} else {outline=gUnion(outline,gcell,byid=FALSE)}
  if (showallcells) {sp.polygons(gcell,col="burlywood",fill="transparent",lwd=1)}
 }
# outline=gConvexHull(gcellsall,byid=FALSE) is an alternative
 sp.polygons(outline,col="burlywood",fill="transparent",lwd=2)

 if (plotcountries) {
  panel.lines(frgmap,col="black",lwd=1)	#More or less equivalent to map(database="worldHires",xlim=c(lonmin,lonmax),ylim=c(latmin,latmax),fill=FALSE,add=TRUE) outside a panel function
#I got the following from http://blog.revolutionanalytics.com/2009/10/geographic-maps-in-r.html for getting the borders of South Sudan and Eritrea (which are not yet included in frgmap for some reason despite the recent update of mapdata).
  sp.polygons(gadmSSD,fill="transparent",col="black",lwd=1)
  sp.polygons(gadmERI,fill="transparent",col="black",lwd=1)
  sp.polygons(gadmYEM,fill="grey",col="black",lwd=1)	#map(database="worldHires",regions=c("Yemen","Oman","Saudi Arabia"),fill=TRUE,col="grey",add=TRUE) outside a panel function
  sp.polygons(gadmOMN,fill="grey",col="black",lwd=1)
  sp.polygons(gadmSAU,fill="grey",col="black",lwd=1)
  sp.polygons(lakepolygons,fill="darkslategray1",col="darkslategray1",add=TRUE)	#col is the outline of the lake, e.g. you could highlight them more clearly using col="light blue"
 }

#Broken lines taken from coordinates in D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/specifyareas.r
 if (plotbrokenlines) {
#Within Kenya/Uganda:
  panel.segments(y0=1.08136,x0=34.52297,y1=-1.61551,x1=41.59816,lwd=1.25,lty=2)
  panel.segments(y0=-0.12401,x0=37.5,y1=-10,x1=37.5,lwd=1.25,lty=2)
  panel.segments(y0=4.48260,x0=36.12697,y1=-0.12401,x1=37.5,lwd=1.25,lty=2)	#From latcellcentre=((-3.355068717*loncellcentre)+125.6910669)

#Within Ethiopia:
  panel.segments(y0=4,x0=40.91474,y1=11.57081,x1=42.78004571,lwd=1.25,lty=2)	#From latcellcentre=(4.058757*loncellcentre-162.063)
  panel.segments(y0=4.48260,x0=36.12697,y1=11.48469,x1=41.77395,lwd=1.25,lty=2)

#Within Somalia:
  panel.segments(y0=7.2,x0=47.2,y1=7.2,x1=51.5,lwd=1.25,lty=2)
  panel.segments(y0=4.04985,x0=42.01564,y1=4.04985,x1=50,lwd=1.25,lty=2)

#Within Uganda/Tanzania:
  panel.segments(y0=1.659394,x0=32.6,y1=-10,x1=32.6,lwd=1.25,lty=2)

  panel.text(y=9.94649,x=48.61843,"SP",col="grey4")
  panel.text(y=5.63712,x=46.86062,"SN",col="grey4")
  panel.text(y=2.08621,x=42.42214,"SS",col="grey4")
  panel.text(y=6.01184,x=35.65749,"IL",col="grey4")
  panel.text(y=5.11210,x=38.64284,"EO",col="grey4")
  panel.text(y=6.81663,x=44.04812,"ES",col="grey4")
  panel.text(y=-1.78023,x=30.05983,"RW",col="grey4")
  panel.text(y=0.93335,x=31.44343,"UW",col="grey4")
  panel.text(y=2.03193,x=33.85280,"UE",col="grey4")
  panel.text(y=2.57945,x=35.78371,"KT",col="grey4")
  panel.text(y=1.56921,x=38.92312,"KN",col="grey4")
  panel.text(y=-0.66821,x=36.06374,"KW",col="grey4")
  panel.text(y=-2.02677,x=38.82937,"KC",col="grey4")
  panel.text(y=-1.95592,x=31.74343,"TW",col="grey4")
  panel.text(y=-2.61457,x=35.03933,"TM",col="grey4")
 }

}
stfn1=function(which.given,which.panel,var.name,par.strip.text,...) {
 if (which.given==1) {strip.default(which.given=1,which.panel=which.panel[1],var.name=var.name[1],par.strip.text=list(cex=0.75),...)}
}
stfn2=function(which.given,which.panel,var.name,par.strip.text,...) {
 if (which.given==2) {strip.default(which.given=1,which.panel=which.panel[2],var.name=var.name[2],par.strip.text=list(cex=0.75),...)}
}




#I need this for ggplot2's insistence on data frames without NAs in them:
remNAs=function(dfin,dfout) {

 dfout=c()
 for (rr in 1:nrow(dfin)) {
  tmp=suppressWarnings(as.numeric(as.character(dfin[rr,])))	#The as.numeric() produces "Warning message: In as.numeric(as.character(dfin[rr, ])) : NAs introduced by coercion" but can ignore this
  if (is.finite(sum(tmp,na.rm=FALSE))) {dfout=rbind(dfout,dfin[rr,])}
 }

return(dfout)
}





#Return time plots as the Climate Lab do them (as close as I could get anyway)
rtplot=function(CFmn,CFsd,Fmn,Fsd,rtts,vartxt,varlvltxt,placetxt,axisunits,multiplier,accuracy,choice6,choice8,rtimexaxis,seasontxt,printts) {

#n.b. CFmn, CFsd, Fmn and Fsd are return times (reciprocals of averaged probabilities; length nthr); rtts contains thresholds and has units (also length nthr)
imposedmaxrt=100

CFmn[!is.finite(CFmn)]=1e30;Fmn[!is.finite(Fmn)]=1e30
if (imposedmaxrt>0) {xmax=imposedmaxrt} else {xmax=max(c(CFmn+CFsd,Fmn+Fsd),na.rm=TRUE)}
ymin=min(rtts,na.rm=TRUE)*multiplier
ymax=max(rtts,na.rm=TRUE)*multiplier*1.10
if (choice8==41) {ymin=0;ymax=100}
if (choice8==48) {ymin=-100;ymax=0}
if (choice8==49 || choice8==51) {ymin=-1.0*(365.25/12);ymax=1.0*(365.25/12)}
if (choice8==50) {ymin=0;ymax=14}
if (choice8==10) {ymin=0;ymax=145}
if (choice8==52) {ymin=0;ymax=110}
if (choice8==54) {ymin=-60;ymax=0}
maintxt=paste(vartxt," in ",axisunits,placetxt,sep="")
barw=(max(rtts)-min(rtts))*multiplier/(accuracy*5)
if (rtimexaxis) {
 xtxt="Return time (yrs)\n(i.e. ensemble sets with low {high} return time in any horizontal slice are more likely (less likely) to exceed the threshold)"
 if (choice6==1) {
  dev.new();plot(x=1,y=mean(rtts)*multiplier,las=1,log="xy",bty="l",type="n",xlab=xtxt,ylab=paste(seasontxt," ",vartxt,varlvltxt," (",axisunits,")",sep=""),main=maintxt,xlim=c(1,xmax),ylim=c(ymin,ymax))
 } else {
  if (is.finite(ymax)) {
   dev.new();plot(x=1,y=mean(rtts)*multiplier,las=1,log="x",bty="l",type="n",xlab=xtxt,ylab=paste(seasontxt," ",vartxt,varlvltxt," (",axisunits,")",sep=""),main=maintxt,xlim=c(1,xmax),ylim=c(ymin,ymax))
  } else {
   dev.new();plot(x=1,y=mean(rtts)*multiplier,las=1,log="x",bty="l",type="n",xlab=xtxt,ylab=paste(seasontxt," ",vartxt,varlvltxt," (",axisunits,")",sep=""),main=maintxt,xlim=c(1,xmax))
  }
 }
}
printtsflag=rep(FALSE,times=length(printts))
jitterup=(rtts[3]-rtts[2])*multiplier*0.25	#Amt to jitter I can estimate from separation of the thresholds
for (thr1 in 1:nthr) {
 thresh=rtts[thr1]*multiplier	#Above this level is taken to be an 'extreme event' (it's clear from the way the thresholds are on the y-axis that there has to be a loop like this and each point must be an ENSEMBLE not a PIXEL because if each pixel were plotted separately for e.g. runoff then because there will always be very dry and very wet pixels there would not be a curve of the shape shown but a triangle with wetland pixels top left)
 upper=CFmn[thr1]+CFsd[thr1];lower=CFmn[thr1]-CFsd[thr1]
 if (is.finite(lower) || is.finite(upper)) {
  if (!is.finite(lower)) {lower=1e-6}
  if (lower<1e-6) {lower=1e-6}
  if (!is.finite(upper)) {upper=1e6}
  if (upper<1e-6) {upper=1e-6}
 }
# cat("thresh=",thr1," - CF: lower=",lower,", mean=",CFmn[thr1],", upper=",upper,"\n")
 segments(upper,thresh,lower,thresh,col="blue")
 segments(upper,thresh-barw,upper,thresh+barw,col="blue")
 segments(lower,thresh-barw,lower,thresh+barw,col="blue")
 upper=Fmn[thr1]+Fsd[thr1];lower=Fmn[thr1]-Fsd[thr1]
 if (is.finite(lower) || is.finite(upper)) {
  if (!is.finite(lower)) {lower=1e-6}
  if (lower<1e-6) {lower=1e-6}
  if (!is.finite(upper)) {upper=1e6}
  if (upper<1e-6) {upper=1e-6}
 }
# cat("thresh=",thr1," - F: lower=",lower,", mean=",Fmn[thr1],", upper=",upper,"\n")
 segments(upper,thresh+jitterup,lower,thresh+jitterup,col="brown")
 segments(upper,thresh-barw+jitterup,upper,thresh+barw+jitterup,col="brown")
 segments(lower,thresh-barw+jitterup,lower,thresh+barw+jitterup,col="brown")
 points(x=CFmn[thr1],y=thresh,col="blue",pch=16,cex=0.6)
 points(x=Fmn[thr1],y=thresh+jitterup,col="brown",pch=16,cex=0.6)
#Print the numbers from the return time plot
 if (choice8==54) {	#I only need these numbers if am plotting inverted ET (and for any other variable would have to change printts anyway)
  for (printlp in 1:length(printts)) {
   if (thresh>printts[printlp] && !printtsflag[printlp]) {cat("Return times for threshold ",printts[printlp],axisunits," on ",vartxt,placetxt,": ",CFmn[thr1],"yrs -> ",Fmn[thr1],"yrs (CF->F)\n");printtsflag[printlp]=TRUE}
  }
 }
}
cat("\n")

return(0)
}































#Toby's spatial plots (this is where I departed from the Climate Lab's way of doing things: this is more or less doing an inverted version of Pall et al. (2011:Fig.3), plotting pixels for points rather than ensemble members)
rtspatial=function(zCF,zF,rttspatial,plotprobs,vartxt,varlvltxt,axisunits,multiplier,accuracy,choice6,choice8,seasontxt) {

#n.b. zCF and zF can be real values or probabilities: if real values then units are the same as rtts
npts=length(zCF)

#Imagine you have an area A. Drought occurs across 40% of A in the CF, but in 50% of A in the F. Therefore probability of occurrence has moved, as a result of human-induced atmospheric gas emissions from 40% to 50%. Therefore the drought return time has moved, as a result of human-induced atmospheric gas emissions from (1/0.40 =) 2.5 yrs to 2 yrs, i.e. droughts are getting more frequent.
#The problem I've always had about this reasoning is that it confounds spread with frequency: what if drought is becoming more widespread but not more frequent?
#The only solution I can see is to specify that A is a small enough area that it has an approximately uniform climate. That means drought by definition can't spread sub-grid.
#I'm happy with that logic, but in the Climate Lab I feel they do calculate return time plots for areas that are too large like all of Africa and I just cannot see how this can be legitimate.
#I've set it up here so that with rtimexaxis=TRUE you have time on the x axis (as do all the examples I've seen from the Climate Lab) or if area A is larger than one homogeneous climate zone then I can avoid the interpretation of spread as frequency and just plot time on x.

if (imposedmaxrt>0) {xmax=imposedmaxrt} else {xmax=1/min(c(zCF,zF),na.rm=TRUE)}
ymin=rttspatial*multiplier
maintxt=paste(vartxt," in ",axisunits,sep="")
if (plotprobs) {
 minval=0;maxval=max(c(zCF,zF),na.rm=TRUE)
 if (maxval>0.75) {maxval=1}	#If probabilities go up to 0.75 may as well put whole scale 0-1 on y axis
 txt7CF=paste("Prob(",seasontxt," ",vartxt,varlvltxt,">",rttspatial*multiplier," ",axisunits," in CF)",sep="")
 txt7F=paste("Prob(",seasontxt," ",vartxt,varlvltxt,">",rttspatial*multiplier," ",axisunits," in F)",sep="")
} else {
 minval=min(c(zCF,zF),na.rm=TRUE);maxval=max(c(zCF,zF),na.rm=TRUE)
 txt7CF=paste(seasontxt," ",vartxt,varlvltxt," in CF",sep="")
 txt7F=paste(seasontxt," ",vartxt,varlvltxt," in F",sep="")
}
if (cropplots) {
 for (ww in 1:npts) {
  if (Seasonality[ww,1]!=1) {zCF[ww]=NA;zF[ww]=NA}
 }
}

dev.new();print(levelplot(zCF~Longitude*Latitude,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=colorRampPalette((c("white","blue","blue4","darkblue"))),panel=pnl,main=txt7CF,sub=paste("(units=",axisunits,"; Counterfactual ensemble mean)",sep=""),asp=1))
dev.new();print(levelplot(zF~Longitude*Latitude,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=colorRampPalette((c("white","brown","chocolate4","saddlebrown"))),panel=pnl,main=txt7F,sub=paste("(units=",axisunits,"; Factual ensemble mean)",sep=""),asp=1))
diff2=zF-zCF
diff2[!is.finite(diff2)]=NA
maxvaldiff=max(abs(diff2),na.rm=TRUE)
if (plotprobs) {maxvaldiff=0.2}	#Impose uniformity on these plots for the paper
dev.new();print(levelplot(diff2~Longitude*Latitude,at=seq(from=-maxvaldiff,to=maxvaldiff,length.out=ncols),col.regions=colorRampPalette((c("darkblue","blue4","blue","white","white","brown","chocolate4","saddlebrown"))),panel=pnl,main=paste(txt7F," - ",txt7CF,sep=""),sub=paste("(units=",axisunits,"; this is Factual ensemble mean minus Counterfactual ensemble mean)",sep=""),asp=1))

return(0)
}












#Toby's arrow plots (my 'state space plots')
arrowplot=function(ssdf,rownummap,xxCF,xxF,yyCF,yyF,Longitude,Latitude,tamdf,choice8x,choice8y,rttspatialx,rttspatialy,littlepoints,littlearrows,bigarrows,showhull,ggplotworks,opt1,multiplierx,multipliery,plotprobs) {

library(sp);library(shape);library(rgeos);library(ggplot2);library(grid)

showhull=TRUE	#This means I'll try to show some kind of convex hull
ggplotworks=FALSE	#Turns off the old way I tried to do the convex hull
newhull=TRUE	#Turns on a new way of doing it
if (newhull) {source("geom_bag.r")}

littlepoints=TRUE;littlearrows=TRUE	#If I do both big and small arrows, then it needs to be explained in an inset in the paper, which is what insetplot=TRUE is for
bigarrows=TRUE
insetplot=TRUE
opt1=3	#=1 makes the arrow an average of the cartesian coords of the individual arrows, =2 as 1 but averaging the polar coordinates, =3 means the arrow instead connects the centroid of the start points to the centroid of the end points

lrtTAM=c()
for (indx in 1:nrow(ssdf)) {
 ssettam=subset(tamdf,lonsTAMvec>=lonmins[indx] & lonsTAMvec<=lonmaxs[indx] & latsTAMvec>=latmins[indx] & latsTAMvec<=latmaxs[indx])
 lrtTAM=c(lrtTAM,mean(ssettam$LongRainsTAMvec,na.rm=TRUE))	#Precip is the only climate variable for which I've got long-term data (TAMSAT): all others only have the CF and F model data so can't plot long-term mean
}

#This loop works region by region and averages across each region first, and then the ensemble
areanamesu=unique(ssdf$areanames)
arstxCF=matrix(NA,nrow=length(areanamesu),ncol=nCF);arstyCF=matrix(NA,nrow=length(areanamesu),ncol=nCF)
arendxF=matrix(NA,nrow=length(areanamesu),ncol=nF);arendyF=matrix(NA,nrow=length(areanamesu),ncol=nF)
lrtTAM1=rep(NA,times=length(areanamesu))
for (uu in 1:length(areanamesu)) {
 if (areanamesu[uu]!="TC") {
  placetxt=paste(" for ",areanamesu[uu],sep="")
  rownumvec=rownummap[uu,][is.finite(rownummap[uu,])]
  pix4region=(ssdf$areanames==areanamesu[uu])
  lrtTAM1[uu]=mean(lrtTAM[pix4region],na.rm=TRUE)	#lrtTAM has length 2040 and ssdf has only 655 rows, so this picks out the pixels in lrtTAM corresponding to area uu.
  for (ensmem in 1:nCF) {
   if (plotprobs) {tmp=ifelse(xxCF[rownumvec,ensmem]>rttspatialx,1,0)} else {tmp=xxCF[rownumvec,ensmem]*multiplierx}
   arstxCF[uu,ensmem]=mean(tmp,na.rm=TRUE)	#In the case of plotprobs=TRUE, I'm ignoring the way I should probably use a logit transformation
   if (plotprobs) {tmp=ifelse(yyCF[rownumvec,ensmem]>rttspatialy,1,0)} else {tmp=yyCF[rownumvec,ensmem]*multipliery}
   arstyCF[uu,ensmem]=mean(tmp,na.rm=TRUE)
  }
  for (ensmem in 1:nF) {
   if (plotprobs) {tmp=ifelse(xxF[rownumvec,ensmem]>rttspatialx,1,0)} else {tmp=xxF[rownumvec,ensmem]*multiplierx}
   arendxF[uu,ensmem]=mean(tmp,na.rm=TRUE)
   if (plotprobs) {tmp=ifelse(yyF[rownumvec,ensmem]>rttspatialy,1,0)} else {tmp=yyF[rownumvec,ensmem]*multipliery}
   arendyF[uu,ensmem]=mean(tmp,na.rm=TRUE)
  }
  arstxCF[!is.finite(arstxCF)]=NA;arstyCF[!is.finite(arstyCF)]=NA
  arendxF[!is.finite(arendxF)]=NA;arendyF[!is.finite(arendyF)]=NA
 }
}

arrowplot=1;if ((choice8x==10 || choice8x==44 || choice8x==46 || choice8x==47) && (choice8y==10 || choice8y==44 || choice8y==46 || choice8y==47)) {arrowplot=1:2}

if (choice8x==5 && min(arstxCF,na.rm=TRUE)>220 && min(arstxCF,na.rm=TRUE)>220) {arendxF=arendxF-273.15;arstxCF=arstxCF-273.15}	#temperature on x
minx=0#min(c(arstxCF,arendxF),na.rm=TRUE)
maxx=max(c(arstxCF,arendxF),na.rm=TRUE)
miny=0#min(c(arstyCF,arendyF),na.rm=TRUE)
maxy=max(c(arstyCF,arendyF),na.rm=TRUE)

#if (choice8x==5) {minx=17;maxx=31}	#Temp on x
#if (choice8x==6) {minx=8;maxx=17}	#Q on x

if (choice8x==10) {minx=min(c(arstxCF,arendxF,lrtTAM1*multiplierx),na.rm=TRUE);maxx=max(c(arstxCF,arendxF,lrtTAM1*multiplierx),na.rm=TRUE)}	#Precip on x
if (choice8x==10) {minx=0;maxx=145}	#Precip on x
if (choice8x==10 && littlearrows) {maxx=200}

if (choice8x==20) {maxx=1}
if (choice8x==41 || choice8x==42 || choice8x==43) {minx=0;maxx=80}	#sthu/sthf/sthzw on x
if (choice8x==44) {minx=-0.0012;maxx=0}	#-Precip on x
if (choice8x==46 || choice8x==47) {	#SPI or -SPI on x (long term mean is by definition zero)
 minx=min(c(arstxCF,arendxF,0),na.rm=TRUE);maxx=max(c(arstxCF,arendxF,0),na.rm=TRUE)
 minx=-1.0;maxx=2.0
 if (choice8x==46) {minx=-1.25;maxx=2}
}
if (choice8x==48) {minx=-100;maxx=0}	#-sthu on x
if (choice8x==52) {minx=0;maxx=110}	#ET on x
if (choice8x==53) {minx=-20;maxx=20}	#Storage on x
if (choice8x==54) {minx=-110;maxx=0.0}	#-ET on x

#if (choice8y==5) {miny=17;maxy=3}	#Temp on y
#if (choice8y==6) {miny=8;maxy=17}	#Q on y

if (choice8y==10) {miny=min(c(arstyCF,arendyF,lrtTAM1*multipliery),na.rm=TRUE);maxy=max(c(arstyCF,arendyF,lrtTAM1*multipliery),na.rm=TRUE)}	#Precip on y
if (choice8y==10) {miny=0;maxy=145}	#Precip on y

if (choice8y==41 || choice8y==42 || choice8y==43) {miny=0;maxy=80}	#sthu/sthf/sthzw on y
if (choice8y==44) {miny=min(c(arstyCF,arendyF,-lrtTAM1*multipliery),na.rm=TRUE);maxy=max(c(arstyCF,arendyF,-lrtTAM1*multipliery),na.rm=TRUE)}	#-Precip on y
#if (choice8y==44) {miny=0.4}
if (choice8y==45) {miny=-0.3;maxy=0}	#-sthu on y
if (choice8y==46 || choice8y==47) {	#SPI or -SPI on y (long term mean is by definition zero)
 miny=min(c(arstyCF,arendyF,0),na.rm=TRUE);maxy=max(c(arstyCF,arendyF,0),na.rm=TRUE)
 miny=-1.0;maxy=2.0
 if (choice8y==46) {miny=-1.25;maxy=0.25}
}
if (choice8y==48) {miny=-100;maxy=0}	#-sthu on y
if (choice8y==52) {miny=0;maxy=110}	#ET on y
if (choice8y==52 && littlearrows) {miny=0.61}
if (choice8y==53) {miny=-20;maxy=20}	#Storage on y
if (choice8y==54) {miny=-110;maxy=0.0}	#-ET on y
if (miny>maxy) {tmp=maxy;maxy=miny;miny=tmp}
if (minx>maxx) {tmp=maxx;maxx=minx;minx=tmp}
if (plotprobs) {minx=-0.1;maxx=1.1;miny=-0.1;maxy=1.1}

rtxvaluesCFmn=rep(NA,times=length(areanamesu));rtxvaluesCFsd=rep(NA,times=length(areanamesu))
rtxvaluesFmn=rep(NA,times=length(areanamesu));rtxvaluesFsd=rep(NA,times=length(areanamesu))
rtyvaluesCFmn=rep(NA,times=length(areanamesu));rtyvaluesCFsd=rep(NA,times=length(areanamesu))
rtyvaluesFmn=rep(NA,times=length(areanamesu));rtyvaluesFsd=rep(NA,times=length(areanamesu))
arstx=rep(NA,times=length(areanamesu));arsty=rep(NA,times=length(areanamesu))
arendx=rep(NA,times=length(areanamesu));arendy=rep(NA,times=length(areanamesu))
textx=rep("",times=length(areanamesu));texty=rep("",times=length(areanamesu))
for (uu in 1:length(areanamesu)) {	#Ensemble averaging
 if (areanamesu[uu]!="TC") {
  if (plotprobs) {
   rtxvaluesCFmn[uu]=mean(arstxCF[uu,],na.rm=TRUE);rtxvaluesCFsd[uu]=sd(arstxCF[uu,],na.rm=TRUE)
   rtxvaluesFmn[uu]=mean(arendxF[uu,],na.rm=TRUE);rtxvaluesFsd[uu]=sd(arendxF[uu,],na.rm=TRUE)
   rtyvaluesCFmn[uu]=mean(arstyCF[uu,],na.rm=TRUE);rtyvaluesCFsd[uu]=sd(arstyCF[uu,],na.rm=TRUE)
   rtyvaluesFmn[uu]=mean(arendyF[uu,],na.rm=TRUE);rtyvaluesFsd[uu]=sd(arendyF[uu,],na.rm=TRUE)
   arstx[uu]=rtxvaluesCFmn[uu];arsty[uu]=rtyvaluesCFmn[uu]
   arendx[uu]=rtxvaluesFmn[uu];arendy[uu]=rtyvaluesFmn[uu]
   if (opt1!=1) {
    rmod=sqrt(((arendxF[uu,]-arstxCF[uu,])^2)+((arendyF[uu,]-arstyCF[uu,])^2))
    alpha=atan((arendyF[uu,]-arstyCF[uu,])/(arendxF[uu,]-arstxCF[uu,]))*180/3.141592658
    arendx[uu]=arstx[uu]+mean(rmod,na.rm=TRUE)*cos(mean(alpha,na.rm=TRUE));arendy[uu]=arsty[uu]+mean(rmod,na.rm=TRUE)*sin(mean(alpha,na.rm=TRUE))
   }
   textx[uu]=mean(c(arstx[uu],arendx[uu]),na.rm=TRUE);texty[uu]=mean(c(arsty[uu],arendy[uu]),na.rm=TRUE)
  } else {
   rtxvaluesCFmn[uu]=mean(arstxCF[uu,],na.rm=TRUE)/multiplierx;rtxvaluesCFsd[uu]=sd(arstxCF[uu,],na.rm=TRUE)/multiplierx
   rtxvaluesFmn[uu]=mean(arendxF[uu,],na.rm=TRUE)/multiplierx;rtxvaluesFsd[uu]=sd(arendxF[uu,],na.rm=TRUE)/multiplierx
   rtyvaluesCFmn[uu]=mean(arstyCF[uu,],na.rm=TRUE)/multipliery;rtyvaluesCFsd[uu]=sd(arstyCF[uu,],na.rm=TRUE)/multipliery
   rtyvaluesFmn[uu]=mean(arendyF[uu,],na.rm=TRUE)/multipliery;rtyvaluesFsd[uu]=sd(arendyF[uu,],na.rm=TRUE)/multipliery
   arstx[uu]=rtxvaluesCFmn[uu]*multiplierx;arsty[uu]=rtyvaluesCFmn[uu]*multipliery
   arendx[uu]=rtxvaluesFmn[uu]*multiplierx;arendy[uu]=rtyvaluesFmn[uu]*multipliery
   if (opt1!=1) {
    rmod=sqrt(((arendxF[uu,]-arstxCF[uu,])^2)+((arendyF[uu,]-arstyCF[uu,])^2))
    alpha=atan((arendyF[uu,]-arstyCF[uu,])/(arendxF[uu,]-arstxCF[uu,]))*180/3.141592658
    arendx[uu]=arstx[uu]+mean(rmod,na.rm=TRUE)*cos(mean(alpha,na.rm=TRUE));arendy[uu]=arsty[uu]+mean(rmod,na.rm=TRUE)*sin(mean(alpha,na.rm=TRUE))
   }
   textx[uu]=mean(c(arstx[uu],arendx[uu]),na.rm=TRUE)*multiplierx;texty[uu]=mean(c(arsty[uu],arendy[uu]),na.rm=TRUE)*multipliery
  }
 }
}

colcpal2=rep(NA,times=length(areanamesu))
for (arrloop in arrowplot) {
 if (arrloop==1) {arrowstxt="Arrows: Counterfactual ensemble -> Factual ensemble"}	#Formerly: "(paleblue,navy,cyan = MM in long rains Mar-May\npalegreen,green,olive = MM in short rains Oct-Dec)"
 if (arrloop==2) {arrowstxt="Arrows: TAMSAT -> Counterfactual ensemble (blue) and TAMSAT -> Factual ensemble (brown)"}
# plot(x=0,y=0,xlim=c(minx,maxx),ylim=c(miny,maxy),type="n",main=arrowstxt,xlab=xtxt,ylab=ytxt)

 xtxt=ifelse(plotprobs,paste("Prob(",xtext," > threshold)",sep=""),ifelse(choice8x==5,paste(xtext," (°C)",sep=""),paste(xtext," (",axisunitsx,")",sep="")))
 ytxt=ifelse(plotprobs,paste("Prob(",ytext," > threshold)",sep=""),paste(ytext," (",axisunitsy,")",sep=""))
 if (arrloop==2 && ggplotworks) {
  for (uu in 1:length(areanamesu)) {
   if (areanamesu[uu]!="TC") {
    arrowstartx=NA;arrowstarty=NA
    if (choice8x==10) {arrowstartx=lrtTAM1[uu]}	#Precip on x
    if (choice8x==44) {arrowstartx=-lrtTAM1[uu]}	#-Precip on x
    if (choice8x==46 || choice8x==47) {arrowstartx=0}	#SPI or -SPI on x (long term mean is by definition zero)
    if (choice8y==10) {arrowstarty=lrtTAM1[uu]}	#Precip on y
    if (choice8y==44) {arrowstarty=-lrtTAM1[uu]}	#-Precip on y
    if (choice8y==46 || choice8y==47) {arrowstarty=0}	#SPI or -SPI on y (long term mean is by definition zero)
   }
  }
 }

 if (arrloop==1 && bigarrows) {
  for (uu in 1:length(areanamesu)) {
   if (areanamesu[uu]!="TC") {
    pix4region=(ssdf$areanames==areanamesu[uu])
    colcpal2[uu]=as.character(ssdf$cpal2[pix4region][1])
    spcloud=FALSE;mat1=cbind(arstxCF[uu,],arstyCF[uu,])
    #The chull command can't handle NA coords so the next two lines replace them with mean of the other legitimate points:
    mnx=mean(mat1[,1],na.rm=TRUE);mat1[!is.finite(mat1[,1]),1]=mnx
    mny=mean(mat1[,2],na.rm=TRUE);mat1[!is.finite(mat1[,2]),2]=mny
    spcloud=TRUE
    if (littlepoints && ggplotworks) {points(mat1,col=colcpal2[uu],pty=1,cex=0.5)}
    if (showhull && ggplotworks) {
     hpts=chull(mat1)
     hpts=c(hpts,hpts[1])
   lines(mat1[hpts,],col=colcpal2[uu],lty=2)
    }
    sp=SpatialPoints(t(mat1))
    spdfst=SpatialPointsDataFrame(mat1,as.data.frame(rep(1,times=nrow(mat1))))	#Just put 1's in the column of the data frame for now
    centroidst=gCentroid(spdfst)
#    points(centroidst,pch=4,col=colcpal2[uu])
#Cloud of end points
    epcloud=FALSE;mat1=cbind(arendxF[uu,],arendyF[uu,])
    mnx=mean(mat1[,1],na.rm=TRUE);mat1[!is.finite(mat1[,1]),1]=mnx
    mny=mean(mat1[,2],na.rm=TRUE);mat1[!is.finite(mat1[,2]),2]=mny
    epcloud=TRUE
    if (littlepoints && ggplotworks) {points(mat1,col=colcpal2[uu],pty=1,cex=0.5)}
    if (showhull && ggplotworks) {
     hpts=chull(mat1)
     hpts=c(hpts,hpts[1])
     lines(mat1[hpts,],col=colcpal2[uu],lty=2)
    }
    sp=SpatialPoints(t(mat1))
    spdfend=SpatialPointsDataFrame(mat1,as.data.frame(rep(1,times=nrow(mat1))))	#Just put 1's in the column of the data frame for now
    centroidend=gCentroid(spdfend)
#    points(centroidend,pch=4,col=colcpal2[uu])
    if (opt1==3) {
     arstx[uu]=ifelse(spcloud,centroidst$x,NA)
     arsty[uu]=ifelse(spcloud,centroidst$y,NA)
     arendx[uu]=ifelse(epcloud,centroidend$x,NA)
     arendy[uu]=ifelse(epcloud,centroidend$y,NA)
     textx[uu]=ifelse(spcloud && epcloud,mean(c(centroidst$x,centroidend$x),na.rm=TRUE),NA)
     texty[uu]=ifelse(spcloud && epcloud,mean(c(centroidst$y,centroidend$y),na.rm=TRUE),NA)
#     rmod=ifelse(spcloud && epcloud,sqrt(((centroidend$x-centroidst$x)^2)+((centroidend$y-centroidst$y)^2)),NA)
#     Arrows(arstx[uu],arsty[uu],arendx[uu],arendy[uu],col=colcpal2[uu],arr.length=rmod,lwd=2,arr.type="triangle",arr.adj=1)
#Check arrow using points(centroidst,pch=4,col="red");points(centroidend,pch=4,col="brown")
    } else {
     if (ggplotworks) {
      rmod=sqrt(((arendx[uu]-arstx[uu])^2)+((arendy[uu]-arsty[uu])^2))
      alpha=atan((arendy[uu]-arsty[uu])/(arendx[uu]-arstx[uu]))*180/3.141592658
      Arrowhead(x0=arstx[uu],y0=arsty[uu],angle=alpha,arr.length=rmod,arr.type="triangle",lcol=colcpal2[uu],arr.col=NA)	#lcol=as.character(tmp$cpal1[tmp$MMs[1]]) instead gives shading by MM
     }
    }
#    text(x=textx[uu],y=texty[uu],tmp$areanames[1],col=colcpal2[uu],cex=0.7,pos=3,offset=0.5)
   }
  }
 }
 cbbPalette=c("#CC0000","#000099","#000000","#CCCCCC","#E69F00","#56B4E9","#00FF00","#009E73","#993300","#F0E442","#0072B2","#9900CC","#D55E00","#CC79A7","#00CCFF")	#Not the same as colcpal2
 cbbPalette=c(rep("#CC0000",times=7),rep("#000099",times=8))
 offset=c(1,1,1,1,1,-1,1,-1,1,1,NA,1,0,-1,1.3,-1);newy=arsty+(0.035*maxy*offset)	#Shift y coords to avoid other labels (except TC)
 offset=c(0,0,0,0,0,0,0,0,0,0,NA,0,-0.5,0,-0.2,0);newx=arstx+(0.035*maxx*offset)	#Shift x coords to avoid other labels (except TC)
 arrheadfactor=12/mean(c(maxx-minx,maxy-miny))
 arrheadsize=pmin(0.7,pmax(0.2,sqrt(((arendx-arstx)^2)+((arendy-arsty)^2))*arrheadfactor))

 plotregions=c("EO")
# plotregions=c("KW")
 if (insetplot) {	#Inset arrow plot. What I did was generate one for EO and one for KW, copy into Paint, cut off the legend and the title and copy into Word.
  littlearrows=TRUE;showhull=TRUE
  dev.new(width=400,height=400,res=10)
  source("arrowhull.r",local=TRUE)
  print(pp)
 }

 littlearrows=FALSE;showhull=TRUE
 plotregions=areanamesu
 dev.new(width=1000,height=400,res=10)	#I'm using arrow heads and these are plotted so as to look right at a particular screensize, which means if I resize the plot window after plotting they will go out of shape, so I need to specify the right aspect ratio here.
 source("arrowhull.r",local=TRUE)
 print(pp)
}

return(0)
}

cat("\nDefined: pnl, rtplot, rtspatial and arrowplot\n")
