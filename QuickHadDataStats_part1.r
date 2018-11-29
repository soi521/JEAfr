
#Note:
	#LongRains is at HadAM3P resolution and calculated from the HadAM3P files (and stacked: npoints rows and nyrs columns)
	#LongRainsTAM is at TAMSAT resolution and calculated from the TAMSAT files (not stacked: lat/lon format; units are precipitation in mm per 3 month period) and
	#LongRainsHAD is last timestep of LongRains resampled to TAMSAT resolution (not stacked: lat/lon as LongRainsTAM is).

newmemlimit=8191		#The default memory limit is probably 5922Mb. My machine allows it to be changed up to any amount, though the man page says there's a limit of 8 Tb (=8192 Mb)
cat("Changing memory limit from",memory.limit(),"Mb to",newmemlimit,"Mb (only has an effect on Windows R, so if you are on Linux/UNIX just ignore the \"'memory.limit()' is Windows-specific\" warnings this generates).\n")
memory.limit(size=newmemlimit)

domarmap=TRUE	#Generally leave this =TRUE but if the NOAA server is down ("Error in getNOAA.bathy(lon1 = lonmin - 1, lon2 = lonmax + 1, lat1 = latmin -  :   The NOAA server cannot be reached") then these commands cause errors. Having =FALSE just means the sea is not plotted on any of the plots.
cat("\nBy the way, if you get any error like \"Error in UseMethod ... no applicable method for 'depth' ...\"\n\tthen it's probably NOT a real error: likely you just closed a plot window while it was plotting.\n\n")

library(ncdf4);library(lattice);library(mapdata);library(sp);library(rgeos)
source("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/BAMSplots.r")
source("D:/D.DRIVE/programs/myncutils.r")	#*************************** Check that flag1=1 in the definition of readinfromnc in here

colrs=c("purple","white","green")
#colrs=c("brown","firebrick","darkorange","red","orangered","orange","gold","yellow","beige","white","lightgreen","green","darkgreen","skyblue","blue","darkblue","royalblue")
cpal=colorRampPalette(colrs)

landmasknc="D:/D.DRIVE/MODELS/iofiles/DrivingData/ace_africa/hadam3p_afr_iaac_2012_1_009349385_0/iaacga/grid_file.JULES.nc"
lonmin=-180;lonmax=180;latmin=-90;latmax=90
lonmin=-25.0;lonmax=62.6;latmin=-14.3;latmax=46.0	#Africa from 14.3°S (Lilongwe, Malawi) to 46.0°N (Slovenia) and from 25.0°W (Cape Verde) to 62.6°E (Herat, Afghanistan)
choice1=3	#choice1=menu(c("Whole domain available in the input files (i.e. the LAM)","CDI (Lake Turkana, Juba River, Wajir, Imi)","GHoA (The Nile, Cape Guardafui, Mombasa, Khartoum)","2/3rds of the way from the GHoA to the CDI","1/3rd of the way from the GHoA to the CDI","NW Kenya","Ahmar, Ethiopia","Juba-Shabelle region of Somalia (inc. Mogadishu) (drought-affected area in 2012 http://www.somaliareport.com/index.php/writer/189/AAH_ )","Kenya-NE"))
if (choice1==2) {
 lonmin=36;lonmax=42.28
 latmin=1.74884;latmax=6.45774
}
if (choice1==3) {
 lonmin=29.4;lonmax=51.75
 latmin=-5;latmax=12.8
}
if (choice1==4) {
 lonmin=30+((36-30)*2/3);lonmax=51.25-((51.25-42.28)*2/3)
 latmin=-4.05+((1.74884-(-4.05))*2/3);latmax=15.63-((15.63-6.45774)*2/3)
}
if (choice1==5) {
 lonmin=30+((36-30)*1/3);lonmax=51.25-((51.25-42.28)*1/3)
 latmin=-4.05+((1.74884-(-4.05))*1/3);latmax=15.63-((15.63-6.45774)*1/3)
}
if (choice1==6) {
 lonmin=34.5;lonmax=37.5
 latmin=2;latmax=5
}
if (choice1==7) {1
 lonmin=38.5;lonmax=44
 latmin=7.5;latmax=10
}
if (choice1==8) {
 lonmin=42;lonmax=45.7
 latmin=0;latmax=5
}
if (choice1==9) {
 lonmin=36.9;lonmax=40.6
 latmin=1.2;latmax=3.05
}
lonmin=lonmin-0.01;latmin=latmin-0.01;lonmax=lonmax+0.01;latmax=latmax+0.01	#This makes sure I get the wider area if a bound lies exactly at the border between two pixels

if (domarmap) {
 library(marmap)
 library(inlmisc)	#Replaces library(Grid2Polygons)
 atl=getNOAA.bathy(lon1=lonmin-1,lon2=lonmax+1,lat1=latmin-1,lat2=latmax+1,resolution=10)	#I need bathymetry. This downloads it for the selected area plus 1 degree buffer to make the edges display correctly.
 tmp=as.SpatialGridDataFrame(atl)
 atldf=Grid2Polygons(tmp,level=TRUE,at=c(-20000,0))	#Pick out sea points only (-20000 to 0 m elevation)
}

choice2tmp=choice2;choice1tmp=choice1
tamsat=TRUE
if (choice1==2 && file.exists("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataSmallRegion.RData")) {tamsat=FALSE;load("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataSmallRegion.RData")}
if (choice1==3 && file.exists("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataLargeRegion.RData")) {tamsat=FALSE;load("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataLargeRegion.RData")}
if (choice1==6 && file.exists("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataNWKenyaRegion.RData")) {tamsat=FALSE;load("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataNWKenyaRegion.RData")}
if (choice1==7 && file.exists("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataAhmarRegion.RData")) {tamsat=FALSE;load("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataAhmarRegion.RData")}
if (choice1==8 && file.exists("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataJubaShabelleRegion.RData")) {tamsat=FALSE;load("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataJubaShabelleRegion.RData")}
if (choice1==9 && file.exists("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataNEKenyaRegion.RData")) {tamsat=FALSE;load("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataNEKenyaRegion.RData")}
if (!tamsat) {cat("Precalculated TAMSAT data file found (simply delete this file if you want R to recalculate everything from fresh)\n")}
choice2=choice2tmp;choice1=choice1tmp;rm(choice2tmp);rm(choice1tmp)

doinitialplots=FALSE	#Turn on initial plots about TAMSAT data etc.
flagrecalc=TRUE	#Set to TRUE to substitute in the model values for calculating SPI rather than TAMSAT values as I had it before (=FALSE for how I had it before) (n.b. this is not coded very well: it calculates everything to do with flagrecalc=FALSE and then throws it away to use the values read in from Dann's files)
cropplots=FALSE	#=TRUE means crop all plots to the bimodal seasonality area in GHoA *** NOT IMPLEMENTED CORRECTLY ***: this crops to the 2014 bimodal area not the TAMSAT bimodal area. Rats! Just keep it FALSE for now ...
startyr=1983;endyr=2012	#Baseline I'm using from the TAMSAT climatology (should be 1983-2012). If these are changed, must regenerate the TAMSAT climatology files
rtimexaxis=TRUE
colflag1=TRUE	#Set colflag1=TRUE to have the area coloured by modal month number, =FALSE to have the area coloured according to where the long rains total rainfall is less than the short rains total (=1) or not (=2). tried it, but didn't like colflag1=FALSE.
imposedmaxrt=100	#Having the x axis always run to 150 year return times can be neater (put =0 here to make it choose an appropriate max each time instead)
accuracy=100

#Read in the lats and lons from file landmasknc first
if (landmasknc!="") {
 cat("\nReading in land mask.\n")
 landmaskdata=readinfromnc(landmasknc,4)
 lonindex=1:length(landmaskdata$lonvals);fliplatindex=1:length(landmaskdata$fliplatvals)
 lonindex=which(landmaskdata$lonvals>=lonmin & landmaskdata$lonvals<=lonmax)
 fliplatindex=which(landmaskdata$fliplatvals>=latmin & landmaskdata$fliplatvals<=latmax)
 countlandpts=0;countselectedlandpts=0
 landlats=c();landlons=c();pointslist=c()
 for (fliplatindexi in length(landmaskdata$fliplatvals):1) {
  for (lonindexi in 1:length(landmaskdata$lonvals)) {
   if (is.finite(landmaskdata$datain[lonindexi,length(landmaskdata$fliplatvals)-fliplatindexi+1])) {
    if (landmaskdata$datain[lonindexi,length(landmaskdata$fliplatvals)-fliplatindexi+1]>0.99999) {
     countlandpts=countlandpts+1;landlats=c(landlats,landmaskdata$fliplatvals[fliplatindexi]);landlons=c(landlons,landmaskdata$lonvals[lonindexi])
     if (landmaskdata$lonvals[lonindexi]>=lonmin & landmaskdata$lonvals[lonindexi]<=lonmax & landmaskdata$fliplatvals[fliplatindexi]>=latmin & landmaskdata$fliplatvals[fliplatindexi]<=latmax) {countselectedlandpts=countselectedlandpts+1;pointslist=c(pointslist,countlandpts)}
    }
   }
  }
 }
 cat("Land point numbers for the area selected are (",countselectedlandpts,"points out of",countlandpts,"land points across the grid (grid=",length(landmaskdata$lonvals),"E-W by",length(landmaskdata$fliplatvals),"N-S =",length(landmaskdata$lonvals)*length(landmaskdata$fliplatvals),"gridcells)):\n     ",toString(pointslist),"\n")
}

txtvar=c("LongRains","ShortRains","BorealWinter","BorealSummer","Seasonality","ModalMonth","SecondModalMonth","ThirdModalMonth","SeasonalityPlus")	#plotpervar=5 for the seasonality plot (like inset to Yang et al. 2015:Fig.4) and that's the index here.
txtunits=c("mm sum over period","mm sum over period","mm sum over period","mm sum over period","1 = 2 seasons, 0 otherwise","month no.","month no.","month no.","months")	#For the first four, each month is in mm so the sum is also in mm
mnthrange=list(3:5,10:12,1:2,6:9,1:12,1:12,1:12,1:12,1:12)	#mnthrange[[plotpervar]] will be the range of months used (e.g. 3:5 for the Long Rains; 1:12 is a 'default' value)
plotpertxt=c("Mar-May","Oct-Dec","Jan-Feb","Jun-Sep","Jan-Dec","Jan-Dec","Jan-Dec","Jan-Dec","Jan-Dec")	#I use plotpervar as the index on plotpertxt in the initial plots and then set plotpervar=1 after first section so that all subsequent plots are for the long rains mean (change it e.g. to =2 to get plots for the short rains mean)

if (tamsat) {
 mnths=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec (from year before)")

#LOSE ONE INDENT

#The HadAM3P data
netcdffile=c("D:/D.DRIVE/MODELS/iofiles/DrivingData/ace_africa/Workunits_for_Fredi/hadam3p_afr_saaa_2013_1_009343828_1/saaaga.pdl4jan.nc","D:/D.DRIVE/MODELS/iofiles/DrivingData/ace_africa/Workunits_for_Fredi/hadam3p_afr_saaa_2013_1_009343828_1/saaaga.pdl4feb.nc","D:/D.DRIVE/MODELS/iofiles/DrivingData/ace_africa/Workunits_for_Fredi/hadam3p_afr_saaa_2013_1_009343828_1/saaaga.pdl4mar.nc","D:/D.DRIVE/MODELS/iofiles/DrivingData/ace_africa/Workunits_for_Fredi/hadam3p_afr_saaa_2013_1_009343828_1/saaaga.pdl4apr.nc","D:/D.DRIVE/MODELS/iofiles/DrivingData/ace_africa/Workunits_for_Fredi/hadam3p_afr_saaa_2013_1_009343828_1/saaaga.pdl4may.nc","D:/D.DRIVE/MODELS/iofiles/DrivingData/ace_africa/Workunits_for_Fredi/hadam3p_afr_saaa_2013_1_009343828_1/saaaga.pdl4jun.nc","D:/D.DRIVE/MODELS/iofiles/DrivingData/ace_africa/Workunits_for_Fredi/hadam3p_afr_saaa_2013_1_009343828_1/saaaga.pdl4jul.nc","D:/D.DRIVE/MODELS/iofiles/DrivingData/ace_africa/Workunits_for_Fredi/hadam3p_afr_saaa_2013_1_009343828_1/saaaga.pdl4aug.nc","D:/D.DRIVE/MODELS/iofiles/DrivingData/ace_africa/Workunits_for_Fredi/hadam3p_afr_saaa_2013_1_009343828_1/saaaga.pdl4sep.nc","D:/D.DRIVE/MODELS/iofiles/DrivingData/ace_africa/Workunits_for_Fredi/hadam3p_afr_saaa_2013_1_009343828_1/saaaga.pdl4oct.nc","D:/D.DRIVE/MODELS/iofiles/DrivingData/ace_africa/Workunits_for_Fredi/hadam3p_afr_saaa_2013_1_009343828_1/saaaga.pdl4nov.nc",
"D:/D.DRIVE/MODELS/iofiles/DrivingData/ace_africa/Workunits_for_Fredi/hadam3p_afr_saaa_2013_1_009343828_1/saaaga.pdl3dec.nc")
startyrH=2014;endyrH=2014
nmnths=12;nyrs=endyrH-startyrH+1
for (yr in startyrH:endyrH) {
 for (mnth in 1:nmnths) {
#  menuHadAM3P=detailsnc(netcdffile[((yr-startyrH)*nmnths)+mnth])
  choiceHadAM3P=7	#  choiceHadAM3P=menu(menuHadAM3P)
  if (file.exists(netcdffile[((yr-startyrH)*nmnths)+mnth])) {
   cat("\nDoing",mnths[mnth],"of",yr,"\n")
   var2plot=readinfromnc(netcdffile[((yr-startyrH)*nmnths)+mnth],choiceHadAM3P)
   if (mnth==1) {
    lonindexTAM=1:length(var2plot$lonvals);fliplatindexTAM=1:length(var2plot$fliplatvals)
    lonindexTAM=which(var2plot$lonvals>=lonmin & var2plot$lonvals<=lonmax)
    fliplatindexTAM=which(var2plot$fliplatvals>=latmin & var2plot$fliplatvals<=latmax)
    npoints=length(lonindexTAM)*length(fliplatindexTAM)
    Longitude=rep(NA,times=npoints);Latitude=rep(NA,times=npoints)
    valsinMAT=matrix(NA,nrow=npoints,ncol=nmnths)
    if (yr==startyrH) {
     LongRains=matrix(NA,nrow=npoints,ncol=nyrs)
     ShortRains=matrix(NA,nrow=npoints,ncol=nyrs)
     BorealWinter=matrix(NA,nrow=npoints,ncol=nyrs)
     BorealSummer=matrix(NA,nrow=npoints,ncol=nyrs)
     Seasonality=matrix(NA,nrow=npoints,ncol=nyrs)
     MM=matrix(NA,nrow=npoints,ncol=nyrs)
     SMM=matrix(NA,nrow=npoints,ncol=nyrs)
     TMM=matrix(NA,nrow=npoints,ncol=nyrs)
     SeasonalityPlus=matrix(NA,nrow=npoints,ncol=nyrs)
    }
   }
   for (ii in 1:length(lonindexTAM)) {
    for (jj in 1:length(fliplatindexTAM)) {
     stckindx=((ii-1)*length(fliplatindexTAM))+jj
     maxday=var2plot$varsize[4]
     jcoord=length(var2plot$fliplatvals)-fliplatindexTAM[jj]+1
     valsinMAT[stckindx,mnth]=sum(var2plot$datain[lonindexTAM[ii],jcoord,1:maxday]*60*60*24);var2plot$varunits="mm"
     Longitude[stckindx]=var2plot$lonvals[lonindexTAM[ii]]
     Latitude[stckindx]=var2plot$fliplatvals[fliplatindexTAM[jj]]
    }
   }
  } else {
   cat("\nNot doing",mnths[mnth],"of",yr,": file does not exist\n")
  }
 }
 for (stckindx in 1:npoints) {
  LongRains[stckindx,yr-startyrH+1]=sum(valsinMAT[stckindx,mnthrange[[1]]],na.rm=TRUE)
  ShortRains[stckindx,yr-startyrH+1]=sum(valsinMAT[stckindx,mnthrange[[2]]],na.rm=TRUE)
  BorealWinter[stckindx,yr-startyrH+1]=sum(valsinMAT[stckindx,mnthrange[[3]]],na.rm=TRUE)
  BorealSummer[stckindx,yr-startyrH+1]=sum(valsinMAT[stckindx,mnthrange[[4]]],na.rm=TRUE)
  Seasonality[stckindx,yr-startyrH+1]=ifelse(LongRains[stckindx,yr-startyrH+1]>BorealSummer[stckindx,yr-startyrH+1] & LongRains[stckindx,yr-startyrH+1]>BorealWinter[stckindx,yr-startyrH+1],1,0)
  mnthprecips=valsinMAT[stckindx,mnthrange[[6]]]
  maxmonth=which(mnthprecips==max(mnthprecips,na.rm=TRUE))
  if (is.na(sum(mnthprecips))) {maxmonth=NA}	#if mnthprecips is c( NA   0   0   0   0   9 100  NA   4   0   0  NA ) then maxmonth will be 7 when actually I want it to be NA
  if (length(maxmonth)>1) {maxmonth=maxmonth[1]}
  if (length(maxmonth)>0) {
   MM[stckindx,yr-startyrH+1]=maxmonth	#The month number of the wettest month, or the first one in the calendar year if there are ties for first
   blankfrom=maxmonth;blankto=maxmonth
   n=1
   repeat {
    if (n>=maxmonth) {break}
    if (!is.finite(mnthprecips[maxmonth-n])) {break}
    if (mnthprecips[maxmonth-n]>mnthprecips[maxmonth-(n-1)]) {break}
    blankfrom=blankfrom-1;n=n+1
   }
   n=1;tmp=12-maxmonth+1
   repeat {
    if (n>=tmp) {break}
    if (!is.finite(mnthprecips[maxmonth+n])) {break}
    if (mnthprecips[maxmonth+n]>mnthprecips[maxmonth+(n-1)]) {break}
    blankto=blankto+1;n=n+1
   }
   mnthprecips[blankfrom:blankto]=-999
   SMM[stckindx,yr-startyrH+1]=NA;TMM[stckindx,yr-startyrH+1]=NA
   if (max(mnthprecips,na.rm=TRUE)>=0) {
    maxmonth=which(mnthprecips==max(mnthprecips,na.rm=TRUE));if (length(maxmonth)>1) {maxmonth=maxmonth[1]}
    SMM[stckindx,yr-startyrH+1]=maxmonth	#Second modal month, or NA if there is only one mode
    blankfrom=maxmonth;blankto=maxmonth
    n=1
    repeat {
     if (n>=maxmonth) {break}
     if (!is.finite(mnthprecips[maxmonth-n])) {break}
     if (mnthprecips[maxmonth-n]>mnthprecips[maxmonth-(n-1)]) {break}
     blankfrom=blankfrom-1;n=n+1
    }
    n=1;tmp=12-maxmonth+1
    repeat {
     if (n>=tmp) {break}
     if (!is.finite(mnthprecips[maxmonth+n])) {break}
     if (mnthprecips[maxmonth+n]>mnthprecips[maxmonth+(n-1)]) {break}
     blankto=blankto+1;n=n+1
    }
    mnthprecips[blankfrom:blankto]=-999
    if (max(mnthprecips,na.rm=TRUE)>=0) {
     maxmonth=which(mnthprecips==max(mnthprecips,na.rm=TRUE));if (length(maxmonth)>1) {maxmonth=maxmonth[1]}
     TMM[stckindx,yr-startyrH+1]=maxmonth	#Third modal month (or NA)
    }
   }
#Ethiopian climate areas are in Fig. 1 of http://www.metoffice.gov.uk/media/pdf/o/9/PRECIS_Experimental_Design_Dawit.pdf:
#Regime A: a bi-modal rain classified as the long rainy season (June–September) and short rains (March-May) locally referred as Kiremt and Belg rains respectively (Figure 2). The rest of the months (October to February) are dry period.
#Regime B: a mono-modal rainfall pattern (June – September), and the rainy period ranges from February through November mainly in the western and south-western part of the country (Figure 3), and decreases northwards (Figure 4).
#Regime C: two distinct wet and dry seasons. The main rain season is from February through May, and short rains from October to November, and the dry periods are June to September and December to February (Figure 5).
#Kenya: The long rains start end of March and run through to the end of May. Northern Kenya sees most of its heavy in rain during the short rains in November.
   if (MM[stckindx,yr-startyrH+1]==3 & SMM[stckindx,yr-startyrH+1]>=10 & SMM[stckindx,yr-startyrH+1]<=12) {SeasonalityPlus[stckindx,yr-startyrH+1]=3}	#Regime C
   if (MM[stckindx,yr-startyrH+1]==4 & SMM[stckindx,yr-startyrH+1]>=10 & SMM[stckindx,yr-startyrH+1]<=12) {SeasonalityPlus[stckindx,yr-startyrH+1]=4}	#Regime C
   if (MM[stckindx,yr-startyrH+1]==5 & SMM[stckindx,yr-startyrH+1]>=10 & SMM[stckindx,yr-startyrH+1]<=12) {SeasonalityPlus[stckindx,yr-startyrH+1]=5}	#Regime C
   if (MM[stckindx,yr-startyrH+1]==10 & SMM[stckindx,yr-startyrH+1]>=3 & SMM[stckindx,yr-startyrH+1]<=5) {SeasonalityPlus[stckindx,yr-startyrH+1]=10}
   if (MM[stckindx,yr-startyrH+1]==11 & SMM[stckindx,yr-startyrH+1]>=3 & SMM[stckindx,yr-startyrH+1]<=5) {SeasonalityPlus[stckindx,yr-startyrH+1]=11}
   if (MM[stckindx,yr-startyrH+1]==12 & SMM[stckindx,yr-startyrH+1]>=3 & SMM[stckindx,yr-startyrH+1]<=5) {SeasonalityPlus[stckindx,yr-startyrH+1]=12}
  }
 }
}
LongRains=ifelse(is.finite(LongRains),LongRains,NA)
ShortRains=ifelse(is.finite(ShortRains),ShortRains,NA)
BorealWinter=ifelse(is.finite(BorealWinter),BorealWinter,NA)
BorealSummer=ifelse(is.finite(BorealSummer),BorealSummer,NA)
Seasonality=ifelse(is.finite(Seasonality),Seasonality,NA)
if (cropplots) {
 for (stckindx in 1:npoints) {
  if (Seasonality[stckindx,1]!=1){
   LongRains[stckindx,1]=NA;ShortRains[stckindx,1]=NA
   BorealWinter[stckindx,1]=NA;BorealSummer[stckindx,1]=NA
   MM[stckindx,1]=NA;SMM[stckindx,1]=NA;TMM[stckindx,1]=NA
  }
 }
}
for (plotpervar in 1:length(txtvar)) {
 minval=0;ncols=100
 if (plotpervar==1) {maxval=max(LongRains,na.rm=TRUE)}
 if (plotpervar==2) {maxval=max(ShortRains,na.rm=TRUE)}
 if (plotpervar==3) {maxval=max(BorealWinter,na.rm=TRUE)}
 if (plotpervar==4) {maxval=max(BorealSummer,na.rm=TRUE)}
 if (plotpervar==5) {minval=0;maxval=1;ncols=2+1}
 if (plotpervar==6 || plotpervar==7 || plotpervar==8) {minval=1;maxval=12;ncols=12+1}
 if (plotpervar==9) {minval=3;maxval=12;ncols=maxval-minval+2}
 for (yr in startyrH:endyrH) {
  maintxt=paste("HadAM3P ",txtvar[plotpervar]," (",txtunits[plotpervar],"): ",yr,sep="")
  if (doinitialplots && plotpervar==1) {dev.new();print(levelplot(LongRains[,yr-startyrH+1]~Longitude*Latitude,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
  if (doinitialplots && plotpervar==2) {dev.new();print(levelplot(ShortRains[,yr-startyrH+1]~Longitude*Latitude,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
  if (doinitialplots && plotpervar==3) {dev.new();print(levelplot(BorealWinter[,yr-startyrH+1]~Longitude*Latitude,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
  if (doinitialplots && plotpervar==4) {dev.new();print(levelplot(BorealSummer[,yr-startyrH+1]~Longitude*Latitude,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
  if (doinitialplots && plotpervar==5) {dev.new();print(levelplot(Seasonality[,yr-startyrH+1]~Longitude*Latitude,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
  if (doinitialplots && plotpervar==6) {dev.new();print(levelplot(MM[,yr-startyrH+1]~Longitude*Latitude,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
  if (doinitialplots && plotpervar==7) {dev.new();print(levelplot(SMM[,yr-startyrH+1]~Longitude*Latitude,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
  if (doinitialplots && plotpervar==8) {dev.new();print(levelplot(TMM[,yr-startyrH+1]~Longitude*Latitude,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
  if (doinitialplots && plotpervar==9) {dev.new();print(levelplot(SeasonalityPlus[,yr-startyrH+1]~Longitude*Latitude,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
 }
}
lons=Longitude[((1:length(lonindexTAM)-1)*length(fliplatindexTAM))+1];lats=Latitude[1:length(fliplatindexTAM)]
hadam3plons=lons;hadam3plats=lats
hadam3plonindexTAM=lonindexTAM;hadam3pfliplatindexTAM=fliplatindexTAM
txtyr=nyrs

#The TAMSAT data
mnths=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
netcdffile=c()
for (yr in startyr:endyr) {
 netcdffile=c(netcdffile,paste("D:/GISwork/TAMSAT_rfe_monthly/",yr,"/",c(paste("0",1:9,sep=""),"10","11","12"),"/rfe",yr,"_",c(paste("0",1:9,sep=""),"10","11","12"),".nc",sep=""))
}
nmnths=12;nyrs=endyr-startyr+1
for (yr in startyr:endyr) {	#This loop takes quite a few minutes to run
 for (mnth in 1:nmnths) {
#  menuTAMSAT=detailsnc(netcdffile[((yr-startyr)*nmnths)+mnth])
  choiceTAMSAT=1	#  choiceTAMSAT=menu(menuTAMSAT)
  if (file.exists(netcdffile[((yr-startyr)*nmnths)+mnth])) {
   cat("\nDoing",mnths[mnth],"of",yr,"\n")
   var2plot=readinfromnc(netcdffile[((yr-startyr)*nmnths)+mnth],choiceTAMSAT)	#RFE = Rain Fall Estimate (rfe in mm)
   if (mnth==1) {
    lonindexTAM=1:length(var2plot$lonvals);fliplatindexTAM=1:length(var2plot$fliplatvals)
    lonindexTAM=which(var2plot$lonvals>=lonmin & var2plot$lonvals<=lonmax)
    fliplatindexTAM=which(var2plot$fliplatvals>=latmin & var2plot$fliplatvals<=latmax)
    npoints=length(lonindexTAM)*length(fliplatindexTAM)
    Longitude=rep(NA,times=npoints);Latitude=rep(NA,times=npoints)
    valsinMAT=matrix(NA,nrow=npoints,ncol=nmnths)
    if (yr==startyr) {mnthly=matrix(NA,nrow=npoints,ncol=nyrs*nmnths)}
   }
   for (ii in 1:length(lonindexTAM)) {
    for (jj in 1:length(fliplatindexTAM)) {
     stckindx=((ii-1)*length(fliplatindexTAM))+jj
     jcoord=length(var2plot$fliplatvals)-fliplatindexTAM[jj]+1
     valsinMAT[stckindx,mnth]=var2plot$datain[lonindexTAM[ii],jcoord]
     Longitude[stckindx]=var2plot$lonvals[lonindexTAM[ii]]
     Latitude[stckindx]=var2plot$fliplatvals[fliplatindexTAM[jj]]
     mnthly[stckindx,((yr-startyr)*nmnths)+mnth]=valsinMAT[stckindx,mnth]
    }
   }
  } else {
   cat("\nNot doing",mnths[mnth],"of",yr,": file does not exist\n")
  }
 }
}
mnthly=ifelse(is.finite(mnthly),mnthly,NA)
minval=0;maxval=max(mnthly,na.rm=TRUE)
lons=Longitude[((1:length(lonindexTAM)-1)*length(fliplatindexTAM))+1];lats=Latitude[1:length(fliplatindexTAM)]

tamsatlons=lons;tamsatlats=lats
tamsatmnthlymean=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM),nmnths))
tamsatmnthlysd=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM),nmnths))
for (ii in 1:length(lonindexTAM)) {
 for (jj in 1:length(fliplatindexTAM)) {
  for (mnth in 1:nmnths) {
   stckindx=((ii-1)*length(fliplatindexTAM))+jj
   seq1=seq(from=mnth,to=mnth+((endyr-startyr)*nmnths),by=nmnths)
   tamsatmnthlymean[ii,jj,mnth]=mean(mnthly[stckindx,seq1],na.rm=TRUE)	#mean monthly precip at each pixel (mean taken across 1983-2012
   tamsatmnthlysd[ii,jj,mnth]=sd(mnthly[stckindx,seq1],na.rm=TRUE)	#SD	/sqrt(sum(is.finite((mnthly[stckindx,seq1]))))	#SE
  }
 }
}
tamsatmnthlymean=ifelse(is.finite(tamsatmnthlymean),tamsatmnthlymean,NA)
maxval=max(tamsatmnthlymean,na.rm=TRUE);minval=-maxval
lons=Longitude[((1:length(lonindexTAM)-1)*length(fliplatindexTAM))+1];lats=Latitude[1:length(fliplatindexTAM)]
for (mnth in 1:nmnths) {
 maintxt=paste("TAMSAT climatology monthly mean ",var2plot$varname," (",var2plot$varunits,"): ",mnths[mnth],sep="")
 if (doinitialplots) {dev.new();print(levelplot(tamsatmnthlymean[,,mnth],row.values=lons,column.values=lats,at=seq(from=minval,to=maxval,length.out=maxval-minval+2),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
}

#Now calculate the same variables as did above for the HadAM3P plot:
LongRainsTAM=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))
ShortRainsTAM=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))
BorealWinterTAM=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))
BorealSummerTAM=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))
SeasonalityTAM=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))
MMTAM=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))
SMMTAM=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))
TMMTAM=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))
SeasonalityPlusTAM=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))

for (ii in 1:length(lonindexTAM)) {
 for (jj in 1:length(fliplatindexTAM)) {
  LongRainsTAM[ii,jj]=sum(tamsatmnthlymean[ii,jj,mnthrange[[1]]],na.rm=TRUE)
  ShortRainsTAM[ii,jj]=sum(tamsatmnthlymean[ii,jj,mnthrange[[2]]],na.rm=TRUE)
  BorealWinterTAM[ii,jj]=sum(tamsatmnthlymean[ii,jj,mnthrange[[3]]],na.rm=TRUE)
  BorealSummerTAM[ii,jj]=sum(tamsatmnthlymean[ii,jj,mnthrange[[4]]],na.rm=TRUE)
  SeasonalityTAM[ii,jj]=ifelse(LongRainsTAM[ii,jj]>BorealSummerTAM[ii,jj] & LongRainsTAM[ii,jj]>BorealWinterTAM[ii,jj],1,0)
  mnthprecips=tamsatmnthlymean[ii,jj,mnthrange[[6]]]
  maxmonth=which(mnthprecips==max(mnthprecips,na.rm=TRUE));if (length(maxmonth)>1) {maxmonth=maxmonth[1]}
  if (length(maxmonth)>0) {
   MMTAM[ii,jj]=maxmonth	#The month number of the wettest month, or the first one in the calendar year if there are ties for first
   blankfrom=maxmonth;blankto=maxmonth
   n=1
   repeat {
    if (n>=maxmonth) {break}
    if (!is.finite(mnthprecips[maxmonth-n])) {break}
    if (mnthprecips[maxmonth-n]>mnthprecips[maxmonth-(n-1)]) {break}
    blankfrom=blankfrom-1;n=n+1
   }
   n=1;tmp=12-maxmonth+1
   repeat {
    if (n>=tmp) {break}
    if (!is.finite(mnthprecips[maxmonth+n])) {break}
    if (mnthprecips[maxmonth+n]>mnthprecips[maxmonth+(n-1)]) {break}
    blankto=blankto+1;n=n+1
   }
   mnthprecips[blankfrom:blankto]=-999
   SMMTAM[ii,jj]=NA;TMMTAM[ii,jj]=NA
   if (max(mnthprecips,na.rm=TRUE)>=0) {
    maxmonth=which(mnthprecips==max(mnthprecips,na.rm=TRUE));if (length(maxmonth)>1) {maxmonth=maxmonth[1]}
    SMMTAM[ii,jj]=maxmonth	#Second modal month, or NA if there is only one mode
    blankfrom=maxmonth;blankto=maxmonth
    n=1
    repeat {
     if (n>=maxmonth) {break}
     if (!is.finite(mnthprecips[maxmonth-n])) {break}
     if (mnthprecips[maxmonth-n]>mnthprecips[maxmonth-(n-1)]) {break}
     blankfrom=blankfrom-1;n=n+1
    }
    n=1;tmp=12-maxmonth+1
    repeat {
     if (n>=tmp) {break}
     if (!is.finite(mnthprecips[maxmonth+n])) {break}
     if (mnthprecips[maxmonth+n]>mnthprecips[maxmonth+(n-1)]) {break}
     blankto=blankto+1;n=n+1
    }
    mnthprecips[blankfrom:blankto]=-999
    if (max(mnthprecips,na.rm=TRUE)>=0) {
     maxmonth=which(mnthprecips==max(mnthprecips,na.rm=TRUE));if (length(maxmonth)>1) {maxmonth=maxmonth[1]}
     TMMTAM[ii,jj]=maxmonth	#Third modal month (or NA)
    }
   }
   if (colflag1) {
    if (SeasonalityTAM[ii,jj]==1 && is.finite(MMTAM[ii,jj]) && is.finite(SMMTAM[ii,jj])) {
#Ethiopian climate areas are in Fig. 1 of http://www.metoffice.gov.uk/media/pdf/o/9/PRECIS_Experimental_Design_Dawit.pdf:
#Regime A: a bi-modal rain classified as the long rainy season (June–September) and short rains (March-May) locally referred as Kiremt and Belg rains respectively (Figure 2). The rest of the months (October to February) are dry period.
#Regime B: a mono-modal rainfall pattern (June – September), and the rainy period ranges from February through November mainly in the western and south-western part of the country (Figure 3), and decreases northwards (Figure 4).
#Regime C: two distinct wet and dry seasons. The main rain season is from February through May, and short rains from October to November, and the dry periods are June to September and December to February (Figure 5).
#Kenya: The long rains start end of March and run through to the end of May. Northern Kenya sees most of its heavy in rain during the short rains in November.
     if (MMTAM[ii,jj]==3 & SMMTAM[ii,jj]>=10 & SMMTAM[ii,jj]<=12) {SeasonalityPlusTAM[ii,jj]=3}	#Regime C
     if (MMTAM[ii,jj]==4 & SMMTAM[ii,jj]>=10 & SMMTAM[ii,jj]<=12) {SeasonalityPlusTAM[ii,jj]=4}	#Regime C
     if (MMTAM[ii,jj]==5 & SMMTAM[ii,jj]>=10 & SMMTAM[ii,jj]<=12) {SeasonalityPlusTAM[ii,jj]=5}	#Regime C
     if (MMTAM[ii,jj]==10 & SMMTAM[ii,jj]>=3 & SMMTAM[ii,jj]<=5) {SeasonalityPlusTAM[ii,jj]=10}
     if (MMTAM[ii,jj]==11 & SMMTAM[ii,jj]>=3 & SMMTAM[ii,jj]<=5) {SeasonalityPlusTAM[ii,jj]=11}
     if (MMTAM[ii,jj]==12 & SMMTAM[ii,jj]>=3 & SMMTAM[ii,jj]<=5) {SeasonalityPlusTAM[ii,jj]=12}
    }
   } else {
    if (is.finite(LongRainsTAM[ii,jj]) && is.finite(ShortRainsTAM[ii,jj])) {
     if (LongRainsTAM[ii,jj]<ShortRainsTAM[ii,jj]) {SeasonalityPlusTAM[ii,jj]=1} else {SeasonalityPlusTAM[ii,jj]=2}
    }
   }
  }
 }
}
LongRainsTAM=ifelse(is.finite(LongRainsTAM),LongRainsTAM,NA)
ShortRainsTAM=ifelse(is.finite(ShortRainsTAM),ShortRainsTAM,NA)
BorealWinterTAM=ifelse(is.finite(BorealWinterTAM),BorealWinterTAM,NA)
BorealSummerTAM=ifelse(is.finite(BorealSummerTAM),BorealSummerTAM,NA)
SeasonalityTAM=ifelse(is.finite(SeasonalityTAM),SeasonalityTAM,NA)
for (plotpervar in 1:length(txtvar)) {
 minval=0;ncols=100
 if (plotpervar==1) {maxval=max(LongRainsTAM,na.rm=TRUE)}
 if (plotpervar==2) {maxval=max(ShortRainsTAM,na.rm=TRUE)}
 if (plotpervar==3) {maxval=max(BorealWinterTAM,na.rm=TRUE)}
 if (plotpervar==4) {maxval=max(BorealSummerTAM,na.rm=TRUE)}
 if (plotpervar==5) {minval=0;maxval=1;ncols=2+1}
 if (plotpervar==6 || plotpervar==7 || plotpervar==8) {minval=1;maxval=12;ncols=12+1}
 if (plotpervar==9 && colflag1) {minval=3;maxval=12;ncols=maxval-minval+2}
 if (plotpervar==9 && !colflag1) {minval=1;maxval=2;ncols=maxval-minval+2}
 maintxt=paste("TAMSAT climatology ",txtvar[plotpervar]," (",txtunits[plotpervar],")",sep="")
 if (doinitialplots && plotpervar==1) {dev.new();print(levelplot(LongRainsTAM,row.values=tamsatlons,column.values=tamsatlats,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
 if (doinitialplots && plotpervar==2) {dev.new();print(levelplot(ShortRainsTAM,row.values=tamsatlons,column.values=tamsatlats,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
 if (doinitialplots && plotpervar==3) {dev.new();print(levelplot(BorealWinterTAM,row.values=tamsatlons,column.values=tamsatlats,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
 if (doinitialplots && plotpervar==4) {dev.new();print(levelplot(BorealSummerTAM,row.values=tamsatlons,column.values=tamsatlats,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
 if (doinitialplots && plotpervar==5) {dev.new();print(levelplot(SeasonalityTAM,row.values=tamsatlons,column.values=tamsatlats,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
 if (doinitialplots && plotpervar==6) {dev.new();print(levelplot(MMTAM,row.values=tamsatlons,column.values=tamsatlats,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
 if (doinitialplots && plotpervar==7) {dev.new();print(levelplot(SMMTAM,row.values=tamsatlons,column.values=tamsatlats,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
 if (doinitialplots && plotpervar==8) {dev.new();print(levelplot(TMMTAM,row.values=tamsatlons,column.values=tamsatlats,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
 if (doinitialplots && plotpervar==9) {dev.new();print(levelplot(SeasonalityPlusTAM,row.values=tamsatlons,column.values=tamsatlats,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main=maintxt,asp=1))}
}
#Now compare the two (resampling HadAM3P to TAMSAT resolution)
LongRainsHAD=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))
ShortRainsHAD=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))
BorealWinterHAD=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))
BorealSummerHAD=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))
SeasonalityHAD=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))
MMHAD=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))
SMMHAD=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))
TMMHAD=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))
SeasonalityPlusHAD=array(NA,dim=c(length(lonindexTAM),length(fliplatindexTAM)))
tmp=ncol(LongRains)
for (ii in 1:length(lonindexTAM)) {
 for (jj in 1:length(fliplatindexTAM)) {
  ii2=max(1,which(hadam3plons>tamsatlons[ii])[1]-1)
  jj2=max(1,which(hadam3plats>tamsatlats[jj])[1]-1)
  stckindx2=((ii2-1)*length(hadam3pfliplatindexTAM))+jj2
  if (!cropplots || (Seasonality[stckindx2,tmp]==1 && is.finite(MM[stckindx2,tmp]) && is.finite(SMM[stckindx2,tmp]))) {
   LongRainsHAD[ii,jj]=LongRains[stckindx2,tmp]
   ShortRainsHAD[ii,jj]=ShortRains[stckindx2,tmp]
   BorealWinterHAD[ii,jj]=BorealWinter[stckindx2,tmp]
   BorealSummerHAD[ii,jj]=BorealSummer[stckindx2,tmp]
   SeasonalityHAD[ii,jj]=Seasonality[stckindx2,tmp]
  }
 }
}
fname=paste("C:/Users/tobmar/Desktop/hadam3p_resampled_",txtyr,".nc",sep="")
lons=Longitude[((1:length(lonindexTAM)-1)*length(fliplatindexTAM))+1];lats=Latitude[1:length(fliplatindexTAM)]
#The following lines do work, but don't need them just now. They create an .nc file on the Desktop that contains a copy of the HAD data resampled to the right resolution
#dimlon=ncdim_def("Longitude","degE",lons);dimlat=ncdim_def("Latitude","degN",lats)
#varid1=ncvar_def(name=paste("hadam3p_resampled_",txtvar[1],"_precip_",txtyr,sep=""),units=txtunits[1],dim=list(dimlon,dimlat),missval=-1)
#varid2=ncvar_def(name=paste("hadam3p_resampled_",txtvar[2],"_precip_",txtyr,sep=""),units=txtunits[2],dim=list(dimlon,dimlat),missval=-1)
#varid3=ncvar_def(name=paste("hadam3p_resampled_",txtvar[3],"_precip_",txtyr,sep=""),units=txtunits[3],dim=list(dimlon,dimlat),missval=-1)
#varid4=ncvar_def(name=paste("hadam3p_resampled_",txtvar[4],"_precip_",txtyr,sep=""),units=txtunits[4],dim=list(dimlon,dimlat),missval=-1)
#varid5=ncvar_def(name=paste("hadam3p_resampled_",txtvar[5],"_",txtyr,sep=""),units=txtunits[5],dim=list(dimlon,dimlat),missval=-1)
#ncnew=nc_create(fname,list(varid1,varid2,varid3,varid4,varid5))
#ncvar_put(ncnew,varid1,LongRainsHAD)
#ncvar_put(ncnew,varid2,ShortRainsHAD)
#ncvar_put(ncnew,varid3,BorealWinterHAD)
#ncvar_put(ncnew,varid4,BorealSummerHAD)
#ncvar_put(ncnew,varid5,SeasonalityHAD)
#nc_close(ncnew)

if (doinitialplots) {
 for (plotpervar in 1:5) {
  if (plotpervar==1) {diffHT=LongRainsHAD-LongRainsTAM}
  if (plotpervar==2) {diffHT=ShortRainsHAD-ShortRainsTAM}
  if (plotpervar==3) {diffHT=BorealWinterHAD-BorealWinterTAM}
  if (plotpervar==4) {diffHT=BorealSummerHAD-BorealSummerTAM}
  if (plotpervar==5) {diffHT=SeasonalityHAD-SeasonalityTAM}
  diffHT=ifelse(is.finite(diffHT),diffHT,NA)
  maxval=max(c(diffHT,-diffHT),na.rm=TRUE);minval=-maxval
  dev.new();print(levelplot(diffHT,row.values=tamsatlons,column.values=tamsatlats,at=seq(from=minval,to=maxval,length.out=maxval-minval+2),col.regions=colorRampPalette((c("purple","white","green"))),panel=pnl,main=paste("HadAM3P ",txtvar[plotpervar]," minus TAMSAT ",txtvar[plotpervar],sep=""),asp=1))
 }
}
plotpervar=1
if (choice1==2) {rm(tamsat);save.image("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataSmallRegion.RData")}
if (choice1==3) {rm(tamsat);save.image("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataLargeRegion.RData")}
if (choice1==6) {rm(tamsat);save.image("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataNWKenyaRegion.RData")}
if (choice1==7) {rm(tamsat);save.image("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataAhmarRegion.RData")}
if (choice1==8) {rm(tamsat);save.image("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataJubaShabelleRegion.RData")}
if (choice1==9) {rm(tamsat);save.image("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/TAMSATdataNEKenyaRegion.RData")}

#REGAIN INDENT

}

fileseqCF=c();fileseqF=c()
batch107chosen=c(66, 73, 382, 386, 437, 196, 286, 304, 384, 466, 80, 84, 146, 213, 512, 57, 76, 408, 445, 451, 69, 94, 132, 184, 478, 37, 48, 185, 270, 443, 49, 130, 266, 377, 404, 113, 164, 244, 411, 477, 82, 86, 339, 364, 379, 126, 167, 169, 496, 513, 102, 183, 359, 360, 406, 47, 71, 179, 447, 456, 61, 147, 381, 416, 452, 72, 226, 272, 284, 369, 192, 231, 316, 444, 485, 24, 28, 138, 387, 515, 154, 202, 205, 220, 263, 36, 224, 242, 275, 294, 70, 170, 277, 420, 436, 134, 218, 237, 334, 434)	#From batch107 (s...'s) and must match what is in ACEAfricaCTRL.f90; i.e. sadu, saef, satx, sau6, ..., sar8, sawn. COUNTERFACTUAL.
if (choice2==1) {batch107chosen=batch107chosen[1:qtr]}

batch111chosen=c(77, 407, 444, 508, 514, 173, 322, 356, 477, 483, 176, 269, 277, 304, 437, 88, 92, 329, 398, 513, 306, 311, 438, 481, 492, 27, 154, 196, 294, 436, 119, 189, 278, 328, 488, 34, 46, 230, 260, 464, 72, 232, 366, 381, 493, 216, 254, 292, 489, 518, 22, 197, 202, 335, 380, 171, 320, 327, 370, 469, 157, 293, 390, 414, 466, 163, 193, 195, 218, 384, 158, 262, 410, 416, 446, 126, 180, 280, 357, 476, 69, 179, 181, 250, 419, 192, 238, 288, 371, 372, 90, 111, 210, 296, 415, 41, 324, 353, 409, 428)	#From batch111 (a...'s and p...'s) and must match what is in ACEAfricaCTRL.f90; i.e. aael, aay1, ab0j, pabs, ..., aay6, aazj. FACTUAL.
if (choice2==1) {batch111chosen=batch111chosen[1:qtr]}
mnths=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec (from 2013)")

file107=read.table("D:/D.DRIVE/MODELS/iofiles/io_ace_africa/batch107.txt")
file111=read.table("D:/D.DRIVE/MODELS/iofiles/io_ace_africa/batch111.txt")
ecode107=c()
for (ii in 1:length(batch107chosen)) {
 thisensemble=batch107chosen[ii]
 batchstr=as.character(file107$V1[((thisensemble-1)*13)+1])
 lastbit=strsplit(strsplit(as.character(batchstr),"/")[[1]][8],"_")[[1]][8]
 ecode107=c(ecode107,strsplit(strsplit(as.character(batchstr),"/")[[1]][7],"_")[[1]][3])
}
ecode111=c()
for (ii in 1:length(batch111chosen)) {
 thisensemble=batch111chosen[ii]
 batchstr=as.character(file111$V1[((thisensemble-1)*13)+1])
 lastbit=strsplit(strsplit(as.character(batchstr),"/")[[1]][8],"_")[[1]][8]
 ecode111=c(ecode111,strsplit(strsplit(as.character(batchstr),"/")[[1]][7],"_")[[1]][3])
}
fileseqF=paste("D:/D.DRIVE/MODELS/iofiles/io_ace_africa/ANALYSISbams/MainRuns/output.",ecode107,"ga/MR",ecode107,".monthly.nc",sep="")
fileseqCF=paste("D:/D.DRIVE/MODELS/iofiles/io_ace_africa/ANALYSISbams/MainRuns/output.",ecode111,"ga/MR",ecode111,".monthly.nc",sep="")
if (choice2==1) {fileseqF=fileseqF[1:qtr];fileseqCF=fileseqCF[1:qtr]}
timetxt="months since 2013-12-01"	#timetxt MUST start with the word "months" otherwise tptsflag will be FALSE
tptsflag=(substr(timetxt,start=1,stop=6)=="months")
nCF=length(fileseqCF);nF=length(fileseqF)
cat("\nNo. of counterfactual ensemble members to plot:",nCF,"\n")
cat("No. of factual ensemble members to plot:",nF,"\n\n")

if (flagrecalc) {
 ncid7=nc_open("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/prp_std_sum_mam_Africa.nc")
 prpvar=ncid7$var$prp_std
 varsize=prpvar$varsize
 nland=varsize[1]
 prpstds=ncvar_get(ncid7,prpvar,start=rep(1,times=prpvar$ndims),count=varsize) 
 lons=ncid7$var$prp_mean$dim[[1]]$vals
 lats=ncid7$var$prp_mean$dim[[2]]$vals
 nc_close(ncid7)
 ncid7=nc_open("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/prp_mean_sum_mam_Africa.nc")
 prpvar=ncid7$var$prp_mean
 varsize=prpvar$varsize
 nland=varsize[1]
 prpmeans=ncvar_get(ncid7,prpvar,start=rep(1,times=prpvar$ndims),count=varsize) 
 lons=ncid7$var$prp_mean$dim[[1]]$vals
 lats=ncid7$var$prp_mean$dim[[2]]$vals
 nc_close(ncid7)
 lonindexTAM=1:200;fliplatindexTAM=138:1
 npoints=length(lonindexTAM)*length(fliplatindexTAM)
 LongitudePRP=rep(NA,times=npoints);LatitudePRP=rep(NA,times=npoints);LongRainsPRP=rep(NA,times=npoints)
 for (ii in 1:200) {
  for (jj in 1:138) {
   stckindx=((ii-1)*138)+jj
   jcoord=138-fliplatindexTAM[jj]+1
   LongRainsPRP[stckindx]=prpmeans[ii,jcoord]
   LongitudePRP[stckindx]=lons[ii]
   LatitudePRP[stckindx]=lats[jj]
  }
 }
# dev.new();print(levelplot(LongRainsPRP~LongitudePRP*LatitudePRP,col.regions=cpal,panel=pnl,main="Precipitation MAM total from Dann",asp=1))
}
cat("\n\n")

choice8=NA
meanvals=c();maxvals=c();minvals=c();varlvl=1;varlvltxt=""
firstexistentfile=TRUE
for (ensmem in 1:(nCF+nF)) {
 netcdffile=c(fileseqCF,fileseqF)[ensmem]
 if (!file.exists(netcdffile)) {
  cat("File not found (#",ensmem,"):",netcdffile,"\n")
 } else {
  cat("Reading file (#",ensmem,"):",netcdffile,"\n")
  if (firstexistentfile) {
   cat("   (sthu = Unfrozen moisture content of each soil layer as a fraction of saturation, sthzw = Soil wetness in the deep (water table) layer)\n")
   varsinncfile=detailsnc(netcdffile)	#Every file in fileseqCF and fileseqF are going to have THE SAME dimensions and variables so can generate the menu list from the first one only
   precip=readinfromnc(netcdffile,10)	#Get npts from the precip field (but could have used any other)
   precip$datain=ifelse(precip$datain>=0.01/(60*60*24),precip$datain,0.01/(60*60*24))	#I did find some negative values for precip that I don't think should be there (and I have to exclude 0)
   lonindex=1:length(precip$lonvals);fliplatindex=1:length(precip$fliplatvals)
   lonindex=which(precip$lonvals>=lonmin & precip$lonvals<=lonmax)
   fliplatindex=which(precip$fliplatvals>=latmin & precip$fliplatvals<=latmax)
   ntpts=precip$varsize[3]
   npts=length(lonindex)*length(fliplatindex)
   Longitude=rep(NA,times=npts);Latitude=rep(NA,times=npts)
   for (ii in 1:length(lonindex)) {
    for (jj in 1:length(fliplatindex)) {
     stckindx=((ii-1)*length(fliplatindex))+jj
     jcoord=length(precip$fliplatvals)-fliplatindex[jj]+1
     Longitude[stckindx]=precip$lonvals[lonindex[ii]]
     Latitude[stckindx]=precip$fliplatvals[fliplatindex[jj]]
    }
   }
   zCF=array(NA,dim=c(npts,nCF));zF=array(NA,dim=c(npts,nF))

#Have left this bit here, but it doesn't work any more ...
#    varlvl=menu(paste("Select dimension [,,",1:var2plot$varsize[3],",] of ",var2plot$varname,sep=""))
#    varlvltxt=paste("[,",varlvl,"]",sep="")

   firstexistentfile=FALSE
   choice8=menu(c(varsinncfile,"Inverted precip","Inverted Dunne runoff","SPI","Inverted SPI","Inverted sthu (gridbox unfrozen moisture content of each soil layer as a fraction of saturation)","Inverted (P-ET), i.e. (ET-P)","Inverted (P/ET), i.e. (ET/P), the reciprocal of UNEP's aridity index http://en.wikipedia.org/wiki/Aridity_index","(ET+runoff-P), i.e. -storage (runoff=Q) or the loss rate of water from the soil","ET","Storage (=P-ET-runoff =((effective precipitation)-runoff))","-ET"))
  }
  if (choice8<=length(varsinncfile)) {
   var2plot=readinfromnc(netcdffile,choice8)
  }
  if (choice8==(length(varsinncfile)+1)) {
   precip=readinfromnc(netcdffile,10)
   precip$datain=ifelse(precip$datain>=0.01/(60*60*24),precip$datain,0.01/(60*60*24))	#I did find some negative values for precip that I don't think should be there (and I have to exclude 0)
   tmp=0	#If I have tmp=max(precip$datain,na.rm=TRUE) then it will use a different max for each ensemble which I don't think is legitimate
   var2plot=precip	#Just to set framework and units, not the values
   var2plot$datain=ifelse(precip$datain>=0,tmp-precip$datain,tmp)
   var2plot$varname=paste("Inverted precip (= ",tmp,"-precip)",sep="")
  }
  if (choice8==(length(varsinncfile)+2)) {
   dunnerunoff=readinfromnc(netcdffile,23)	#Saturation excess surface ('Dunne') dunnerunoff (sat_excess_roff in kg m-2 s-1)
   var2plot=dunnerunoff	#Just to set framework and units, not the values
   var2plot$datain=-dunnerunoff$datain
   var2plot$varname="Inverted Dunne runoff (=-sat_excess_roff)"
  }
  if (choice8==(length(varsinncfile)+3) || choice8==(length(varsinncfile)+4)) {	#SPI or inverted SPI (here I am using the TAMSAT data so my instinct is to move to the higher resolution, but in the other menu options I have used the grid from the JULES output files so actually it's better to do that for consistency.
   precip=readinfromnc(netcdffile,10)
   albedo=readinfromnc(netcdffile,33)	#I am using this as a land mask to cut out problematic near-coastal points from TAMSAT (they're hidden from the plot by the fill of the sea, but if I nominate a box to contain near-coastal points then they'll be included in the average for the box and I don't want that).
   precip$datain=ifelse(precip$datain>=0.01/(60*60*24),precip$datain,0.01/(60*60*24))	#I did find some negative values for precip that I don't think should be there (and I have to exclude 0)
   lonindex=1:length(precip$lonvals);fliplatindex=1:length(precip$fliplatvals)
   lonindex=which(precip$lonvals>=lonmin & precip$lonvals<=lonmax)
   fliplatindex=which(precip$fliplatvals>=latmin & precip$fliplatvals<=latmax)
   spi=precip;tamsatmeanJOG=precip;tamsatsdJOG=precip	#Just to set framework and units, not the values (JOG = JULES output grid, i.e. the same grid as in the netcdf files being read in)
   spi$datain=array(NA,dim=precip$varsize);tamsatmeanJOG$datain=array(NA,dim=precip$varsize);tamsatsdJOG$datain=array(NA,dim=precip$varsize)	#Blank the values
   for (ii in 1:length(lonindex)) {
    for (jj in 1:length(fliplatindex)) {
     stckindx=((ii-1)*length(fliplatindex))+jj
     if (ii==1) {down=precip$lonvals[lonindex[ii]]-(precip$lonvals[lonindex[ii+1]]-precip$lonvals[lonindex[ii]])} else {down=precip$lonvals[lonindex[ii-1]]}
     mid=precip$lonvals[lonindex[ii]]
     if (ii==length(lonindex)) {up=precip$lonvals[lonindex[ii]]+(precip$lonvals[lonindex[ii]]-precip$lonvals[lonindex[ii-1]])} else {up=precip$lonvals[lonindex[ii+1]]}
     lb=mean(c(down,mid),na.rm=TRUE);ub=mean(c(mid,up),na.rm=TRUE)
     iitam=which(tamsatlons>=lb & tamsatlons<=ub)
     if (jj==1) {down=precip$fliplatvals[fliplatindex[jj]]-(precip$fliplatvals[fliplatindex[jj+1]]-precip$fliplatvals[fliplatindex[jj]])} else {down=precip$fliplatvals[fliplatindex[jj-1]]}
     mid=precip$fliplatvals[fliplatindex[jj]]
     if (jj==length(fliplatindex)) {up=precip$fliplatvals[fliplatindex[jj]]+(precip$fliplatvals[fliplatindex[jj]]-precip$fliplatvals[fliplatindex[jj-1]])} else {up=precip$fliplatvals[fliplatindex[jj+1]]}
     lb=mean(c(down,mid),na.rm=TRUE);ub=mean(c(mid,up),na.rm=TRUE)
     jjtam=which(tamsatlats>=lb & tamsatlats<=ub)
     jcoord=length(precip$fliplatvals)-fliplatindex[jj]+1
     for (mnth in 1:nmnths) {
      mr=mean(as.vector(tamsatmnthlymean[iitam,jjtam,mnth]),na.rm=TRUE)	#TAMSAT precip values in mm/mo
      sr=mean(as.vector(tamsatmnthlysd[iitam,jjtam,mnth]),na.rm=TRUE)
      modprecip=precip$datain[lonindex[ii],length(precip$fliplatvals)-fliplatindex[jj]+1,mnth]*60*60*24*30	#Modelled precip in mm/mo
      if (is.finite(sr) && sr>(1e-4) && !is.na(albedo$datain[lonindex[ii],length(precip$fliplatvals)-fliplatindex[jj]+1,mnth])) {
       if (flagrecalc==TRUE) {
        mr=prpmeans[lonindex[ii],length(precip$fliplatvals)-fliplatindex[jj]+1]/3
        sr=prpstds[lonindex[ii],length(precip$fliplatvals)-fliplatindex[jj]+1]/3
       }
       spi$datain[lonindex[ii],length(precip$fliplatvals)-fliplatindex[jj]+1,mnth]=(modprecip-mr)/sr	#Definition of SPI (applied to each month)
#       if (spi$datain[lonindex[ii],length(precip$fliplatvals)-fliplatindex[jj]+1,mnth]>500) {cat("ii=",ii,",jj=",jj,"(",precip$fliplatvals[jj],"degN, ",precip$lonvals[ii],"degE) has\n\tprecip=",modprecip,",mr=",mr,",sr=",sr," therefore SPI=",spi$datain[lonindex[ii],length(precip$fliplatvals)-fliplatindex[jj]+1,mnth],"\n")}
       spi$datain[lonindex[ii],length(precip$fliplatvals)-fliplatindex[jj]+1,mnth]=min(c(5,max(c(-5,spi$datain[lonindex[ii],length(precip$fliplatvals)-fliplatindex[jj]+1,mnth]))))	#If precip is displaced slightly in the model from e.g. the Ethiopian Highlands to the Kenya desert - which seems to happen often - then you'll easily get SPI values around 500-600 but I think better for visualisation to restrict it to -5 to +5.
      }
      tamsatmeanJOG$datain[lonindex[ii],length(precip$fliplatvals)-fliplatindex[jj]+1,mnth]=mr
      tamsatsdJOG$datain[lonindex[ii],length(precip$fliplatvals)-fliplatindex[jj]+1,mnth]=sr
     }
    }
   }
   var2plot=precip	#Just to set framework and units, not the values
   if (choice8==(length(varsinncfile)+3)) {
    var2plot$datain=spi$datain
    var2plot$varname="SPI"
   } else {
    var2plot$datain=-spi$datain
    var2plot$varname="-SPI"
   }
   var2plot$varunits="SDs"
  }
  if (choice8==(length(varsinncfile)+5)) {
   dontseemtoneedtoselectshowlayer=TRUE	#Spotted Aug 2017: why did I need to code the =FALSE bit before?
   if (dontseemtoneedtoselectshowlayer) {
    sthu=readinfromnc(netcdffile,41)	#Gridbox unfrozen moisture content of each soil layer as a fraction of saturation (sthu in NoUnitsGiven)
    var2plot=sthu	#Just to set framework and units, not the values
    var2plot$datain=-sthu$datain
    var2plot$varname="Inverted sthu at surface (= -sthu[1])"
   } else {
    sthu=readinfromnc(netcdffile,41)	#Gridbox unfrozen moisture content of each soil layer as a fraction of saturation (sthu in NoUnitsGiven)
    var2plot=sthu	#Just to set framework and units, not the values
    showlayer=1	#Select soil layer 1 (from 1-4)
    var2plot$datain=-sthu$datain[,,showlayer,]
    var2plot$varsize=c(var2plot$varsize[1],var2plot$varsize[2],1,var2plot$varsize[4])
    var2plot$varname="Inverted sthu at surface (= -sthu[1] ; n.b. NEGATIVE NUMBERS!!!)"
   }
  }
  if (choice8==(length(varsinncfile)+6)) {
   precip=readinfromnc(netcdffile,10)
   evap=readinfromnc(netcdffile,31)
   transp=readinfromnc(netcdffile,32)
   precip$datain=ifelse(precip$datain>=0.01/(60*60*24),precip$datain,0.01/(60*60*24))	#I did find some negative values for precip that I don't think should be there (and I have to exclude 0)
   evap$datain=ifelse(evap$datain>=0.01/(60*60*24),evap$datain,0.01/(60*60*24))
   transp$datain=ifelse(transp$datain>=0.01/(60*60*24),transp$datain,0.01/(60*60*24))
   var2plot=evap	#Just to set framework and units, not the values
   var2plot$datain=(evap$datain+transp$datain)-precip$datain
   var2plot$varname="ET-P (=(esoil+ecan)-precip)"
  }
  if (choice8==(length(varsinncfile)+7)) {
   precip=readinfromnc(netcdffile,10)
   evap=readinfromnc(netcdffile,31)
   transp=readinfromnc(netcdffile,32)
   precip$datain=ifelse(precip$datain>=0.01/(60*60*24),precip$datain,0.01/(60*60*24))	#I did find some negative values for precip that I don't think should be there (and I have to exclude 0)
   evap$datain=ifelse(evap$datain>=0.01/(60*60*24),evap$datain,0.01/(60*60*24))
   transp$datain=ifelse(transp$datain>=0.01/(60*60*24),transp$datain,0.01/(60*60*24))
   var2plot=evap	#Just to set framework and units, not the values
   var2plot$datain=(evap$datain+transp$datain)/precip$datain
   var2plot$varname="ET/P (=(esoil+ecan)/precip)"
  }
  if (choice8==(length(varsinncfile)+8)) {
   precip=readinfromnc(netcdffile,10)
   evap=readinfromnc(netcdffile,31)
   transp=readinfromnc(netcdffile,32)
   runoff=readinfromnc(netcdffile,20)
   precip$datain=ifelse(precip$datain>=0.01/(60*60*24),precip$datain,0.01/(60*60*24))	#I did find some negative values for precip that I don't think should be there (and I have to exclude 0)
   evap$datain=ifelse(evap$datain>=0.01/(60*60*24),evap$datain,0.01/(60*60*24))
   transp$datain=ifelse(transp$datain>=0.01/(60*60*24),transp$datain,0.01/(60*60*24))
   runoff$datain=ifelse(runoff$datain>=0.01/(60*60*24),runoff$datain,0.01/(60*60*24))
   var2plot=evap	#Just to set framework and units, not the values
   var2plot$datain=(evap$datain+transp$datain)+runoff$datain-precip$datain
   var2plot$varname="-Storage ((esoil+ecan)+runoff-precip)"
  }
  if (choice8==(length(varsinncfile)+9)) {	#I haven't done a full test, but it does seem like the value 1/4320000 is being used as a value for 'sea flux' ...
   evap=readinfromnc(netcdffile,31)
   transp=readinfromnc(netcdffile,32)
   evap$datain=ifelse(evap$datain>=0.01/(60*60*24),evap$datain,0.01/(60*60*24))
   transp$datain=ifelse(transp$datain>=0.01/(60*60*24),transp$datain,0.01/(60*60*24))
   var2plot=evap	#Just to set framework and units, not the values
   var2plot$datain=evap$datain+transp$datain
   var2plot$varname="Evapotranspiration"	#ET=esoil+ecan
  }
  if (choice8==(length(varsinncfile)+10)) {
   precip=readinfromnc(netcdffile,10)
   evap=readinfromnc(netcdffile,31)
   transp=readinfromnc(netcdffile,32)
   runoff=readinfromnc(netcdffile,20)
   precip$datain=ifelse(precip$datain>=0.01/(60*60*24),precip$datain,0.01/(60*60*24))	#I did find some negative values for precip that I don't think should be there (and I have to exclude 0)
   evap$datain=ifelse(evap$datain>=0.01/(60*60*24),evap$datain,0.01/(60*60*24))
   transp$datain=ifelse(transp$datain>=0.01/(60*60*24),transp$datain,0.01/(60*60*24))
   runoff$datain=ifelse(runoff$datain>=0.01/(60*60*24),runoff$datain,0.01/(60*60*24))
   var2plot=precip	#Just to set framework and units, not the values
   var2plot$datain=precip$datain-(evap$datain+transp$datain)-runoff$datain
   var2plot$varname="Storage (=precip-(esoil+ecan)-runoff)"
  }
  if (choice8==(length(varsinncfile)+11)) {
   evap=readinfromnc(netcdffile,31)
   transp=readinfromnc(netcdffile,32)
   evap$datain=ifelse(evap$datain>=0.01/(60*60*24),evap$datain,0.01/(60*60*24))
   transp$datain=ifelse(transp$datain>=0.01/(60*60*24),transp$datain,0.01/(60*60*24))
   var2plot=evap	#Just to set framework and units, not the values
   var2plot$datain=-(evap$datain+transp$datain)
   var2plot$varname="(-Evapotranspiration)"
  }

  if (length(var2plot$varsize)>3) {stop("Can't currently print variables with several layers (e.g. STHU): I've made some kind of error with varlvl and varlvltxt ...")}
  source("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/setmultiplier.r")
  cat("Selected:",var2plot$varname," in ",var2plot$varunits,"\n")
#  print(str(var2plot))
#  cat("\n\tDimensions of data to be plotted:")
#  cat(str(var2plot$datain))

  zvals=matrix(NA,nrow=npts,ncol=ntpts)	#Idea is that I'll be able to plot this out using levelplot(zvals[,1]~Longitude+Latitude)
  zvalstavg=rep(NA,times=npts)
#Take the time mean (e.g. over the Long Rains) of var2plot and put it in zvalstavg
  tmp2=ifelse(is.finite(var2plot$datain),var2plot$datain,NA)
  tmp2=ifelse(tmp2<(-990),NA,ifelse(tmp2>1e6,NA,tmp2))
  for (ii in 1:length(lonindex)) {
   for (jj in 1:length(fliplatindex)) {
    stckindx=((ii-1)*length(fliplatindex))+jj
    jcoord=length(var2plot$fliplatvals)-fliplatindex[jj]+1
    for (tpt in mnthrange[[plotpervar]]) {	#For these files, 1:ntpts would be months 1:12
     zvals[stckindx,tpt]=tmp2[lonindex[ii],jcoord,tpt]
#     maintxt=paste(var2plot$varname," in ",var2plot$varunits," at ",var2plot$vardims[length(var2plot$vardims)],"=",tpt,sep="")
    }
   }
  }
  zvals=ifelse(is.finite(zvals),zvals,NA)
  for (ii in 1:length(lonindex)) {
   for (jj in 1:length(fliplatindex)) {
    stckindx=((ii-1)*length(fliplatindex))+jj
    jcoord=length(var2plot$fliplatvals)-fliplatindex[jj]+1
    zvalstavg[stckindx]=mean(zvals[stckindx,mnthrange[[plotpervar]]],na.rm=TRUE)
   }
  }
  zvalstavg=ifelse(is.finite(zvalstavg),zvalstavg,NA)

  minvals=rep(NA,times=ntpts);maxvals=rep(NA,times=ntpts);meanvals=rep(NA,times=ntpts)
  for (tpt in 1:ntpts) {	#Spatial averages
   minvals[tpt]=min(as.vector(zvals[,tpt]),na.rm=TRUE)
   maxvals[tpt]=max(as.vector(zvals[,tpt]),na.rm=TRUE)
   meanvals[tpt]=max(as.vector(zvals[,tpt]),na.rm=TRUE)
  }

#  if (length(which(var2plot$varsize>1))>3) {	#With all my files of this type, when there are 4 dimensions (e.g. field203 with vardims=c("longitude0","latitude0","z0","time0"), EITHER the 3rd one is only of length 1 and therefore datain will only have 3 dimensions and tpt will correspond to the final one OR I need to take a cross-section.
#   var2plot$datain=var2plot$datain[,,varlvl,]
#   cat("Plotting",var2plot$vardims[3],"=",varlvl,"cross-section\n")
#  }
  if (ensmem<=nCF) {zCF[,ensmem]=zvalstavg} else {zF[,ensmem-nCF]=zvalstavg}
 }
}
vartxt=var2plot$varname;placetxt=""
if (vartxt=="precip") {vartxt="Precipitation"}
if (vartxt=="t1p5m_gb") {vartxt="Temperature at 1.5 m height"}
if (vartxt=="ftl_gb") {vartxt="Surface sensible heat flux"}
if (vartxt=="q1p5m_gb") {vartxt="Specific humidity at 1.5 m height"}
if (vartxt=="sthu") {vartxt="Unfrozen soil moisture content"}

#Set the thresholds for the return time plots on the last of the ensemble members (not really worth parsing the whole ensemble: it'll be negligibly different)
if (choice6==1) {
 rtts=10^seq(from=min(log10(zvalstavg),na.rm=TRUE),to=max(log10(zvalstavg),na.rm=TRUE),length.out=accuracy)
} else {
 rtts=seq(from=min(zvalstavg,na.rm=TRUE),to=max(zvalstavg,na.rm=TRUE),length.out=accuracy)
 rtts=c(min(zvalstavg,na.rm=TRUE),rtts,max(zvalstavg,na.rm=TRUE))	#Somehow, the seq above misses off the end points so I need this too
}
nthr=length(rtts)	#n.b. this is the no. of vertical slices in the output return-time plot
