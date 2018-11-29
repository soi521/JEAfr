
newmemlimit=8191        #The default memory limit is probably 5922Mb. My machine allows it to be changed up to any amount, though the man page says there's a limit of 8 Tb (=8192 Mb)
cat("Changing memory limit from",memory.limit(),"Mb to",newmemlimit,"Mb (only has an effect on Windows R, so if you are on Linux/UNIX just ignore the \"'memory.limit()' is Windows-specific\" warnings this generates).\n")
memory.limit(size=newmemlimit)

library(ncdf4);library(lattice);library(mapdata);library(sp);library(rgeos)
load("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/FilesCanDeleteButMakeScriptRunFaster/bordercalcs.RData")
source("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/BAMSplots.r")
load("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/52_10.RData")

domarmap=TRUE;plotcountries=TRUE;ncoe2o=FALSE;showallcells=FALSE;plotbrokenlines=TRUE

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

vartxt="ET"
fname="gleam_data/E_2014LongRains_GLEAM_v3.2a.nc"	#I created this file by (i) downloading GLEAM data, (ii) ncks -d time,60,151 E_2014_GLEAM_v3.2a.nc E_2014LongRains_GLEAM_v3.2a.nc and then (iii) ncwa -O -a time E_2014LongRains_GLEAM_v3.2a.nc E_2014LongRains_GLEAM_v3.2a.nc

#source("D:/D.DRIVE/PROJECTS/projACE-Africa/JULES_EAfr_paper/BAMSplots.r")
source("D:/D.DRIVE/programs/myncutils.r")	#*************************** Check that flag1=1 in the definition of readinfromnc in here
varsinncfile=detailsnc(fname)

varaet=readinfromnc(fname,1)
	#n.b. t(varaet$datain[1:1440,400:1]) is now an array where row 1 is the northmost and col 1 is the westmost (i.e. varaet$datain actually has latitudes flipped and the whole grid transposed)
aetvals=ifelse(varaet$datain>50,50,varaet$datain)
aetvals=ifelse(aetvals<0,0,aetvals)
levelplot(log10(aetvals),at=seq(from=-2.5,to=2.5,length.out=10+1),col.regions=colorRampPalette(c("red","red2","darkblue","blue","green","green4","yellow2","yellow")))	#Maximise this one on the laptop screen
#above=sum(ifelse(aetvals>=1,1,0),na.rm=TRUE);below=sum(ifelse(aetvals<1,1,0),na.rm=TRUE)
#cat("aet values are ",100*above/(above+below),"% above and ",100*below/(above+below),"% below the line\n")

npts=length(varaet$lonvals)*length(varaet$fliplatvals)
aetvals1=rep(NA,times=npts);Longitude=rep(NA,times=npts);Latitude=rep(NA,times=npts)
for (ii in 1:length(varaet$lonvals)) {
 for (jj in 1:length(varaet$fliplatvals)) {
  stckindx=((ii-1)*length(varaet$fliplatvals))+jj
  jcoord=length(varaet$fliplatvals)-jj+1
  Longitude[stckindx]=varaet$lonvals[ii]
  Latitude[stckindx]=varaet$fliplatvals[jj]
  aetvals1[stckindx]=aetvals[ii,length(varaet$fliplatvals)-jj+1]*365.25/12	#aetvals in mm/day averaged over the Long Rains Mar-May, but aetvals1 in mm/mo as I used in my Fig. 4
 }
}

#aetvals1=ifelse(aetvals1>150,150,aetvals1)
minval=0;maxval=max(aetvals1,na.rm=TRUE)
ncols=30
dev.new(width=7.88,height=5,res=72,units="in")	#Do this command once and drag it to full height (R seems to ignore the height option ...)
cpal=colorRampPalette((c("red4","red","darkorange1","yellow","chartreuse","green","green4","green4")))
print(levelplot(aetvals1~Longitude*Latitude,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main="",sub=paste("Units=mm/mo (mean over Long Rains)",sep=""),asp=1,xlim=c(lonmin,lonmax),ylim=c(latmin,latmax)))

#This plot was wrong: Can't quite remember but I think it's the baseline average aggregated to larger pixels
#obsprecip=LongRainsTAMvec*60*60*24*(365.25/12)	#LongRainsTAMvec is in mm/s so obsprecip is in mm/mo
#dev.new(width=7.88,height=5,res=72,units="in")	#Do this command once and drag it to full height (R seems to ignore the height option ...)
#print(levelplot(obsprecip~lonsTAMvec*latsTAMvec,at=seq(from=minval,to=195,length.out=ncols),col.regions=cpal,panel=pnl,main="",sub=paste("Units=mm/mo (mean over Long Rains)",sep=""),asp=1,xlim=c(lonmin,lonmax),ylim=c(latmin,latmax)))

#TAMSAT obs for 2014
obsprecip=LongRainsTAM/3
minval=0;maxval=max(obsprecip,na.rm=TRUE)
dev.new(width=7.88,height=5,res=72,units="in")	#Do this command once and drag it to full height (R seems to ignore the height option ...)
print(levelplot(obsprecip,row.values=tamsatlons,column.values=tamsatlats,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,main="",asp=1))

#JULES AET predictions from factual runs:
   npts=length(lonindex)*length(fliplatindex)
   Lon111=rep(NA,times=npts);Lat111=rep(NA,times=npts)
   for (ii in 1:length(lonindex)) {
    for (jj in 1:length(fliplatindex)) {
     stckindx=((ii-1)*length(fliplatindex))+jj
     jcoord=length(precip$fliplatvals)-fliplatindex[jj]+1
     Lon111[stckindx]=precip$lonvals[lonindex[ii]]
     Lat111[stckindx]=precip$fliplatvals[fliplatindex[jj]]
    }
   }
aetF=rep(NA,times=2040);aetFsd=rep(NA,times=2040)
for (lp in 1:2040) {
 aetF[lp]=mean(yyF[lp,],na.rm=TRUE)*60*60*24*365.25/12	#AET from JULES runs, mean across 100 factual ensemble members
 aetFsd[lp]=sd(yyF[lp,],na.rm=TRUE)*60*60*24*365.25/12
}
dev.new(width=7.88,height=5,res=72,units="in")	#Do this command once and drag it to full height (R seems to ignore the height option ...)
minval=min(aetF,na.rm=TRUE);maxval=max(aetF,na.rm=TRUE)
print(levelplot(aetF~Lon111*Lat111,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,asp=1))
df45=data.frame(aetF,Lon111,Lat111,aetFsd)






#stop()
print(Sys.time())	#WARNING: this loop takes 2.5 hours
#tamsatlons=tamsatlons[1:30];tamsatlats=tamsatlats[1:30]

#Generate Scatter plot of precip obs versus aet obs:
precips=rep(NA,times=length(tamsatlons)*length(tamsatlats))
aets=rep(NA,times=length(tamsatlons)*length(tamsatlats))
aetsJ=rep(NA,times=length(tamsatlons)*length(tamsatlats))
aetsJsd=rep(NA,times=length(tamsatlons)*length(tamsatlats))
yyFmatched=array(NA,dim=c(length(tamsatlons)*length(tamsatlats),100))
yyCFmatched=array(NA,dim=c(length(tamsatlons)*length(tamsatlats),100))
regs99=rep("",times=length(tamsatlons)*length(tamsatlats))
colrs99=rep("",times=length(tamsatlons)*length(tamsatlats))
aetvalsdf=data.frame(aetvals1,Longitude,Latitude)
for (ii in 1:length(tamsatlons)) {	#301:305 is a good test because those values are in KN, EO and KC
 cat("Doing ",ii," of ",length(tamsatlons),"\n")
 for (jj in 1:length(tamsatlats)) {
  stckindx=((ii-1)*length(tamsatlats))+jj
  precips[stckindx]=obsprecip[ii,jj]	#The TAMSAT precip
  loncellcentre=tamsatlons[ii];latcellcentre=tamsatlats[jj]

  closestETobs=subset(aetvalsdf,abs(Latitude-latcellcentre)<0.26 & abs(Longitude-loncellcentre)<0.26)
  aets[stckindx]=mean(closestETobs$aetvals1,na.rm=TRUE)	#Mean of the closest match ET points from GLEAM

#Do you take the ensemble mean and then the spatial mean?....
  closestETjules=subset(df45,abs(Lat111-latcellcentre)<0.26 & abs(Lon111-loncellcentre)<0.26)
  aetsJ[stckindx]=mean(closestETjules$aetF,na.rm=TRUE)	#Mean of the closest match ET points from JULES
  aetsJsd[stckindx]=mean(closestETjules$aetFsd,na.rm=TRUE)

#...or do you instead take the spatial mean for each ensemble member and take the ensemble mean at the end? I think it's more correct to do the latter actually, even though (a) it's generally much more memory-intensive because you have to take all the ensemble members through all the calculations and (b) very annoyingly it usually means you can't use the subset command which makes it more fiddly.
  kk=(abs(df45$Lat111-latcellcentre)<0.26 & abs(df45$Lon111-loncellcentre)<0.26)
  for (ensmem in 1:100) {
   yyFmatched[stckindx,ensmem]=mean(yyF[which(kk),ensmem],na.rm=TRUE)*60*60*24*365.25/12
   yyCFmatched[stckindx,ensmem]=mean(yyCF[which(kk),ensmem],na.rm=TRUE)*60*60*24*365.25/12
  }

    col99="";reg99=""
#Code from specifyareas.r (should really hive this off into its own function sometime ...)
    inkenya=point.in.polygon(loncellcentre,latcellcentre,mapkenya$x,mapkenya$y)
    insomalia=point.in.polygon(loncellcentre,latcellcentre,mapsomalia$x,mapsomalia$y)
    inethiopia=point.in.polygon(loncellcentre,latcellcentre,mapethiopia$x,mapethiopia$y)
    inuganda=point.in.polygon(loncellcentre,latcellcentre,mapuganda$x,mapuganda$y)
    inrwanda=point.in.polygon(loncellcentre,latcellcentre,maprwanda$x,maprwanda$y)
    insudan=point.in.polygon(loncellcentre,latcellcentre,mapsudan$x,mapsudan$y)	#This is pre-2011 Sudan
    intanzania=point.in.polygon(loncellcentre,latcellcentre,maptanzania$x,maptanzania$y)
    inlakevic=point.in.polygon(loncellcentre,latcellcentre,maplakevic$x,maptanzania$y)
    notsea=(inkenya || insomalia || inethiopia || inuganda || inrwanda || intanzania || insudan) && (!inlakevic)	#I know my bounding box doesn't include Yemen etc. so don't need to exclude them
    if (notsea && latcellcentre<=12.4962
 && (latcellcentre<=(14.97967*loncellcentre-492.686)	#almost north-south line from (3.88545,33.14968) to (1.91053,33.01784)
 || latcellcentre<=(0.506926*loncellcentre-14.8271))	#line from (1.91053,33.01784) to (0.32886,29.89772)
 && latcellcentre<=(0.974231*loncellcentre-28.41)	#line from (5.81203,35.12722) to (3.88545,33.14968)
 && (latcellcentre<=(1.200313*loncellcentre-41.4036)	#line from (10.94047,43.60866) to (7.77558,40.97194)
 || latcellcentre<=(0.335953*loncellcentre-5.98906))	#line from (7.77558,40.97194) to (5.81203,35.12722)
 && latcellcentre<=(16.49522*loncellcentre-492.841)	#line from (0.32886,29.89772) to (-2.57067,29.72194)
 && latcellcentre>=(-0.23675*loncellcentre+4.465948))	#line from (-2.57067,29.72194) to (-5.02601,40.09304)
 {	#Condition here is whether or not this cell is in or close to the bimodal rainfall zone
     cellnum=cellnum+1
#     cat("Cell ",cellnum,": EW ",divsEW[ii],"-",divsEW[ii+1],", NS ",divsNS[jj],"-",divsNS[jj+1],"\n")
     latmins=c(latmins,divsNS[jj])
     latmaxs=c(latmaxs,divsNS[jj+1])
     lonmins=c(lonmins,divsEW[ii])
     lonmaxs=c(lonmaxs,divsEW[ii+1])
     MMs=c(MMs,1)	#For now, modal month in all cells is Jan (had thought for long time to use this, but now not)

     col99="grey";reg99="ND"
#In reg99, ND = No data
#SP = Somalia (Puntland and Somaliland)
#SN = Somalia N of Mogadishu but south of Puntland
#SS = Southern Somalia (Mogadishu and south)
#EO = Ethiopia/Oromia
#ES = Ethiopia/Somali
#IL = the Ilemi Triangle in South Sudan and Ethiopia/South Omo
#KN = Kenya north of a line from Kolbio to Kitale and east of a line from Mt Kenya to Lake Turkana
#KT = Kenya north of a line from Kolbio to Kitale and west of a line from Mt Kenya to Lake Turkana
#KW = Kenya south and west of Mt Kenya
#(Within Kenya, KT and KW are almost exactly the wetter savanna types and KN and KC are the drier savanna (inc. bushland and thicket) shown on Kiage & Liu 2006:Fig.4, by the way)
#KC = Kenya from Mt Kenya south and east to the coast
#UW = Uganda West of Kampala (western half of Lake Vic)
#UE = Uganda East of Kampala (eastern half of Lake Vic)
#TW = Tanzania West of Mwanza (western half of Lake Vic)
#TM = Mid Tanzania from Mt Kilimanjaro west to the middle of Lake Vic
#TC = Tanzania from opp. Nairobi to coast (however, only one single point from this area is in my bimodal rainfall zone so can forget this area)
     if (latcellcentre>(13.52449-0.36396*loncellcentre)) {	#A line from the northern end of Lake Albert (2.11915,31.33693) through Mt Kenya to the southernmost tip of Somalia (-1.61551,41.59816)
      col99="cadetblue2";reg99="KN"
      if (inethiopia) {
       col99="dodgerblue";reg99="EO"
       if (latcellcentre<=(4.058757*loncellcentre-162.063)) {	#Approx. the border between Ethiopia/Oromia and Ethiopia/Somali
        col99="green4";reg99="ES"
       }
      }
      if (insomalia && latcellcentre<=4.04985) {col99="black";reg99="SS"}
      if (insomalia && latcellcentre>4.04985) {
       if (latcellcentre<=7.2) {col99="orange";reg99="SN"} else {col99="firebrick3";reg99="SP"}
      }
      if (latcellcentre>=4.48260 && latcellcentre>=(1.239970745*loncellcentre-40.31378592)) {	#Approx. the Great Rift Valley in Ethiopia from (4.48260,36.12697) at Lake Turkana on the border to (11.48469,41.77395) near Djibouti
       col99="peru";reg99="IL"
      }
      if (latcellcentre<=((-3.355068717*loncellcentre)+125.6910669)) {
       if (inuganda) {col99="forestgreen";reg99="UE"} else {col99="coral2";reg99="KT"}
      }
     } else {
      if (loncellcentre<37.5) {	#Roughly Kilimanjaro and west to western border of Rwanda, i.e. part of the 'East African Plateau'
       col99="forestgreen";reg99="UE"
       if (inkenya) {col99="darkgoldenrod1";reg99="KW"}
       if (intanzania) {col99="gray49";reg99="TM"}
       if (latcellcentre<=(80.50349-2.24426*loncellcentre) && latcellcentre>(1.676435*loncellcentre-56.7208)) {	#Uganda or Rwanda or TW (these 2 straight lines more or less match the curve of the E Uganda border)
        if (loncellcentre<32.6) {	#Western half of Lake Vic
         if (inrwanda) {col99="burlywood";reg99="RW"}
         if (inuganda) {col99="darkolivegreen3";reg99="UW"}
         if (intanzania) {col99="darkorchid2";reg99="TW"}
        }
       }
      } else {	#Roughly the Mt Kenya to Coast area (S of where Somalia border meets the coast)
       if (inkenya) {col99="bisque2";reg99="KC"}
       if (intanzania) {col99="firebrick2";reg99="TC"}
      }
     }
    }
#End of code from specifyareas.r
  regs99[stckindx]=reg99
  colrs99[stckindx]=col99
 }
}
rm(aetvalsdf)
regions=c("IL","TW","TM","SP","SN","ES","SS","EO","KN","KC","KT","KW","UE","RW","UW")	#From regions=unique(regs99) but reordered to make the wet regions plot last (use this for my scatterplots)
regions=c("RW","UW","TW","TM","UE","KT","KW","IL","KN","EO","KC","SS","ES","SN","SP")	#From regions=unique(regs99) but following the order I use in the paper (use this for generating Table 2 only)
yyFmatchedSmeans=array(NA,dim=c(length(regions),100))
yyCFmatchedSmeans=array(NA,dim=c(length(regions),100))

scdivEthenS=(aetsJ-aets)/aetsJsd	#Calculate this on a pixel by pixel basis before spatial averaging in the loop below.

scatterplotdf1=data.frame(precips,aets,aetsJ,regs99,colrs99,aetsJsd,scdivEthenS)
scatterplotdf=subset(scatterplotdf1,aetsJ>2 & precips>1.5 & aets>1)

maxaet=150	#=max(c(aets,aetsJ),na.rm=TRUE)
dev.new(width=6.3,height=5,res=72,units="in")
with(plot(x=precips,y=aets,xlab="Precipitation in mm/mo",ylab="GLEAM AET in mm/mo",type="n",xlim=c(0,max(precips,na.rm=TRUE)),ylim=c(0,maxaet),las=1,bty="l"),data=scatterplotdf)
cat("Table of values (EthenS means I took the ensemble mean before spatial mean (EthensdS means spatial SD of ensemble mean), SthenE the reverse):\n\tA=Spatial mean of GLEAM AET values, B=EthenS of JULES AET values (F ensemble), C=scdivEthenS(=(JULES AET - GLEAM AET)/(Ensemble SD of JULES AET values)) (F ensemble), D=SthenE of JULES AET values from F and CF ensembles, E=scdivSthenE (F ensemble)\n")
for (lp in 1:length(regions)) {
 loop=regions[lp]
 ss=subset(scatterplotdf,regs99==loop)
 with(points(x=precips,y=aets,pch=16,col=as.character(ss$colrs99[1])),data=ss)

 kk=(regs99==loop)
 for (ensmem in 1:100) {
  yyFmatchedSmeans[lp,ensmem]=mean(yyFmatched[which(kk),ensmem],na.rm=TRUE)	#Spatial mean for each ensemble member
  yyCFmatchedSmeans[lp,ensmem]=mean(yyCFmatched[which(kk),ensmem],na.rm=TRUE)
 }
 scdivSthenE=(mean(yyFmatchedSmeans[lp,],na.rm=TRUE)-mean(ss$aets,na.rm=TRUE))/sd(yyFmatchedSmeans[lp,],na.rm=TRUE)
 cat("For region",as.character(ss$regs99[1]),": A=",mean(ss$aets,na.rm=TRUE),", B=",mean(ss$aetsJ,na.rm=TRUE),", C=",mean(ss$scdivEthenS,na.rm=TRUE),", D(F)=",mean(yyFmatchedSmeans[lp,],na.rm=TRUE),", D(CF)=",mean(yyCFmatchedSmeans[lp,],na.rm=TRUE),", E=",scdivSthenE,"\n")
}
ss=subset(scatterplotdf,regs99!="" & regs99!="ND" & regs99!="TC")
fit1=with(lm(aets~precips),data=ss)
abline(fit1,lty=2)
abline(a=0,b=1,lty=1)
cat("Rule of thumb for GHA bimodal seasonality area is (aet in mm/mo)=(",fit1$coefficients[2],")*(precip in mm/mo)+(",fit1$coefficients[1],")\n")
summary(fit1)
Fvalue=summary(fit1)$fstatistic[1]
df1=summary(fit1)$fstatistic[2];df2=summary(fit1)$fstatistic[3]
pvalue=pf(Fvalue,df1,df2,lower.tail=FALSE)
if (pvalue<0.05) {
 cat("Fit is significant at the 5% level (p=",pvalue,", F test, F=",Fvalue,"on degrees of freedom ",df1," and ",df2,")\n")
} else {
 cat("Fit is not significant at the 5% level (p=",pvalue,", F test, F=",Fvalue,"on degrees of freedom ",df1," and ",df2,")\n")
}


dev.new(width=6.3,height=5,res=72,units="in")
with(plot(x=aets,y=aetsJ,xlab="GLEAM AET in mm/mo",ylab="JULES AET in mm/mo",type="n",xlim=c(0,maxaet),ylim=c(0,maxaet),las=1,bty="l"),data=scatterplotdf)
for (loop in regions) {
 ss=subset(scatterplotdf,regs99==loop)
 with(points(x=aets,y=aetsJ,pch=16,col=as.character(ss$colrs99[1])),data=ss)
}
ss=subset(scatterplotdf,regs99!="" & regs99!="ND" & regs99!="TC")
fit1=with(lm(aetsJ~aets),data=ss)
abline(fit1,lty=2)
abline(a=0,b=1,lty=1)
cat("Rule of thumb for GHA bimodal seasonality area is (aetsJ in mm/mo)=(",fit1$coefficients[2],")*(aets in mm/mo)+(",fit1$coefficients[1],")\n")
summary(fit1)
Fvalue=summary(fit1)$fstatistic[1]
df1=summary(fit1)$fstatistic[2];df2=summary(fit1)$fstatistic[3]
pvalue=pf(Fvalue,df1,df2,lower.tail=FALSE)
if (pvalue<0.05) {
 cat("Fit is significant at the 5% level (p=",pvalue,", F test, F=",Fvalue,"on degrees of freedom ",df1," and ",df2,")\n")
} else {
 cat("Fit is not significant at the 5% level (p=",pvalue,", F test, F=",Fvalue,"on degrees of freedom ",df1," and ",df2,")\n")
}

dev.new(width=6.3,height=5,res=72,units="in")
with(plot(x=precips,y=aetsJ,xlab="Precipitation in mm/mo",ylab="JULES AET in mm/mo",type="n",xlim=c(0,max(precips,na.rm=TRUE)),ylim=c(0,maxaet),las=1,bty="l"),data=scatterplotdf)
for (loop in regions) {
 ss=subset(scatterplotdf,regs99==loop)
 with(points(x=precips,y=aetsJ,pch=16,col=as.character(ss$colrs99[1])),data=ss)
}
ss=subset(scatterplotdf,regs99!="" & regs99!="ND" & regs99!="TC")
fit1=with(lm(aetsJ~precips),data=ss)
abline(fit1,lty=2)
abline(a=0,b=1,lty=1)
cat("Rule of thumb for GHA bimodal seasonality area is (aetsJ in mm/mo)=(",fit1$coefficients[2],")*(precip in mm/mo)+(",fit1$coefficients[1],")\n")
summary(fit1)
Fvalue=summary(fit1)$fstatistic[1]
df1=summary(fit1)$fstatistic[2];df2=summary(fit1)$fstatistic[3]
pvalue=pf(Fvalue,df1,df2,lower.tail=FALSE)
if (pvalue<0.05) {
 cat("Fit is significant at the 5% level (p=",pvalue,", F test, F=",Fvalue,"on degrees of freedom ",df1," and ",df2,")\n")
} else {
 cat("Fit is not significant at the 5% level (p=",pvalue,", F test, F=",Fvalue,"on degrees of freedom ",df1," and ",df2,")\n")
}

print(Sys.time())








stop()
#Now compare obs AET to JULES AET:

#for (ww in 1:npts) {
# ensmem=3	#1:nF
# yyF[ww,ensmem]
#}

   npts=length(lonindex)*length(fliplatindex)
   Lon111=rep(NA,times=npts);Lat111=rep(NA,times=npts)
   for (ii in 1:length(lonindex)) {
    for (jj in 1:length(fliplatindex)) {
     stckindx=((ii-1)*length(fliplatindex))+jj
     jcoord=length(precip$fliplatvals)-fliplatindex[jj]+1
     Lon111[stckindx]=precip$lonvals[lonindex[ii]]
     Lat111[stckindx]=precip$fliplatvals[fliplatindex[jj]]
    }
   }
aetF=yyF*60*60*24*365.25/12	#AET from JULES runs
dev.new(width=7.88,height=5,res=72,units="in")	#Do this command once and drag it to full height (R seems to ignore the height option ...)
minval=min(aetF,na.rm=TRUE);maxval=max(aetF,na.rm=TRUE)
print(levelplot(aetF~Lon111*Lat111,at=seq(from=minval,to=maxval,length.out=ncols),col.regions=cpal,panel=pnl,asp=1))

#arendyF=matrix(NA,nrow=length(areanamesu),ncol=nF)
#for (uu in 1:length(areanamesu)) {
# if (areanamesu[uu]!="TC") {
#  placetxt=paste(" for ",areanamesu[uu],sep="")
#  rownumvec=rownummap[uu,][is.finite(rownummap[uu,])]
#  for (ensmem in 1:nF) {
#   tmp=yyF[rownumvec,ensmem]*60*60*24*365.25/12
#   arendyF[uu,ensmem]=mean(tmp,na.rm=TRUE)
#  }
#  arendyF[!is.finite(arendyF)]=NA
# }
#}
