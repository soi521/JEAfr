  westbound=lonmin-2	#I admit that having the +/- 2's here to a certain extent obviates the exactEW/exactNS bit I put below ...
  eastbound=lonmax+2
  southbound=latmin-2
  northbound=latmax+2
  resEW=0.5;resNS=0.5	#Degrees
  cellnum=0;areanames=c();latmins=c();latmaxs=c();lonmins=c();lonmaxs=c();MMs=c();cpal2=c();cellnumtxt=c()
  exactEW=FALSE;exactNS=FALSE
  if (((eastbound-westbound)/resEW)==floor((eastbound-westbound)/resEW)) {exactEW=TRUE}
  if (((northbound-southbound)/resNS)==floor((northbound-southbound)/resNS)) {exactNS=TRUE}
  if (exactEW) {divsEW=seq(from=westbound,to=eastbound,by=resEW)} else {divsEW=c(seq(from=westbound,to=eastbound,by=resEW),eastbound)}
  if (exactNS) {divsNS=seq(from=southbound,to=northbound,by=resNS)} else {divsNS=c(seq(from=southbound,to=northbound,by=resNS),northbound)}
  for (ii in 1:(length(divsEW)-1)) {
   for (jj in 1:(length(divsNS)-1)) {
    stckindx=((ii-1)*(length(divsNS)-1))+jj
    latcellcentre=(divsNS[jj]+divsNS[jj+1])/2
    loncellcentre=(divsEW[ii]+divsEW[ii+1])/2
    col99="";reg99=""
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
     cpal2=c(cpal2,col99)
     areanames=c(areanames,reg99)
     cellnumtxt=c(cellnumtxt,toString(cellnum))
    }
   }
  }
