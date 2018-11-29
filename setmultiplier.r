multiplier=1
axisunits=var2plot$varunits
rttspatial=quantile(x=var2plot$datain,p=0.20,na.rm=TRUE)	#Not rttspatial=median(var2plot$datain,na.rm=TRUE). I need a sensible threshold for var & I think the standard kind of approach is to work out the level lvl such that 25% of the values of var are below lvl (assuming a normal distribution of var values), i.e. the first quantile.

#4: Gridbox surface pressure (pstar in Pa)
#5: Gridbox temperature at 1.5m height (t1p5m_gb in K)
if (choice8==5 && axisunits=="K") {
 multiplier=1	#I haven't implemented a non-multiplicative change of units
 rttspatial=273.15+25	#25°C is a hot day.
}
#6: Gridbox specific humidity at 1.5m height (q1p5m_gb in kg kg-1)
if (choice8==6 && axisunits=="kg kg-1") {
 multiplier=1000;axisunits="g/kg"
 rttspatial=10/multiplier
}
#7: Gridbox wind speed (wind in m s-1)
#8: Gridbox surface downward LW radiation (lw_down in W m-2)
if (choice8==8 && axisunits=="W m-2") {
 multiplier=1;axisunits="W/m2"
 rttspatial=375/multiplier	#LWdown is, on average, around 324 W/m2 (http://science.nasa.gov/media/medialibrary/2002/04/03/22apr_ceres_resources/rad_bal.gif ; I guess that is an average of a 24 hr period across all seasons?). 400 W/m2 is not very extreme but equivalent to a bright temperate summer's day e.g. see http://wattsupwiththat.com/2011/09/15/cloud-radiation-forcing-in-the-tao-dataset/
}
#9: Gridbox surface downward SW radiation (sw_down in W m-2)
if (choice8==9 && axisunits=="W m-2") {
 multiplier=1;axisunits="W/m2"
 rttspatial=300/multiplier	#SWdown is, on average, around 342 W/m2 (http://science.nasa.gov/media/medialibrary/2002/04/03/22apr_ceres_resources/rad_bal.gif ; I guess that is an average of a 24 hr period across all seaosns?). My SWdown values range from 109.0 to 334.6 W/m2 so go for 300 W/m2 is not very extreme but equivalent to a bright temperate summer's day e.g. see http://wattsupwiththat.com/2011/09/15/cloud-radiation-forcing-in-the-tao-dataset/
}
#10: Gridbox precipitation rate (precip in kg m-2 s-1)
if (choice8==10 && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24*(365.25/12);axisunits="mm/mo"
 rttspatial=30/multiplier
}
#11: Fractional cover of each surface type (frac in NoUnitsGiven)
#12: PFT leaf area index (lai in NoUnitsGiven)
#13: PFT canopy height (canht in m)
#14: Gridbox gross primary productivity (gpp_gb in kg m-2 s-1)
if (choice8==14 && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24*365.25*10000/1000;axisunits="t C ha-1 yr-1"
 rttspatial=35/multiplier
}
#15: Gridbox net primary productivity (npp_gb in kg m-2 s-1)
if (choice8==15 && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24*365.25*10000/1000;axisunits="t C ha-1 yr-1"
 rttspatial=15/multiplier
}
#16: Gridbox plant respiration (resp_p_gb in kg m-2 s-1)
if (choice8==16 && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24*365.25*10000/1000;axisunits="t C ha-1 yr-1"
}
#17: Gridbox soil respiration (total) (resp_s_gb in kg m-2 s-1)
if (choice8==17 && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24*365.25*10000/1000;axisunits="t C ha-1 yr-1"
}
#18: Gridbox mean vegetation carbon at end of model timestep. (cv in kg m-2)
#19: Gridbox soil carbon (total) (cs_gb in kg m-2)
#20: Gridbox runoff rate (runoff in kg m-2 s-1)
if (choice8==20 && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24;axisunits="mm/day"
 rttspatial=1/multiplier
}
#21: Gridbox surface runoff (surf_roff in kg m-2 s-1)
if (choice8==21 && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24;axisunits="mm/day"
}
#22: Gridbox sub-surface runoff (sub_surf_roff in kg m-2 s-1)
if (choice8==22 && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24;axisunits="mm/day"
}
#23: Saturation excess surface ('Dunne') runoff (sat_excess_roff in kg m-2 s-1)
if (choice8==23 && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24;axisunits="mm/day"
}
#24: Gridbox total soil moisture in column (smc_tot in kg m-2)
#25: Drainage from bottom (nshyd) soil layer (drain in kg m-2 s-1)
if (choice8==25 && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24;axisunits="mm/day"
}
#26: Baseflow (lateral subsurface runoff) (qbase in kg m-2 s-1)
if (choice8==26 && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24;axisunits="mm/day"
}
#27: Baseflow from deep layer (qbase_zw in kg m-2 s-1)
if (choice8==27 && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24;axisunits="mm/day"
}
#28: Gridbox mean depth to water table (zw in m)
#29: Wetland fraction at end of model timestep. (fwetl in NoUnitsGiven)
if (choice8==29) {
 multiplier=100;axisunits="% cover"
}
#30: Scaled methane flux from wetland fraction (fch4_wetl in 10^-9 kg m-2 s-1)
#31: Gridbox surface evapotranspiration from soil moisture store (esoil_gb in kg m-2 s-1)
if (choice8==31 && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24;axisunits="mm/day"
}
#32: Gridbox mean evaporation from canopy/surface store (ecan_gb in kg m-2 s-1)
if (choice8==32 && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24;axisunits="mm/day"
}
#33: Gridbox albedo (as used for net shortwave calculation) (albedo_land in NoUnitsGiven)
#34: Gridbox surface latent heat flux (latent_heat in W m-2)
#35: Tile surface latent heat flux for land tiles (le in W m-2)
#36: Gridbox surface sensible heat flux (ftl_gb in W m-2)
#37: Tile surface sensible heat flux for land tiles (ftl in W m-2)
#38: Gridbox moisture flux from surface (fqw_gb in kg m-2 s-1)
if (choice8==38 && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24;axisunits="mm/day"
}
#39: Tile surface moisture flux for land tiles (fqw in kg m-2 s-1)
if (choice8==39 && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24;axisunits="mm/day"
}
#40: Gridbox sub-surface temperature of each layer (t_soil in K)
#41: Gridbox unfrozen moisture content of each soil layer as a fraction of saturation (sthu in NoUnitsGiven)
if (choice8==41) {
 multiplier=100;axisunits="% of saturation"
}
#42: Gridbox frozen moisture content of each soil layer as a fraction of saturation (sthf in NoUnitsGiven)
if (choice8==42) {
 multiplier=100;axisunits="% of saturation"
}
#43: Soil wetness in deep (water table/TOPMODEL) layer (sthzw in NoUnitsGiven)
#44 et seq.: Calculated combinations
#44: Inverted precip
if (choice8==(length(varsinncfile)+1) && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24*(365.25/12);axisunits="mm/mo"
 rttspatial=30/multiplier
}
#45: Inverted Dunne runoff
if (choice8==(length(varsinncfile)+2) && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24;axisunits="mm/day"
 rttspatial=quantile(x=var2plot$datain,p=0.12,na.rm=TRUE)	#For inverted dunnerunoff, p>=0.20 or p<0.05 gives a blank plot
}
#46: SPI
if (choice8==(length(varsinncfile)+3)) {
# rttspatial=2.0	#Picks out prob. of pixels being "extremely wet"
# rttspatial=1.5	#Picks out prob. of pixels being "very wet" or wetter
 rttspatial=1.0	#Picks out prob. of pixels being "moderately wet" or wetter
# rttspatial=-1.0	#Picks out prob. of pixels being normal or wet
}
#47: Inverted SPI
if (choice8==(length(varsinncfile)+4)) {
# rttspatial=2.0	#Picks out prob. of pixels being "extremely dry"
# rttspatial=1.5	#Picks out prob. of pixels being "very dry" or drier
 rttspatial=1.0	#Picks out prob. of pixels being "moderately dry" or drier
# rttspatial=-1.0	#Picks out prob. of pixels being normal or dry
}
#48: Inverted sthu (gridbox unfrozen moisture content of each soil layer as a fraction of saturation)
if (choice8==(length(varsinncfile)+5)) {
 multiplier=-100;axisunits="% of saturation"
 rttspatial=-30	#Picks out prob. of pixels with -sthu>-0.3, which is a commonly-used mean value for dry soils.
}
#49: Inverted (P-ET), i.e. (ET-P)
if (choice8==(length(varsinncfile)+6) && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24;axisunits="mm/day"
 rttspatial=1.0/multiplier	#Picks out prob. of pixels with (P-E)<1 mm/day
}
#50: Inverted (P/ET), i.e. (ET/P), the reciprocal of UNEP's aridity index http://en.wikipedia.org/wiki/Aridity_index
if (choice8==(length(varsinncfile)+7) && axisunits=="kg m-2 s-1") {
 multiplier=1;axisunits="(frac)"
 rttspatial=2	#Picks out prob. of pixels with (ET/P)>2 (see Zhou et al. 2015:Fig.5a : P/PET up to 0.5 is red/yellow).
}
#51: (ET+runoff-P), i.e. -storage (runoff=Q) or the loss rate of water from the soil
if (choice8==(length(varsinncfile)+8) && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24*(365.25/12);axisunits="mm/mo"
 rttspatial=4.5/multiplier	#Picks out prob. of pixels with (loss rate)>4.5 mm/mo
}
#52: ET
if (choice8==(length(varsinncfile)+9) && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24*(365.25/12);axisunits="mm/mo"
# rttspatial=5/multiplier	#Picks out prob. of pixels with ET>5 mm/mo
# rttspatial=10/multiplier	#Picks out prob. of pixels with ET>10 mm/mo
# rttspatial=20/multiplier	#Picks out prob. of pixels with ET>20 mm/mo
 rttspatial=30/multiplier	#Picks out prob. of pixels with ET>30 mm/mo
}
#53: Storage (=P-ET-runoff =((effective precipitation)-runoff))
if (choice8==(length(varsinncfile)+10)) {
 multiplier=60*60*24*(365.25/12);axisunits="mm/mo"
 rttspatial=30/multiplier	#Picks out prob. of pixels with S>30 mm/mo
}
#54: -ET
if (choice8==(length(varsinncfile)+11) && axisunits=="kg m-2 s-1") {
 multiplier=60*60*24*(365.25/12);axisunits="mm/mo"
# rttspatial=-5/multiplier	#Picks out prob. of pixels with ET<5 mm/mo
# rttspatial=-10/multiplier	#Picks out prob. of pixels with ET<10 mm/mo
# rttspatial=-20/multiplier	#Picks out prob. of pixels with ET<20 mm/mo
 rttspatial=-30/multiplier	#Picks out prob. of pixels with ET<30 mm/mo
}
