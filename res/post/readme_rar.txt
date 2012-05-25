#####################################################
# TU Berlin Regional Atmospheric Reanalysis Dataset #
#####################################################


    ****************
    * Introduction *
    ****************

Copyright: Fachgebiet Klimatologie - Institut für Ökologie - Technische Universität Berlin
Contact: F. Maussion, fabien.maussion@tu-berlin.de


    ******************
    * 1. File Format *
    ******************

NetCDF (Network Common Data Format) *Version 4 and higher* (http://www.unidata.ucar.edu/software/netcdf/)
Each file contains one *single* variable for one year period, in various aggregation steps.
For more information about the files, use for example the ncdump utility or other softwares capable to read NCDF formats (dump for daily air temperature given as an example at the end of this document)


    *****************************
    * 2. File naming convention *
    *****************************

"region"_ "spatial resolution"_"aggregation level"_"vertical resolution"_"variable name"_"year".nc

Examples: 
tibet_d10km_d_3d_eta_cldfra_2002.nc
tibet_d10km_d_3d_eta_cldfra_2002.nc
tibet_d30km_y_2d_psfc_2010.nc
tibet_d30km_static_hgt.nc

Spatial resolution: 
d30km, d10km or d02km

Aggregation level: 
h (hourly, model output), d (daily means), m (monthly means), y (yearly means), static (invariant fields)

Vertical resolution:
2d (surface variables), 3d_eta (model eta levels), 3d_press (pressure levels), 3d_soil (soil levels)


    **********************************************
    * 3. Georeference (map projection and grids) *
    **********************************************

Each file contains sufficent information for a precise representation of the WRF grid (see WRF-ARW's user guide for more info on WRF projections).
Working with lon/lat matrices will be unefficient and generate errors, it is strongly recommended to build a clean grid information using e.g. ENVI/IDL or other map projection softwares.  

    ********************
    * 4. Variable list *
    ********************

NAME                 TYPE    DESCRIPTION                                    UNIT 
 albedo               2d      albedo                                         -    
 canwat               2d      canopy water                                   kg m-
 cldfra               3d      cloud fraction                                 -    
 cosalpha             2d      local cosine of map rotation                   -    
 emiss                2d      surface emissivity                             -    
 geopotential         3d      full model geopotential on mass points         m2 s-
 graupel              2d      grid scale graupel (step-wize)                 mm   
 grdflx               2d      ground heat flux                               w m-2
 hail                 2d      grid scale hail (step-wize)                    mm   
 hfx                  2d      upward heat flux at the surface                w m-2
 hgt                  2d      terrain height                                 m    
 isltyp               2d      dominant soil category                         -    
 ivgtyp               2d      dominant vegetation category                   -    
 lai                  2d      leaf area index                                area/
 landmask             2d      land mask (1 for land, 0 for water)            -    
 lh                   2d      latent heat flux at the surface                w m-2
 lu_index             2d      land use category                              -    
 lwdown               2d      downward long wave flux at ground surface      w m-2
 lwup                 2d      upward long wave flux at ground surface        w m-2
 netrad               2d      net radiation at ground surface (+ = downward) w m-2
 pblh                 2d      pbl height                                     m    
 potevap              2d      potential evaporation (step-wize)              w m-2
 prcp                 2d      total precipitation (step-wize)                mm   
 prcp_c               2d      cumulus precipitation (step-wize)              mm   
 prcp_fr              2d      frozen precipitation (step-wize)               mm   
 prcp_nc              2d      grid scale precipitation (step-wize)           mm   
 pressure             3d      full model pressure                            hpa  
 psfc                 2d      sfc pressure                                   pa   
 q2                   2d      qv at 2 m                                      kg kg
 qfx                  2d      upward moisture flux at the surface            kg m-
 qvapor               3d      water vapor mixing ratio                       kg kg
 sfroff               2d      surface runoff                                 mm   
 sh2o                 soil    soil liquid water                              m3 m-
 sinalpha             2d      local sine of map rotation                     -    
 slp                  2d      sea level pressure                             hpa  
 smcrel               soil    relative soil moisture                         -    
 smois                soil    soil moisture                                  m3 m-
 snow                 2d      snow water equivalent                          kg m-
 snowfall             2d      grid scale snow and ice (step-wize)            mm   
 snowh                2d      physical snow depth                            m    
 sr                   2d      fraction of frozen precipitation               -    
 sst                  2d      sea surface temperature                        k    
 swdown               2d      downward short wave flux at ground surface     w m-2
 swup                 2d      upward short wave flux at ground surface       w m-2
 t2                   2d      temp at 2 m                                    k    
 theta                3d      potential temperature (theta)                  k    
 tsk                  2d      surface skin temperature                       k    
 tslb                 soil    soil temperature                               k    
 u                    3d      x-wind component                               m s-1
 u10                  2d      u at 10 m                                      m s-1
 udroff               2d      underground runoff                             mm   
 ust                  2d      u* in similarity theory                        m s-1
 v                    3d      y-wind component                               m s-1
 v10                  2d      v at 10 m                                      m s-1
 vegfra               2d      vegetation fraction                            -    
 w                    3d      z-wind component                               m s-1


    *******************
    * 5. Example file *
    *******************

netcdf : tibet_d10km_d_2d_t2_2006.nc
{

---------------
* Dimensions * 
---------------

       time = UNLIMITED ; // 365 currently
       west_east = 270 ;
       south_north = 180 ;
 
----------------------
* Global attributes * 
----------------------
 
       TITLE = WRF Regional Atmospheric Reanalysis - Central Asia and the Tibetan Plateau - WET_Standard_V01_d02
       DATA_NOTES = File generated with the output of successive model runs of 36H (first 12 hours discarded for spin-up)
       WRF_VERSION = WRF Regional Atmospheric Reanalysis - Central Asia and the Tibetan Plateau - WET_Standard_V01_d02
       CREATED_BY = Fabien Maussion - fabien.maussion@tu-berlin.de
       INSTITUTION = Technische Universität Berlin, Institut für Ökologie, Fachgebiet Klimatologie
       CREATION_DATE = 07.04.2012 13:31:05
       SOFTWARE_NOTES = IDL V7.1.1, WAVE post V0.1
       VARNAME = t2
       PROJECTION = WRF Lambert Conformal
       PROJ_ENVI_STRING = 4, 6370000.00000000, 6370000.00000000,      30.00000000,      87.00000000, 0.0, 0.0,      30.00000000,      35.00000000, WGS-84, WRF Lambert Conformal
       DATUM = WGS-84
       DOMAIN =            2
       NESTED = YES
       TIME_ZONE = UTC
       GRID_INFO = Grid spacing: Global Attributes DX and DY (unit: m), Down left corner: Global Attributes X0 and Y0 (unit: m) 
       DX =       10000.0
       DY =       10000.0
       X0 =   -1434999.0
       Y0 =    -534999.9
       PRODUCT_LEVEL = d
       LEVEL_INFO = H: original simulation output; D: daily; M: monthly; Y: yearly; 
 
--------------
* Variables * 
--------------
 
       long time(time)  ;
               time:long_name = time ;
               time:units = days since 2006-01-01 00:00:00 ;
       float west_east(west_east)  ;
               west_east:long_name = x-coordinate in cartesian system ;
               west_east:units = m ;
       float south_north(south_north)  ;
               south_north:long_name = y-coordinate in cartesian system ;
               south_north:units = m ;
       float lon(west_east,south_north)  ;
               lon:long_name = longitude ;
               lon:units = degrees_east ;
       float lat(west_east,south_north)  ;
               lat:long_name = latitude ;
               lat:units = degrees_north ;
       float t2(west_east,south_north,time)  ;
               t2:long_name = temp at 2 m ;
               t2:units = k ;
               t2:agg_method = mean ;

}


