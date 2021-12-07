# WAVE library

## Table of Content

-   [Installation](#Installation)
-   [Overview](#Overview)
    -   [Naming Convention](#Naming-Convention)
    -   [Project Structure](#Project-Structure)
-   [History](#History)
-   [Author](#Author)
-   [Copyright](#Copyright)

## Installation {#installation}

Find the install manual for Windows from IDL (german) here:
[Intranet](https://www.klima.tu-berlin.de/fg_doku/doku.php?id=idl)

or clone the repository

```sh
git clone https://gitlab.klima.tu-berlin.de/klima/WAVE.git
```

Start IDL and make sure the folder is in your path environment. Then run

```idl
@WAVEstart.mac
```

This compiles the library.

## Overview {#overview}

### Naming Convention {#naming-convention}

-   files starting with **"w\_"** are objects or **routines having the same name** as the file
-   files in **capital letters** contain a bundle of **standalone routines/functions**

### Project Structure {#project-structure}

| Folder           |                                                                                                                                                                                                                                                                                                                                                                                Description                                                                                                                                                                                                                                                                                                                                                                                 |
| ---------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------: |
| ./               |                                                                                                                                                                                                                                                                                                                           Main-level programs such as `WAVEstart.mac` which initialises the library, `WAVE.pro` which defines the error handler.                                                                                                                                                                                                                                                                                                                           |
| dev              |                                                                                                                                                                                                                                                                                                                                                 Routines in development before their integration in the sources directory.                                                                                                                                                                                                                                                                                                                                                 |
| doc              |                                                                                                                                                                                                                                                                                                                                                                            Documentation files.                                                                                                                                                                                                                                                                                                                                                                            |
| res              |                                                                                                                                                                                                                                                                                                                                                                              Resource files.                                                                                                                                                                                                                                                                                                                                                                               |
| res/files        |                                                                                                                                                                                                                                                                                                                                                                          Resource files (ASCII).                                                                                                                                                                                                                                                                                                                                                                           |
| res/ncdc         |                                                                                                                                                                                                                                                                                                                                                           Templates and `.sav` files for NCDC library routines.                                                                                                                                                                                                                                                                                                                                                            |
| res/shapes       |                                                                                                                                                                                                                                                                                                                                                                 Resource files (shape files for mapping).                                                                                                                                                                                                                                                                                                                                                                  |
| retired          |                                                                                                                                                                                                                                                                                                                                                             Routines kept for compatibility (Roman's) reasons.                                                                                                                                                                                                                                                                                                                                                             |
| src              |                                                                                                                                                                                                                                                                                                                                                                             The source files.                                                                                                                                                                                                                                                                                                                                                                              |
| src/general      |                                                                                                                                                                                                                                                                               Routines of general puproses. `TIME.pro` is providing time handling and time series processing tools that are used by the WAVE. `UTILS.pro` contains all routines that did not find their place anywhere else.                                                                                                                                                                                                                                                                               |
| src/gis          |                                                                                                                                                                             The gis directory is a central package. It contains the grid transformations handler `w_grid2d` class, some low level tools for manipulating NetCDF files (`w_ncdf`) and HDF files (`w_hdf`), some medium level tools for "conventional" georeferenced climatological datasets (`w_geo_nc` that knows how to read COARDS conventions, or `w_hdf_eos` that is currently more a place holder than a finished tool).                                                                                                                                                                              |
| src/gis/datasets | High level tools based on `src/gis/` low level objects for reading and manipulating usual datasets such as `w_wrf`, `w_trmm`, `w_modis` and more are to come... All these objects realize two "contracts": <br/>1. They implement `w_grid2d` and thus provide all the usefull transformation routines such as `w_Grid2D::map_gridded_data` or `w_Grid2D::reGrid` or informational routines such as `w_Grid2D::get_LonLat` <br/>2. They implement one of the low level data format tools (e.g. `w_geo_nc` for `w_wrf`, `w_hdf_eos` for `w_modis`) or provide routines having the same functions to keep a certain coherence between the classes. For example, they implement `w_wrf::Define_subset`, `w_geo_nc::get_Var`, `w_geo_nc::quickPlotVar`, `w_wrf::get_TimeSerie`. |
| src/plot         |                                                                                                                                                                                                                                                       Tools to make simple plots like line plots with a time axis (`w_timelineplot`), scatter plots (`w_scatterplot`) and more in the future, but also to plot georeferenced datasets on a self-generated map (`w_map`), which is all the WAVE development is about.                                                                                                                                                                                                                                                       |
| src/plot/astron  |                                                                                                                                                                                                                                                                                                                                                            Some routines "borrowed" from the IDL astron library                                                                                                                                                                                                                                                                                                                                                            |
| src/wrf_post     |                                                                                                                                                                                                                                                                           WRF post-processing tools such as `POST_crop_file` to remove useless indexes from a WRF file, `POST_aggregate_directory` to aggregate single files into a larger one and selecting variables using configuration files                                                                                                                                                                                                                                                                           |
| test             |                                                                                                                                                                                                                                                                                                                                           Testing routines (`w_test.pro`) and some examples for the new users (`w_examples.pro`)                                                                                                                                                                                                                                                                                                                                           |

## Author {#author}

FG Klimatologie - TU Berlin

## History {#history}

WAVE 0.1 was completely re-written from scratch and released on XX.xx.2011

## Copyright {#copyright}

WAVE is for FG Klima use only.

Technische Universität Berlin Institut für Ökologie - Fachgebiet Klimatologie Rothenburgstr. 12 D-12165 Berlin Tel.:
+49-30-314-71495 www.klima.tu-berlin.de

Copyright (c) 2011 All rights reserved.

:ocean: :ocean: :ocean:
