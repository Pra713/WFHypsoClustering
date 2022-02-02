The script for this analysis was written in R. GIS analysis was performed in ArcGIS Desktop. However, the GIS analysis can be done in any GIS program.

Prerequisites:
- An IDE for R. I prefer to use RStudio.
- Required packages in R: tidyverse, gridExtra, rgdal, raster, reticule, princurve, cluster, factoextra, dendextend, reshape2, minpack.lm, sn, mixsmsn
- Raw GIS files. These files include the DEM raster file and the sub-basins shape file.
- A GIS program that can run terrain analysis tools. These tools include filling sinks, flow direction, flow accumulation, and flow length analysis. I use ArcGIS and have provided ArcPy codes to perform these analysis in R. If you want to run these tools in your preferred GIS program, you can use the outputs of these tools as input to the R script and skip the GIS section of the script.

General Instructions:
- The code is divided into multiple sections. The sections are named in SXX-YY format, where XX denotes the section number, and YY denotes the sub-section number.
- Section 1 (S01-)
  - S01-01: 
  - S01-02:
- Section 2 (S02-) 
