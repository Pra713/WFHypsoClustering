The script for this analysis was written in R. GIS analysis was performed in ArcGIS Desktop. However, the GIS analysis can be done in any GIS program.

Prerequisites:
- An IDE for R. I prefer to use RStudio.
- Required packages in R: tidyverse, gridExtra, rgdal, raster, reticule, princurve, cluster, factoextra, dendextend, reshape2, minpack.lm, sn, mixsmsn
- Raw GIS files. These files include the DEM raster file and the sub-basins shape file.
- A GIS program that can run terrain analysis tools. These tools include filling sinks, flow direction, flow accumulation, and flow length analysis. I use ArcGIS and have provided ArcPy codes to perform these analysis in R. If you want to run these tools in your preferred GIS program, you can use the outputs of these tools as input to the R script and skip the GIS section of the script.

Files:
- WFandHypsoClustering: This is the main file to run all of the analysis. This file reads the functions in other R files using the "source" function.
- functions_hypso: This file has all of the codes associated with hypsometric analysis, including the functional estimation and divergence metric.
- functions_wf: This files has all of the codes for the function estimation of width function.
- functions_wf_divergence: This file has all of the codes for computing the divergence metric for width functions.
- functions_supplementary: This file has a number of helper functions.
- functions_cross_plot: This file has the codes to make the cross plot between hypsometric and width function clusters.
- DecomposablePlots: This file has the codes to save plots as pptx.

General Instructions:
- You only need to run the main code. This is the file, "WFandHypsoClustering". All other files contain functions for specific tasks while this file reads those functions and performs the analyses.
- After every analysis, the output files are saved as RDS files. This prevents the need to run these analysis in every session. Once the RDS file has been saved, the prior analysis can be skipped and the RDS file can be read instead.
- Plots made during the analysis are saved as either png files or pptx files. The code provides an options whether to save the pptx files not not. This can be skipped if desired.
- The main code is divided into multiple sections. The sections are named in SXX-YY format, where XX denotes the section number, and YY denotes the sub-section number.
- Section 1 (S01-)
  - S01-01: 
  - S01-02: 
- Section 2 (S02-) 
  - S02-01: 
  - S02-02: 
  - S02-03: 
  - S02-04: 
- Section 3 (S02-) 
  - S03-01: 
  - S03-02: 
  - S03-03: 
  - S03-04: 
  - S03-05: 
- Section 4 (S02-) 
  - S04-01: 
  - S04-02: 
  - S04-03: 
  - S04-04: 
  - S04-05: 
  - S04-06: 
  - S04-07: 
  - S04-08: 
- Section 5 (S02-) 
  - S05-01: 
  - S05-02: 
