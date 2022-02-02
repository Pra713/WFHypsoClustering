The script for this analysis was written in R. GIS analysis was performed in ArcGIS Desktop. However, the GIS analysis can be done in any GIS program.

If anyone faces issues when running this program, please feel free to email me about any questions to prashanta.bajracharya@maine.edu. I am happy to answer any questions and/ or assist with the code.

Prerequisites:
- An IDE for R. I prefer to use RStudio.
- Required packages in R: tidyverse, gridExtra, rgdal, raster, reticule, princurve, cluster, factoextra, dendextend, reshape2, minpack.lm, sn, mixsmsn
- Raw GIS files. These files include the DEM raster file and the sub-basins shape file.
- A GIS program that can run spatial analysis and terrain analysis tools. 

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
- I ran the terrain analysis tools for filling sinks, flow direction, flow accumulation, slope, and flow length analysis in ArcGIS. If you want to run these tools in your preferred GIS program, you can use the outputs of these tools as input to the R script. 
- I use ArcGIS desktop's ArcPy script to extract the terrain data for each sub-basin separately. If you prefer to do this in a different way, you can then use the clipped DEMs (for each sub-basins) as the inputs instead and skip the GIS section of the code.
- The main code is divided into multiple sections. The sections are named in SXX-YY format, where XX denotes the section number, and YY denotes the sub-section number.
- Section 1 (S01-)
  - S01-01: This section loads all the necessary packages and reads other function files. Additinally, this section also sets the file and folder locations. This includes the location for the GIS files (sub-basins vector file, DEM, flow accumulation raster, slope raster, and flow length raster) as input files, output folder locations, and other parameters. More information is available as comments in the code. The file formats used in the code are .shp files for vector and .tif files for rasters. However, with minor modifications, the code can be updated to use other popular file formats.
  - S01-02: Once you have run some of the code in later sections, they are then saved as RDS files so that the same code does not need to be run in later R sessions. Instead of rerunning the code, you can just read the saved RDS files.
- Section 2 (S02-) 
  - S02-01: ArcGIS desktop's ArcPy script is used here to extract the terrain data for each sub-basin separately. If you prefer to do this in a different way, you can then use the clipped DEMs (for each sub-basins) as the inputs instead and skip the GIS section of the code.
  - S02-02: The extracted sub-basin data is then converted to data frame format.
  - S02-03: The spatial sub-basin data is them summarized into sub-basins statistics such as minimum and maximum elevation.
- Section 3 (S03-) 
  - S03-01: This section has the code to perform the functional estimation of hypsometric curves.
  - S03-02: The functional estimation parameters are then tabulated here. Additionally, hypsometric curves at 0.01 intervals in x axis are generated from the fitted hypsometric function.
  - S03-03: Next, a distance matrix is created based on the discordance index.
  - S03-04: The sub-basins are then grouped using hierarchical clustering. A variety of algorithms are tested (average, single, complete, and ward). The best algorithm can then be chosen based on agglomerative coefficient.
  - S03-05: Outliers are removed here. A variety of methods are provided. In the end, the choice of threshold for outlier removal requires some subjective judgement.
  - S03-06: Code for plotting the results of hypsometric clustering. The codes are plotted on-screen, but can be output as image files (png, etc.) or as pptx files.
- Section 4 (S04-) 
  - S04-01: This section has the code to perform the functional estimation of width functions.
  - S04-02: The functional estimation parameters are then tabulated here. Additionally, width functions at 0.01 intervals in x axis are computed from the fitted width function.
  - S04-03: Next, a distance matrix is created based on the discordance index.
  - S04-04: The sub-basins are then grouped using hierarchical clustering. A variety of algorithms are tested (average, single, complete, and ward). The best algorithm can then be chosen based on agglomerative coefficient.
  - S04-05: Outliers are removed here. A variety of methods are provided. In the end, the choice of threshold for outlier removal requires some subjective judgement.
  - S04-06: Code for plotting the results of width function clustering. The codes are plotted on-screen, but can be output as image files (png, etc.) or as pptx files.
- Section 5 (S05-) 
  - S05-01: The width function and hypsometric function data are combined into a single data frame.
  - S05-02: This section has code to make entanglement plots and cross plots between hypsometric and width function clusters.
