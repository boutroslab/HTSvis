<p>
<img align="right" max-width="75%" src="https://github.com/boutroslab/HTSvis/blob/master/inst/appdir/WWW/logo.png">
</p>

</br> 
</br> 

# HTSvis: a web app for exploratory data analysis and visualization of arrayed high-throughput screens 


Version 1.2.0
</br> 



## About 
HTSvis is an R/Shiny open-source web application for interactive visualization and 
exploratory analysis of data from arrayed high-throughput screens. The web application 
is either available via http://htsvis.dkfz.de/ or can be installed as an R package as described here.
Shiny allows that the usage of the application in the default web browser does not require any bioinformatics training.
Input data can either be a result file obtained upon analysis with the Bioconductor/R package cellHTS or a generic table with raw or analyzed data from, e.g. a high-content microscopy screen. Any data has to be aggregated per well. Tools to aggregate single cell data from microscopy screens are available in CellProfiler Analyst, for example.  </br> 
</br> 
## Installation
HTSvis is provided as an R package and requires R version 3.1.2 for installation 
(the R version dependency can be changed in the DESCRIPTION file of the package).</br>
</br> 
Run the following lines of code in your R session to download and install the package:
```
install.packages("devtools", dependencies = TRUE)

devtools::install_github("boutroslab/HTSvis", build_vignettes = F, type="source")
```
## Usage 
### Load the package and call the function 'HTSvis' to start the web application
We recommend to start a new session and clear the workspace when using the application 
```
library(HTSvis)

HTSvis()
```
## Test Data
Some sample datasets can be downloaded from http://b110-wiki.dkfz.de/confluence/display/HTSvis. This page also contains detailed descriptions of each test dataset.<br />
Following test data sets are provided, please check the help page in the app for instructions on how to upload them. <br />

| File | Description |
| --- | --- |
| topTable.txt<sup>1</sup> | result table from an analysis of an RNAi screen using cellHTS2 (also available as .xlsx) |
| topTable_dc.txt<sup>1</sup>  | result table from an analysis of a dual channel RNAi screen using cellHTS2 |
| humanSGI.RData<sup>2</sup> |  multiparametric data set from an image-based screen |
| 96wellFACS.csv   |  multiparametric data set from a flowcytometry screen (unpublished) |


</br> <sup>1</sup> cellHTS: Analysis of cell-based RNAi screens, Boutros et al. 2006, Genome Biology 
</br> <sup>2</sup> Measuring genetic interactions in human cells by RNAi and imaging, Laufer et al. 2014, Nature Methods
</br>
</br>

## Manual
### A comprehensive manual is provided on the help page in the application 
</br>

## Tip
If you're running HTSvis on a laptop, low battery might slow down the application 
</br>
</br>

## Additional notes 
<b>macOS</b> user should have the current version of Xcode Command Line Tools installed<br />
<b>Windows </b>user should have the current version of RTools installed <br />
<b>Linux</b> user should have a compiler with corresponding development libraries installed (e.g. r-devel or r-base-dev) <br />

In case the installation fails try to install the following packages manually  
```
install.packages("tidyr")
install.packages("miniUI")
install.packages("shinyjs")
install.packages("httpuv")
install.packages("gplots")
install.packages("htmlwidgets")
```
Xcode can be downloaded from the App Store<br />
RTools can be downloaded from https://cran.r-project.org/bin/windows/Rtools/

The package also depends on other R-packages that should be automatically downloaded and installed by devtools. However, a detailed list can be found below:
    R (>= 3.3.2),
    tools (>= 3.0.0),
    tibble (>= 1.2),
    stringr (>= 1.1.0),
    tidyr (>= 0.6.0),
    data.table (>= 1.10.0), 
    shinyjs (>= 0.8),
    ggplot2 (>= 2.2.0),
    reshape2 (>= 1.4.2),
    dplyr (>= 0.5.0),
    ggvis (>= 0.4.3),
    RColorBrewer (>= 1.1.2),
    scales (>= 0.4.1),
    gplots (>= 3.0.1),
    DT (>= 0.2),
    gtools (>= 3.5.0),
    shiny (>= 0.14.2),
    readxl (>= 1.0.0)
  
### HTSvis on local servers 
HTSvis can be deployed on local servers using shiny-server. This way local IT-departments can provide their own instance of HTSvis as web server. In brief, the GitHub repository has to be downloaded and the inst/appdir folder has to be copied to a designated folder on the shiny-server (e.g. /var/www/webapps). The detailed procedure depends on the local computing infrastructure. Further, detailed instructions on how to deploy shiny apps for the local network can be found in the shiny-server administrator's guide: http://docs.rstudio.com/shiny-server/

## Contact 
#### Christian Scheeder c.scheeder@dkfz.de
#### Florain Heigwer f.heigwer@dkfz.de
#### Michael Boutros m.boutros@dkfz.de 


