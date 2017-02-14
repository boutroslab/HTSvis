<p>
<img align="right" max-width="75%" src="https://github.com/cscheeder/HTSvis/blob/master/inst/appdir/WWW/logo.png">
</p>

</br> 
</br> 

# A shiny app for exploratory data analysis and visualization of arrayed high-throughput screens 

</br> 
</br> 

## About 
HTSvis is R/Shiny open-source web application for interactive visualization and 
exploratory analysis of data from arayed high-throughput screens. 
The apllication can be installed as an R package and is started fom the R console,
usage of the application in the user's default web browser does not require further coding.
Input data can either be a result file obtained upon analysis with the Bioconductor/R package cellHTS or 
a generic table with raw or analyzed data from, e.g. a high-content microscopy screen. 

## INSTALLATION
HTSvis is provided as an R package.</br> 
Run the following lines of code in your R session to download and install the package:
```
install.packages("devtools", dependencies = TRUE)

devtools::install_github("boutroslab/HTSvis", build_vignettes = F)
```
## Usage 
### Load the package and call the funtion 'HTSvis' to start the web application
We recommend to start a new session and clear the workspace when using the application 
```
library(HTSvis)

HTSvis()
```
## Test Data
Test data can be found in the GitHub repository ('test_data' folder), you can get it by dowloading the repository ('clone or download' option). Please note that Safari does not support the upload of RData files.<br />
Following test data sets are provided, please check the help page in the app for instructions on how to upload them:<br />

| File | Description |
| --- | --- |
| topTable.txt<sup>1</sup> | result table from an analysis of an RNAi screen using cellHTS2 |
| topTable_dc.txt<sup>1</sup>  | result table from an analysis of a dual channel RNAi screen using cellHTS2 |
| humanSGI.RData<sup>2</sup> |  multiparametric data set from an image-based screen |
| 96wellFACS.csv   |  multiparametric data set from a flowcytometry screen (unpublished) |
</br> <sup>1</sup> cellHTS: Analysis of cell-based RNAi screens, Boutros et al. 2006, Genome Biology 
</br> <sup>2</sup> Measuring genetic interactions in human cells by RNAi and imaging, Laufer et al. 2014, Nature Methods

## Manual
### A comprehensive manual is provided on the help page in the application 
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

## Contact 
#### Christian Scheeder c.scheeder@dkfz.de
#### Michael Boutros m.boutros@dkfz.de 


