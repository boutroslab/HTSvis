

<p>
<img align="right" max-width="75%" src="https://github.com/cscheeder/HTSvis/blob/master/inst/appdir/WWW/logo.png">
<h1 id="title"  >HTSvis</h1>
</p>



# A shiny app for exploratory data analysis and visualization of arrayed high-throughput screens 




## INSTALLATION
### HTSvis is provided as an R package
### Run the following lines of code in your R session to download and install the package
```
install.packages("devtools", dependencies = TRUE)

devtools::install_github("boutroslab/HTSvis", build_vignettes = F)
```
## Usage 
### Load the packe and call the funtion 'HTSvis' to start the web application
We recommend to start a new session and clear the workspace when using the application 
```
library(HTSvis)

HTSvis()
```
## Test Data
Test data can be found in the GitHub repository ('test_data' folder) and has to be downloaded manually. <br />
Following test data sets are provided:<br />

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


