

<p>
<img align="right" max-width="75%" src="https://github.com/cscheeder/HTSvis/blob/master/inst/appdir/WWW/logo.png">
<h1 id="title"  >HTSvis</h1>
</p>



# A shiny application for the visualization and analysis of arrayed high-throughput screens 




## INSTALLATION
### HTSvis is provided as an R package
### Run the following lines of code in your R session to download and install the R package
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
| kimorph.RData<sup>2</sup> |  multiparametric data set from an image-based profiling experiment |
| 96wellFACS.csv   |  multiparametric data set from a flowcytometry screen |
</br> <sup>1</sup> cellHTS: Analysis of cell-based RNAi screens, Boutros et al. 2006, Genome Biology 
</br> <sup>2</sup> imageHTS: Analysis of high-throughput microscopy-based screens. R package version 1.24.0. Pau G et al. 2016 

## Manual
### A comprehensive manual is provided on the help page in the application 

## Contact 
#### Christian Scheeder c.scheeder@dkfz.de
#### Michael Boutros m.boutros@dkfz.de 


