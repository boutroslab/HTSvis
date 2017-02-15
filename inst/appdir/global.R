##Define global settings and variables

##clean workspace and console window
rm(list=setdiff(ls(), "x"))
cat("\014")

#Increase max file size for upload (default is 5MB)
options(shiny.maxRequestSize = 10000*1024^2)

##source functions
#plotKey.R for color key of heatmaps in plate viwer tab
#plotHeatmap.R for plate heatmaps
#setLimits.R for dynamic limits of color scale of plate heatmaps
#switchButton.R for radio button in scatter plot tab
#helpPopup2.R for question marks with popups
source("./plotKey.R")
source("./plotHeatmap.R")
source("./switchButton.R")
source("./setLimits.R")
source("./helpPopup2.R")


##color settings
#col_palette for plate heatmaps in plateviwer tab
#col_dfHeatmap for heatmap of clicked rows in feature table tab
#nt_col,pp_col & pm_col set colors of controls in quality control tab
#population_colors for scatter plot tab
col_palette <- colorRampPalette(c("blue", "white", "red"))(n = 500)
col_dfHeatmap <- rev(colorRampPalette(brewer.pal(9, "RdBu"))(500))
nt_col <-  "limegreen"
pp_col <- "blue"
pn_col <- "red"
population_colors <- c("#E31A1C","#1F78B4","#33A02C","#6A3D9A","#FF7F00",
                       "#8E0152","#003C30","#543005","#006837","#A50026","#DD2D26")



