####Function to plot the color keys of the plate heatmaps in the plate viewer tab
##Arguments:
#col_palette: color scale defined in global.R
#limits:    upper and lower numerical limit of color scale 
#           color scale is allways spanned over entire color space. 
           

#set margins
#get min and max values
#create vector with breaks 
#create color key as matrix
#plot the matrix
#adjust axes

plotKey <- function(col_palette,limits) {
    par(oma = c(0,0,0,0),mar=c(2,2,0,2))  
        min <- round(limits[1],4)
        max <- round(limits[2],4)
        scale <- c(seq(min,max,(max-min)/500))
        col_mtrx <- matrix(scale, ncol =1, nrow = length(scale) )
        image(
            col_mtrx,  
            col = col_palette,
            xlab="",
            ylab = "",
            axes = F
        )
  axis(1, at = 0:1, labels = c(min,max), cex.axis=1)
}