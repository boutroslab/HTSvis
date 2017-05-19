####Function that return a data frame for the plate heatmaps
##data frame is created by subsetting the input table based on the values of 
##  of the corresponding drop down lists which are passed over as function arguments
##col_palette defined in global.R

plotHeatmap <- function(data_table,limits,curr_plate,curr_screen,curr_feature,plateDim,expDim,wellDim,annoDim){

  #define color breaks and range
  #limits for range passed over from Server.R
  min <- limits[1]
  max <- limits[2]

  if(min == max){
      min <- min-(min/1000)
      }

  breakPoints <- sort(
                      c( seq(min- (max-min)/(500),
                             max+ (max-min)/(500),
                                (max-min)/(500-1)
                              )
                          )
                      )

  #create a data frame corresponding to the current plate sorted according to multi-well format 
  if(curr_screen == F) {
  curr_df= data_table %>%
    dplyr::filter_(lazyeval::interp(quote(x == y),
                             x = as.name(plateDim),
                             y = curr_plate)) %>%
        dplyr::select_(annoDim,wellDim,curr_feature) %>%
            dplyr::mutate_(row=lazyeval::interp(~gsub("[^A-z]","",x),
                                       x=as.name(wellDim))) %>%
                dplyr::mutate_(column=lazyeval::interp(~gsub("[^0-9]","",x),
                                            x=as.name(wellDim))) %>%
                    dplyr::mutate_("value"=curr_feature) %>%
                        dplyr::select_(wellDim,"row","column","value",annoDim)
  } else {
    curr_df= data_table %>%
        dplyr::filter_(lazyeval::interp(quote(x == y),
                                 x=as.name(plateDim), y=curr_plate)) %>%
            dplyr::filter_(lazyeval::interp(quote(x == y),
                                   x=as.name(expDim), y=curr_screen)) %>%
                dplyr::select_(annoDim,wellDim,curr_feature) %>%
                    dplyr::mutate_(row=lazyeval::interp(~gsub("[^A-z]","",x),
                                             x=as.name(wellDim))) %>%
                    dplyr::mutate_(column=lazyeval::interp(~gsub("[^0-9]","",x),
                                                  x=as.name(wellDim))) %>%
                        dplyr::mutate_("value"=curr_feature) %>%
                            dplyr::select_(wellDim,"row","column","value",annoDim)
    }

  curr_df <- data.frame(curr_df)
  curr_df <- curr_df[gtools::mixedorder(curr_df[,wellDim]),]
  curr_df$column <- factor(
                            as.character(as.numeric(curr_df$column)),
                            levels=seq(1:n_distinct(curr_df$column))
                            )

#Set color scale
  curr_df = mutate(
                curr_df,def.color=as.character(cut(value,
                                                   breaks=breakPoints,
                                                   labels = col_palette)
                                               )
                )
  curr_df$def.color[curr_df$value < min] = col_palette[1]
  curr_df$def.color[curr_df$value > max] = col_palette[length(col_palette)]

  # Set NA values in df2$def.color to light balck in df2$def.color
  curr_df$def.color[is.na(curr_df$value)] = "black"
  curr_df$CSidB110 <- 1:nrow(curr_df)
  return(curr_df)
}
