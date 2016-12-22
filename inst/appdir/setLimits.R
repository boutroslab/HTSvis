#####################################################################################################
#Function to get limits for plate viewer
#####################################################################################################
##Arguments:
#data: loaded data set
#lo_scale_lim: input from ui.R
#hi_scale_lim: input from ui.R
#x: feature
#y: plate
#z: screen
#####################################################################################################


setLimits <- function(data_lim,lo_scale_lim,hi_scale_lim,curr_plate_lim,curr_screen_lim,curr_feature_lim) {


    if(curr_screen_lim == F) {
        plate_lims <- data_lim %>%
            select_(TabDimensions$plate,curr_feature_lim) %>%
            filter_(lazyeval::interp(quote(x == y), x = as.name(TabDimensions$plate), y = curr_plate_lim)) %>%
            select_(curr_feature_lim) %>%
            do(funs=c(min(.,na.rm=T),max(.,na.rm=T))) %>% unlist(use.names = F)
    } else {
        plate_lims <- data_lim %>%
            select_(TabDimensions$plate,TabDimensions$experiment,curr_feature_lim) %>%
            filter_(lazyeval::interp(quote(x == y), x = as.name(TabDimensions$plate), y = curr_plate_lim)) %>%
            filter_(lazyeval::interp(quote(x == y), x = as.name(TabDimensions$experiment), y = curr_screen_lim)) %>%
            select_(curr_feature_lim) %>%
            do(funs=c(min(.,na.rm=T),max(.,na.rm=T))) %>% unlist(use.names = F)
    }


    test_lo_scale_lim <- suppressWarnings(as.numeric(lo_scale_lim,na.rm = T)) #creates "Warning message: NAs introduced by coercion
    test_hi_scale_lim <- suppressWarnings(as.numeric(hi_scale_lim, na.rm = T))

    if(lo_scale_lim != ""  & isTRUE(is.na(test_lo_scale_lim))  | hi_scale_lim != ""  & is.na(test_hi_scale_lim)) {
        setMin <- plate_lims[1]
        setMax <- plate_lims[2]
        return(c(setMin,setMax) )
    } else {
        if(lo_scale_lim == "") {
            setMin <- plate_lims[1]
        } else {setMin <- suppressWarnings(as.numeric(lo_scale_lim,na.rm = T))}
        if(hi_scale_lim == "") {
            setMax <- plate_lims[2]
        } else {setMax <- suppressWarnings(as.numeric(hi_scale_lim,na.rm = T))}
        return(c(setMin,setMax) )
    }
} #end of set_limits
