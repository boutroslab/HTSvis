#######################################
#### server of quality control tab ####
#######################################

### reactiveValues object 'TabDimensions' defined in lauchApp.R 
### 3 quality control plots are generated: plate plot, desity plot and box plot

### Control populations are selected by clicking on corresponding wells of a 
### plotted heatmap 

### ReactiveValues objects:
### 'getWells' to catch the clicked wells, ATTENTION: col is defined in launchApp.R
### 'storeMeans' to store the mean of clicked wells per plate 

getWells <- reactiveValues()
storeMeans <- reactiveValues()
loadCtrlWells <- reactiveValues(state=F)


## observer to check if plate selection is set to "all"
## plateStateQC is a reactiveValues object to capture teh "all" selection
plateStateQC <- reactiveValues(state=F)
observeEvent(input$allPlates,{
    validate(need(input$feature_selection_qc, message=FALSE))
    validate(need(input$screen_selection_qc, message=FALSE))
    validate(need(input$platesQC, message=FALSE))
    
    if(isTRUE(input$allPlates)) {
        plateStateQC$state = T
        updateSelectInput(session, 
                          "platesQC",
                          choices = input$platesQC)
    } else {
        plateStateQC$state = F
        updateSelectInput(session, 
                          "platesQC",
                          choices = tabInput$inputPlates)
    }
})

observe({ 
    validate(need(tabInput$inputPlates, message=FALSE))
    for(i in tabInput$inputPlates) {
        getWells[[i]] <- list(rep("undefined", tabInput$rows*tabInput$cols),
                              paste0(rep(LETTERS[1:tabInput$rows], tabInput$cols),
                                     unlist(lapply(1:tabInput$rows, rep, tabInput$cols))),
                              rep("white",tabInput$rows*tabInput$cols),
                              rep(0.3,tabInput$rows*tabInput$cols)
        )
    }
})


observe({
    validate(need(tabInput$inputPlates, message=FALSE))
    inputPlates <- tabInput$inputPlates
    
    storeMeans$pos_plates <-  cbind.data.frame(
        plate=inputPlates,
        med=rep(0,length(inputPlates)),
        status=rep(NA,length(inputPlates)),
        color = rep(0,length(inputPlates)),
        cex = rep(0.0,length(inputPlates))
    )
    storeMeans$neg_plates <- cbind.data.frame(
        plate=inputPlates,
        med=rep(0,length(inputPlates)),
        status=rep(NA,length(inputPlates)),
        color = rep(0,length(inputPlates)),
        cex = rep(0.0,length(inputPlates))
    )
    storeMeans$nt_plates <- cbind.data.frame(
        plate=inputPlates,
        med=rep(0,length(inputPlates)),
        status=rep(NA,length(inputPlates)),
        color = rep(0,length(inputPlates)),
        cex = rep(0.0,length(inputPlates))
    )
})





## Heatmap to define control populations by clickin'
## a data frame is created and saved as a reactive using the function 'plotHeatmap'
df_qc <- reactive({
    validate(need(input$feature_selection_qc, message=FALSE))
    validate(need(input$screen_selection_qc, message=FALSE))
    validate(need(input$platesQC, message=FALSE))
    
    if(isTRUE(plateStateQC$state)) {
        
        
        allPlatesDummy <- feature_table2$data %>%
            dplyr::select_(TabDimensions$well) %>% unique() %>%
            dplyr::mutate_(row=lazyeval::interp(~gsub("[^A-z]","",x),
                                                x=as.name(TabDimensions$well))) %>%
            dplyr::mutate_(column=lazyeval::interp(~gsub("[^0-9]","",x),
                                                   x=as.name(TabDimensions$well))) %>%
            dplyr::mutate(value = 1) %>%
            dplyr::mutate(def.color = "white") 
        allPlatesDummy <- allPlatesDummy[gtools::mixedorder(allPlatesDummy[,TabDimensions$well]),]
        allPlatesDummy$column <- factor(
            as.character(as.numeric(allPlatesDummy$column)),
            levels=seq(1:n_distinct(allPlatesDummy$column))
        )
        allPlatesDummy$CSidB110 <- 1:nrow(allPlatesDummy)
        return(allPlatesDummy)
        
    } else {
        
        if(!isTRUE(IsSingleExperimentTabs$state)) {
            limits_qc <- feature_table2$data %>%
                dplyr::select_(TabDimensions$plate,
                               TabDimensions$experiment,
                               input$feature_selection_qc) %>%
                dplyr::filter_(lazyeval::interp(quote(x == y),
                                                x = as.name(TabDimensions$experiment),
                                                y = input$screen_selection_qc)) %>%
                dplyr::filter_(lazyeval::interp(quote(x == y),
                                                x = as.name(TabDimensions$plate),
                                                y = input$platesQC)) %>%
                dplyr::select_(input$feature_selection_qc) %>%
                do(funs=c(min(.,na.rm=T),
                          max(.,na.rm=T))) %>%
                unlist(use.names = F)
            if(is.finite(limits_qc)[1] & is.finite(limits_qc)[2]) {
                warnEmpty_js_string <- ''
                
                plotHeatmap(data_table=feature_table2$data,
                            limits=limits_qc,
                            curr_plate=input$platesQC,
                            curr_screen=input$screen_selection_qc,
                            curr_feature=input$feature_selection_qc,
                            plateDim=TabDimensions$plate,
                            expDim=TabDimensions$experiment,
                            wellDim=TabDimensions$well,
                            annoDim=TabDimensions$annotation)
            } else {
                warnEmpty_js_string <- 'alert("Selected column combination does not exist");'
                session$sendCustomMessage(type='jsCode',
                                          list(value = warnEmpty_js_string ))
                return(NULL)}
        } else {
            limits_qc <- feature_table2$data %>%
                select_(TabDimensions$plate,
                        "value"=input$feature_selection_qc) %>%
                filter_(lazyeval::interp(quote(x == y),
                                         x = as.name(TabDimensions$plate),
                                         y = input$platesQC)) %>%
                select_("value") %>%
                do(funs=c(min(.,na.rm=T)
                          ,max(.,na.rm=T))) %>%
                unlist(use.names = F)
            
            if(is.finite(limits_qc)[1] & is.finite(limits_qc)[2]) {
                warnEmpty_js_string <- ''
                
                plotHeatmap(data_table=feature_table2$data,
                            limits=limits_qc,
                            curr_plate=input$platesQC,
                            curr_screen=F,
                            curr_feature=input$feature_selection_qc,
                            plateDim=TabDimensions$plate,
                            expDim=TabDimensions$experiment,
                            wellDim=TabDimensions$well,
                            annoDim=TabDimensions$annotation)
            } else {
                warnEmpty_js_string <- 'alert("Selected column combination does not exist");'
                session$sendCustomMessage(type='jsCode',
                                          list(value = warnEmpty_js_string ))
                return(NULL)}
        }
        
    }
})


## action button to kill the clicked population(s)
observeEvent(input$resetControls,{
    for(i in tabInput$inputPlates) {
        getWells[[i]] <- list(rep("undefined", tabInput$rows*tabInput$cols),
                              paste0(rep(LETTERS[1:tabInput$rows], tabInput$cols),
                                     unlist(lapply(1:tabInput$rows, rep, tabInput$cols))),
                              rep("white",tabInput$rows*tabInput$cols),
                              rep(0.3,tabInput$rows*tabInput$cols)
        )
    }
    
    storeMeans$pos_plates <-  cbind.data.frame(
        plate=tabInput$inputPlates,
        med=rep(0,length(tabInput$inputPlates)),
        status=rep(NA,length(tabInput$inputPlates)),
        color = rep(0,length(tabInput$inputPlates)),
        cex = rep(0.0,length(tabInput$inputPlates))
    )
    
    storeMeans$neg_plates <- cbind.data.frame(
        plate=tabInput$inputPlates,
        med=rep(0,length(tabInput$inputPlates)),
        status=rep(NA,length(tabInput$inputPlates)),
        color = rep(0,length(tabInput$inputPlates)),
        cex = rep(0.0,length(tabInput$inputPlates))
    )
    
    storeMeans$nt_plates <- cbind.data.frame(
        plate=tabInput$inputPlates,
        med=rep(0,length(tabInput$inputPlates)),
        status=rep(NA,length(tabInput$inputPlates)),
        color = rep(0,length(tabInput$inputPlates)),
        cex = rep(0.0,length(tabInput$inputPlates))
    )
    
    for(i in names(pos_wellStore)) {
        pos_wellStore[[i]] <- NULL}
    
    for(i in names(neg_wellStore)) {
        neg_wellStore[[i]] <- NULL}
    
    for(i in names(nt_wellStore)) {
        nt_wellStore[[i]] <- NULL}
    
    for(i in names(data_ntWells)) {
        data_ntWells[[i]] <- NULL}
    
    for(i in names(data_posWells)) {
        data_posWells[[i]] <- NULL}
    
    for(i in names(data_negWells)) {
        data_negWells[[i]] <- NULL}
})


## observer to kill the clicked population(s) if 'all Plates' is set/unset
observeEvent(input$allPlates,{
    for(i in tabInput$inputPlates) {
        getWells[[i]] <- list(rep("undefined", tabInput$rows*tabInput$cols),
                              paste0(rep(LETTERS[1:tabInput$rows], tabInput$cols),
                                     unlist(lapply(1:tabInput$rows, rep, tabInput$cols))),
                              rep("white",tabInput$rows*tabInput$cols),
                              rep(0.3,tabInput$rows*tabInput$cols)
        )
    }
    
    storeMeans$pos_plates <-  cbind.data.frame(
        plate=tabInput$inputPlates,
        med=rep(0,length(tabInput$inputPlates)),
        status=rep(NA,length(tabInput$inputPlates)),
        color = rep(0,length(tabInput$inputPlates)),
        cex = rep(0.0,length(tabInput$inputPlates))
    )
    
    storeMeans$neg_plates <- cbind.data.frame(
        plate=tabInput$inputPlates,
        med=rep(0,length(tabInput$inputPlates)),
        status=rep(NA,length(tabInput$inputPlates)),
        color = rep(0,length(tabInput$inputPlates)),
        cex = rep(0.0,length(tabInput$inputPlates))
    )
    
    storeMeans$nt_plates <- cbind.data.frame(
        plate=tabInput$inputPlates,
        med=rep(0,length(tabInput$inputPlates)),
        status=rep(NA,length(tabInput$inputPlates)),
        color = rep(0,length(tabInput$inputPlates)),
        cex = rep(0.0,length(tabInput$inputPlates))
    )
    
    for(i in names( pos_wellStore)) {
        pos_wellStore[[i]] <- NULL
    }
    
    for(i in names( neg_wellStore)) {
        neg_wellStore[[i]] <- NULL
    }
    
    for(i in names( nt_wellStore)) {
        nt_wellStore[[i]] <- NULL
    }
    
    for(i in names(data_ntWells)) {
        data_ntWells[[i]] <- NULL
    }
    
    for(i in names(data_posWells)) {
        data_posWells[[i]] <- NULL
    }
    
    for(i in names(data_negWells)) {
        data_negWells[[i]] <- NULL
    }
})


## and observer to reset the col object  
observe({
    if(!is.null(input$screen_selection_qc)) {
        if(nchar(input$screen_selection_qc)>0){
            for(i in names(getWells)) {
                getWells[[i]][[3]][which(
                    getWells[[i]][[1]] == "undefined")] <- df_qc()$def.color[which(
                        getWells[[i]][[1]] == "undefined")]
            }
        }}
})

observe({
    if(!is.null(input$platesQC)) {
        if(nchar(input$platesQC)>0){
            for(i in names(getWells)) {
                getWells[[i]][[3]][which(
                    getWells[[i]][[1]] == "undefined")] <- df_qc()$def.color[which(
                        getWells[[i]][[1]] == "undefined")]
            }
        }}
})

observe({
    if(!is.null(input$feature_selection_qc)) {
        if(nchar(input$feature_selection_qc)>0){
            for(i in names(getWells)) {
                getWells[[i]][[3]][which(
                    getWells[[i]][[1]] == "undefined")] <- df_qc()$def.color[which(
                        getWells[[i]][[1]] == "undefined")]
            }
        }
    }
})


## function to select controls by clicking (is passed over to the plot)
## input$radio is a radio button to switch between the contol populations 
## function is called in ggvis heatmap below 
## ATTENTION: 'return(NULL)'
fu_qc_click <- reactive({print_out <- function(x) {
    if(is.null(x)) return(NULL) else
        isolate(
            if(getWells[[input$platesQC]][[1]][x$CSidB110] == "undefined") {
                if(input$radio == "pos") {
                    getWells[[input$platesQC]][[1]][x$CSidB110] = "positive"
                    getWells[[input$platesQC]][[3]][x$CSidB110] = pp_col
                    getWells[[input$platesQC]][[4]][x$CSidB110] = 1
                } else {
                    if(input$radio == "neg") {
                        getWells[[input$platesQC]][[1]][x$CSidB110] = "negative"
                        getWells[[input$platesQC]][[3]][x$CSidB110] = pn_col
                        getWells[[input$platesQC]][[4]][x$CSidB110] = 1 
                    } else {
                        if(input$radio == "nt") {
                            getWells[[input$platesQC]][[1]][x$CSidB110] = "nt"
                            getWells[[input$platesQC]][[3]][x$CSidB110] = nt_col
                            getWells[[input$platesQC]][[4]][x$CSidB110] = 1
                        }
                    }
                }
            } else {
                getWells[[input$platesQC]][[1]][x$CSidB110] = "undefined"
                getWells[[input$platesQC]][[3]][x$CSidB110] = df_qc()$def.color[x$CSidB110]
                getWells[[input$platesQC]][[4]][x$CSidB110] = 0.3
            }
        )
    return(NULL)
}
})


## function for hover over heatmap 
fu_qc_hover <- reactive({print_out <- function(x) {
    if(is.null(x)) return(NULL)
    
    if(isTRUE(plateStateQC$state)) {
        return(df_qc()[df_qc()$CSidB110 == x$CSidB110,TabDimensions$well])
    } else {
        if(TabDimensions$well == TabDimensions$annotation) {
            paste(df_qc()[df_qc()$CSidB110 == x$CSidB110,TabDimensions$well],
                  df_qc()[df_qc()$CSidB110 == x$CSidB110,"value" ],sep="<br />")
        } else {
            paste(df_qc()[df_qc()$CSidB110 == x$CSidB110,TabDimensions$well],
                  df_qc()[df_qc()$CSidB110 == x$CSidB110,"value" ],
                  df_qc()[df_qc()$CSidB110 == x$CSidB110,TabDimensions$annotation],sep="<br />") }
    }
}
})


## reactive values object to buffer reactivity before heatmap is plotted 
test_df_qc <- reactiveValues(state1=F,state2=F)

observe({
    validate(need(input$feature_selection_qc, message=FALSE))
    validate(need(input$platesQC, message=FALSE))
    if(!is.null(getWells[[input$platesQC]][[3]]))
        test_df_qc$state1 = T
})

observe({
    validate(need(input$screen_selection_qc, message=FALSE))
    validate(need(input$platesQC, message=FALSE))
    if(!is.null(getWells[[input$platesQC]][[3]]))
        test_df_qc$state2 = T
})


## plot the reactive heatmap 
observe({
    if(isTRUE(test_df_qc$state1) | isTRUE(test_df_qc$state2)) {
        df_qc %>%
            ggvis(~column,
                  ~row,
                  fill:=~getWells[[input$platesQC]][[3]],
                  fillOpacity:=~getWells[[input$platesQC]][[4]],
                  stroke :="black",
                  key :=~CSidB110)%>%
            add_tooltip(fu_qc_click(), "click") %>%
            add_tooltip(fu_qc_hover(), "hover")%>%
            layer_rects(width = band(), height = band())%>%
            scale_nominal("x", padding = 0, points = FALSE)%>%
            scale_nominal("y", padding = 0, points = FALSE)%>%
            add_axis("x",
                     title="",
                     tick_size_major=0,
                     properties=axis_props(
                         axis=list(stroke="white",
                                   strokeWidth=0),
                         grid=list(strokeWidth=0)))%>%
            add_axis("y",
                     title="",
                     tick_size_major=0,
                     properties=axis_props(
                         axis=list(stroke="white",
                                   strokeWidth=0),
                         grid=list(strokeWidth = 0)))%>%
            set_options(height = 300, width = 450) %>%
            bind_shiny("heatmap_qc")
    }
})

## after clicking the selected wells are stored in reactiveValues objects 
pos_wellStore <- reactiveValues()
neg_wellStore <- reactiveValues()
nt_wellStore <- reactiveValues()

observe({
    validate(need(input$feature_selection_qc, message=FALSE))
    validate(need(input$screen_selection_qc, message=FALSE))
    validate(need(input$platesQC, message=FALSE))
    
    pos_wells <- df_qc()[which(getWells[[input$platesQC]][[1]] == "positive"),TabDimensions$well]
    neg_wells <- df_qc()[which(getWells[[input$platesQC]][[1]] == "negative"),TabDimensions$well] 
    nt_wells <- df_qc()[which(getWells[[input$platesQC]][[1]] == "nt"),TabDimensions$well]
    
    if(length(pos_wells) > 0 ){
        pos_wellStore[[input$platesQC]] <- list(pos_wells)
    } else {
        pos_wellStore[[input$platesQC]] <- NULL}
    
    if(length(neg_wells) > 0 ){
        neg_wellStore[[input$platesQC]] <- list(neg_wells)
    } else {
        neg_wellStore[[input$platesQC]] <- NULL}
    
    if(length(nt_wells) > 0 ){
        nt_wellStore[[input$platesQC]] <- list(nt_wells)
    } else {
        nt_wellStore[[input$platesQC]] <- NULL}
})


## define reactive object with data according to drop down list selections
final <- reactive ({
    validate(need(input$feature_selection_qc, message=FALSE))
    validate(need(input$screen_selection_qc, message=FALSE))
    validate(need(input$platesQC, message=FALSE))
    
    if(!isTRUE(IsSingleExperimentTabs$state)) {
        returnFinal <- feature_table2$data %>%
            filter_(lazyeval::interp(quote(x == y),
                                     x = as.name(TabDimensions$experiment),
                                     y = input$screen_selection_qc)) %>%
            select_(TabDimensions$well,
                    TabDimensions$plate,
                    "value"=input$feature_selection_qc)
        return(returnFinal)
    } else {
        feature_table2$data %>%
            select_(TabDimensions$well,
                    TabDimensions$plate,
                    "value"=input$feature_selection_qc)
    }
})


## data of clicked wells is stored in reactive values objects 
data_ntWells <- reactiveValues()
observe({
    validate(need(input$feature_selection_qc, message=FALSE))
    validate(need(input$screen_selection_qc, message=FALSE))
    validate(need(input$platesQC, message=FALSE))
    
    if(isTRUE(plateStateQC$state)) {
        nt_plates <- as.character(storeMeans$nt_plates[,"plate"])
        if(length(nt_plates) > 0 ){
            for(i in nt_plates) {
                if(length(unlist(nt_wellStore[[i]]))>0){
                    data_ntWells[[i]] <- final()[which(final()[,TabDimensions$well] %in% unlist(nt_wellStore[[i]]) ),"value"]
                } else {
                    data_ntWells[[i]] <- NULL}
            }
        }
    } else {
        nt_plates <- as.character(storeMeans$nt_plates[which(!is.na(storeMeans$nt_plates$status)),"plate"])
        if(length(nt_plates) > 0 ){
            for(i in nt_plates) {
                if(length(unlist(nt_wellStore[[i]]))>0){
                    data_ntPlates <- final()[which(final()[,TabDimensions$plate] %in% i),]
                    data_ntPlatesTest <- data_ntPlates[which(data_ntPlates[,TabDimensions$well] %in% unlist(nt_wellStore[[i]]) ),"value"]
                    if(any(!is.na(data_ntPlatesTest)))
                        data_ntWells[[i]] <- data_ntPlatesTest
                } else {
                    data_ntWells[[i]] <- NULL}
            }
        }
    }
})

data_negWells <- reactiveValues()
observe({
    validate(need(input$feature_selection_qc, message=FALSE))
    validate(need(input$screen_selection_qc, message=FALSE))
    validate(need(input$platesQC, message=FALSE))
    
    if(isTRUE(plateStateQC$state)) {
        neg_plates <- as.character(storeMeans$neg_plates[,"plate"])
        if(length(neg_plates) > 0 ){
            for(i in neg_plates) {
                if(length(unlist(neg_wellStore[[i]]))>0){
                    data_negWells[[i]] <- final()[which(final()[,TabDimensions$well] %in% unlist(neg_wellStore[[i]]) ),"value"]
                } else {
                    data_negWells[[i]] <- NULL}
            }
        }
    } else {
        
        neg_plates <- as.character(storeMeans$neg_plates[which(!is.na(storeMeans$neg_plates$status)),"plate"])
        if(length(neg_plates) > 0 ){
            for(i in neg_plates) {
                if(length(unlist(neg_wellStore[[i]]))>0){
                    data_negPlates <- final()[which(final()[,TabDimensions$plate] %in% i),]
                    data_negPlatesTest <- data_negPlates[which(data_negPlates[,TabDimensions$well] %in% unlist(neg_wellStore[[i]]) ),"value"]
                    if(any(!is.na(data_negPlatesTest))) 
                        data_negWells[[i]] <- data_negPlatesTest
                } else {
                    data_negWells[[i]] <- NULL}
            }
        }
    }
})

data_posWells <- reactiveValues()
observe({
    validate(need(input$feature_selection_qc, message=FALSE))
    validate(need(input$screen_selection_qc, message=FALSE))
    validate(need(input$platesQC, message=FALSE))
    
    if(isTRUE(plateStateQC$state)) {
        pos_plates <- as.character(storeMeans$pos_plates[,"plate"])
        if(length(pos_plates) > 0 ){
            for(i in pos_plates) {
                if(length(unlist(pos_wellStore[[i]]))>0){
                    data_posWells[[i]] <- final()[which(final()[,TabDimensions$well] %in% unlist(pos_wellStore[[i]]) ),"value"]
                } else {
                    data_posWells[[i]] <- NULL}
            }
        }
    } else {
        pos_plates <- as.character(storeMeans$pos_plates[which(!is.na(storeMeans$pos_plates$status)),"plate"])
        if(length(pos_plates) > 0 ){
            for(i in pos_plates) {
                if(length(unlist(pos_wellStore[[i]]))>0){
                    data_posPlates <- final()[which(final()[,TabDimensions$plate] %in% i),]
                    data_posWellsTest <- data_posPlates[which(data_posPlates[,TabDimensions$well] %in% unlist(pos_wellStore[[i]]) ),"value"]
                    if(any(!is.na(data_posWellsTest))) 
                        data_posWells[[i]]   <- data_posWellsTest
                } else {
                    data_posWells[[i]] <- NULL}
            }
        }
    }
})


## assign reactive values object to show/hide the density and box plot
## those plots will only be drawn when the control populations are defined
##      this controlled via a reactiveValues object 
DummyPlots <- reactiveValues(dens=F,box=F)

observe({
    if(length(unlist(reactiveValuesToList(data_posWells))) >= 10 ||
       length(unlist(reactiveValuesToList(data_negWells))) >= 10 ||
       length(unlist(reactiveValuesToList(data_ntWells))) >= 10) {
        DummyPlots$dens <- T
    } else {
        DummyPlots$dens <- F
    }
})


##################
## Density Plot ##
##################
output$densityPlot <- renderPlot({
    validate(need(input$feature_selection_qc, message=FALSE))
    validate(need(input$screen_selection_qc, message=FALSE))
    densPlotOut()
})

densPlotOut <- function(){
    if(!isTRUE(DummyPlots$dens)){
        plot(1, type="n", axes=F, xlab="", ylab="" ,main = NA)
    }else{
        
        dens_sum <- list()
        dens_state <- vector()
        
        dens_sum$nt <- list()
        dens_sum$nt$x <- 0
        dens_sum$nt$y <- 0
        dens_state["nt"] <- NA
        
        dens_sum$neg <- list()
        dens_sum$neg$x <- 0
        dens_sum$neg$y <- 0
        dens_state["neg"] <- NA
        
        dens_sum$pos <- list()
        dens_sum$pos$x <- 0
        dens_sum$pos$y <- 0
        dens_state["pos"] <- NA
        
        legend_cols <- list()
        legend_string <- list()
        
        if(length(unlist(reactiveValuesToList(data_posWells))) >= 10 ) {
            dens_sum$pos <- suppressWarnings(density(unlist(reactiveValuesToList(data_posWells)),na.rm=T))
            sd_pos <- sd(unlist(reactiveValuesToList(data_posWells)),na.rm=T)
            mean_pos <- mean(unlist(reactiveValuesToList(data_posWells)),na.rm=T)
            legend_cols$pos <- pp_col
            legend_string$pos <- "positive controls"
            dens_state["pos"] <- 1
        } 
        
        if(length(unlist(reactiveValuesToList(data_negWells))) >= 10) {
            dens_sum$neg <- suppressWarnings(density(unlist(reactiveValuesToList(data_negWells)),na.rm=T))
            sd_neg <- sd(unlist(reactiveValuesToList(data_negWells)),na.rm=T)
            mean_neg <- mean(unlist(reactiveValuesToList(data_negWells)),na.rm=T)
            legend_cols$neg <- pn_col
            legend_string$neg <- "negative controls"
            dens_state["neg"] <- 1
        }
        
        if( length(unlist(reactiveValuesToList(data_ntWells))) >= 10) {
            dens_sum$nt <- suppressWarnings(density(unlist(reactiveValuesToList(data_ntWells)),na.rm=T))
            legend_cols$nt <- nt_col
            legend_string$nt <- "non-targeting controls"
            dens_state["nt"] <- 1
        }
        
        xmin <-  min(c(dens_sum$neg$x,dens_sum$pos$x,dens_sum$nt$x),na.rm = T)
        +(0-abs(min(c(dens_sum$neg$x,dens_sum$pos$x,dens_sum$nt$x),na.rm = T)*0.5))
        xmax <-  max(c(dens_sum$neg$x,dens_sum$pos$x,dens_sum$nt$x),na.rm = T)
        +(max(c(dens_sum$neg$x,dens_sum$pos$x,dens_sum$nt$x),na.rm = T)*0.5)
        ymin <- min(c(dens_sum$neg$y,dens_sum$pos$y,dens_sum$nt$y),na.rm = T)
        +(0-abs(min(c(dens_sum$neg$y,dens_sum$pos$y,dens_sum$nt$y),na.rm = T)*0.2))
        ymax <- max(c(dens_sum$neg$y,dens_sum$pos$y,dens_sum$nt$y),na.rm=T)
        +max(c(dens_sum$neg$y,dens_sum$pos$y,dens_sum$nt$y,na.rm=T)*0.2)
        
        if(length(unlist(reactiveValuesToList(data_posWells))) >= 10 && length(unlist(reactiveValuesToList(data_negWells))) >= 10 ) {
            z_factor <- round(
                (1-((3*(sd_neg+sd_pos))/(abs(mean_neg-mean_pos)))),
                2)
            z_factor_printout <- paste("Z'-factor",
                                       z_factor,
                                       sep=" : ")
            legend_string$z <- z_factor_printout
            legend_cols$z <- "black"
        }
        
        feature <- input$feature_selection_qc
        
        first <- names(which(!is.na(dens_state)))[1]
        
        plot(dens_sum[[first]],
             ylim=c(ymin,ymax),
             xlim =c(xmin,xmax),
             main=NA,
             col=unlist(legend_cols[[first]]),xlab=feature)
        
        if(length(names(which(!is.na(dens_state))))>1) {
            second <- names(which(!is.na(dens_state)))[2]
            lines(dens_sum[[second]],col = unlist(legend_cols[[second]]))
        }
        
        if(length(names(which(!is.na(dens_state))))>2) {
            third <- names(which(!is.na(dens_state)))[3]
            lines(dens_sum[[third]],col = unlist(legend_cols[[third]]))
        }
        title(main=substitute(
            paste(
                "Density Distributions of Controls for",
                "\n",italic(feature) ) ) )
        
        if(length(unlist(legend_string,use.names = F))>0) {
            legend("topleft",
                   pch=c(15,15,16),
                   col=unlist(legend_cols,use.names=F),
                   legend=unlist(legend_string,use.names = F))
        }
    }
}

## control conditional panel for dummy plots (when populations are undefined)
output$showDensPlots <- reactive({
    return(DummyPlots$dens)
})
outputOptions(output, "showDensPlots", suspendWhenHidden=FALSE)

output$hideDensPlots <- reactive({
    return(!DummyPlots$dens)
})
outputOptions(output, "hideDensPlots", suspendWhenHidden=FALSE)


#dummy text for density plot
output$textDensDummy <- renderUI(
    HTML(paste("
               <b> Density distribution (KDE) plot </b>",
               "Select control wells
               to create a density distribution (KDE) plot.",
               "Minimum 10 data points for positve and negative controls are
               required",
               sep="<br/>"))
)


################
## Plate Plot ##
################

## plot via function to make plots downloadable 
output$platePlot <- renderPlot({
    validate(need(input$feature_selection_qc, message=FALSE))
    validate(need(tabInput$inputPlates, message=FALSE))
    platePlotOut()
})

## function for plate plot 
## use the reactive object 'final' to get all sample values 
## use reactiveValues object 'storeMeans' to plot controls of means 
platePlotOut <- function(){
    #positive controls 
    if(isTRUE(plateStateQC$state)) {
        if(is.null(pos_wellStore[[input$platesQC]])) {
            storeMeans$pos_plates[,"med"] <- 0
            storeMeans$pos_plates[,"status"] <- NA
            storeMeans$pos_plates[,"color"] <- 0
            storeMeans$pos_plates[,"cex"] <- 0.0
            pos_meds <- storeMeans$pos_plates
            pos_meds[,"plate"] <- as.numeric(as.factor(pos_meds[,"plate"]))
        } else {
            posWellsMean <- unlist(reactiveValuesToList(pos_wellStore),use.names=F)
            posFrame <- cbind.data.frame(   unique(final()[,TabDimensions$plate]),
                                            rep(NA,length(unique(final()[,TabDimensions$plate]))))
            names(posFrame) <- c(TabDimensions$plate,"mean")
            sapply(seq_along(1:nrow(posFrame)),function(id,df,wells){
                plato <- posFrame[id,TabDimensions$plate]
                platos <- df[which(df[,TabDimensions$plate] %in% plato),]
                wellos <- platos[which(platos[,TabDimensions$well] %in% wells),]
                meanos <- mean(wellos[,"value"],na.rm=T)
                posFrame[id,"mean"] <<- meanos   
            },df=final(),wells=posWellsMean)
            storeMeans$pos_plates[,"med"] <- posFrame$mean
            storeMeans$pos_plates[,"plate"] <- posFrame[,TabDimensions$plate]
            storeMeans$pos_plates[,"status"] <- "positive"
            storeMeans$pos_plates[,"color"] <- pp_col
            storeMeans$pos_plates[,"cex"] <- 1
            pos_meds <- storeMeans$pos_plates
            pos_meds[,"plate"] <- as.numeric(as.factor(pos_meds[,"plate"]))
        }
    } else {
        if(is.null(pos_wellStore[[input$platesQC]])) {
            ind = which(storeMeans$pos_plates$plate %in% input$platesQC)
            storeMeans$pos_plates[ind,"med"] <- 0
            storeMeans$pos_plates[ind,"status"] <- NA
            storeMeans$pos_plates[ind,"color"] <- 0
            pos_meds <- storeMeans$pos_plates
            pos_meds[,"plate"] <- as.numeric(as.factor(pos_meds[,"plate"]))
        } else {
            x <- final()[with(final(),which(
                final()[,TabDimensions$plate] %in% input$platesQC)),]
            ind = which(storeMeans$pos_plates$plate %in% input$platesQC)
            storeMeans$pos_plates[ind,"med"] <- mean(x[with(x, which(
                x[,TabDimensions$well] %in% unlist(pos_wellStore[[input$platesQC]])) ),"value"],na.rm=T )
            storeMeans$pos_plates[ind,"status"] <- "positive"
            storeMeans$pos_plates[ind,"color"] <- pp_col
            storeMeans$pos_plates[ind,"cex"] <- 1
            pos_meds <- storeMeans$pos_plates
            pos_meds[,"plate"] <- as.numeric(as.factor(pos_meds[,"plate"]))
        }
    }
    #negative controls
    if(isTRUE(plateStateQC$state)) {
        if(is.null(neg_wellStore[[input$platesQC]])) {
            storeMeans$neg_plates[,"med"] <- 0
            storeMeans$neg_plates[,"status"] <- NA
            storeMeans$neg_plates[,"color"] <- 0
            storeMeans$neg_plates[,"cex"] <- 0.0
            neg_meds <- storeMeans$neg_plates
            neg_meds[,"plate"] <- as.numeric(as.factor(neg_meds[,"plate"]))
        } else {
            negWellsMean <- unlist(reactiveValuesToList(neg_wellStore),use.names=F)
            negFrame <- cbind.data.frame(   unique(final()[,TabDimensions$plate]),
                                            rep(NA,length(unique(final()[,TabDimensions$plate]))))
            names(negFrame) <- c(TabDimensions$plate,"mean")
            sapply(seq_along(1:nrow(negFrame)),function(id,df,wells){
                plato <- negFrame[id,TabDimensions$plate]
                platos <- df[which(df[,TabDimensions$plate] %in% plato),]
                wellos <- platos[which(platos[,TabDimensions$well] %in% wells),]
                meanos <- mean(wellos[,"value"],na.rm=T)
                negFrame[id,"mean"] <<- meanos   
            },df=final(),wells=negWellsMean)
            storeMeans$neg_plates[,"med"] <- negFrame$mean
            storeMeans$neg_plates[,"plate"] <- negFrame[,TabDimensions$plate]
            storeMeans$neg_plates[,"status"] <- "negative"
            storeMeans$neg_plates[,"color"] <- pn_col
            storeMeans$neg_plates[,"cex"] <- 1
            neg_meds <- storeMeans$neg_plates
            neg_meds[,"plate"] <- as.numeric(as.factor(neg_meds[,"plate"]))
        }
    } else {
        if(is.null(neg_wellStore[[input$platesQC]])) {
            ind = which(storeMeans$neg_plates$plate %in% input$platesQC)
            storeMeans$neg_plates[ind,"med"] <- 0
            storeMeans$neg_plates[ind,"status"] <- NA
            storeMeans$neg_plates[ind,"color"] <- 0
            neg_meds <- storeMeans$neg_plates
            neg_meds[,"plate"] <- as.numeric(as.factor(neg_meds[,"plate"]))
        } else {
            x <- final()[with(final(),which(
                final()[,TabDimensions$plate] %in% input$platesQC)),]
            ind = which(storeMeans$neg_plates$plate %in% input$platesQC)
            storeMeans$neg_plates[ind,"med"] <- mean(x[with(x, which(
                x[,TabDimensions$well] %in% unlist(neg_wellStore[[input$platesQC]])) ),"value"],na.rm=T )
            storeMeans$neg_plates[ind,"status"] <- "negative"
            storeMeans$neg_plates[ind,"color"] <- pn_col
            storeMeans$neg_plates[ind,"cex"] <- 1
            neg_meds <- storeMeans$neg_plates
            neg_meds[,"plate"] <- as.numeric(as.factor(neg_meds[,"plate"]))
        }
    }
    #non-targeting controls
    if(isTRUE(plateStateQC$state)) {
        if(is.null(nt_wellStore[[input$platesQC]])) {
            storeMeans$nt_plates[,"med"] <- 0
            storeMeans$nt_plates[,"status"] <- NA
            storeMeans$nt_plates[,"color"] <- 0
            storeMeans$nt_plates[,"cex"] <- 0.0
            nt_meds <- storeMeans$nt_plates
            nt_meds[,"plate"] <- as.numeric(as.factor(nt_meds[,"plate"]))
        } else {
            ntWellsMean <- unlist(reactiveValuesToList(nt_wellStore),use.names=F)
            ntFrame <- cbind.data.frame(   unique(final()[,TabDimensions$plate]),
                                           rep(NA,length(unique(final()[,TabDimensions$plate]))))
            names(ntFrame) <- c(TabDimensions$plate,"mean")
            sapply(seq_along(1:nrow(ntFrame)),function(id,df,wells){
                plato <- ntFrame[id,TabDimensions$plate]
                platos <- df[which(df[,TabDimensions$plate] %in% plato),]
                wellos <- platos[which(platos[,TabDimensions$well] %in% wells),]
                meanos <- mean(wellos[,"value"],na.rm=T)
                ntFrame[id,"mean"] <<- meanos   
            },df=final(),wells=ntWellsMean)
            
            storeMeans$nt_plates[,"med"] <- ntFrame$mean
            storeMeans$nt_plates[,"plate"] <- ntFrame[,TabDimensions$plate]
            storeMeans$nt_plates[,"status"] <- "non-targeting"
            storeMeans$nt_plates[,"color"] <- nt_col
            storeMeans$nt_plates[,"cex"] <- 1
            nt_meds <- storeMeans$nt_plates
            nt_meds[,"plate"] <- as.numeric(as.factor(nt_meds[,"plate"]))
        }
    } else {
        if(is.null(nt_wellStore[[input$platesQC]])) {
            ind = which(storeMeans$nt_plates$plate %in% input$platesQC)
            storeMeans$nt_plates[ind,"med"] <- 0
            storeMeans$nt_plates[ind,"status"] <- NA
            storeMeans$nt_plates[ind,"color"] <- 0
            nt_meds <- storeMeans$nt_plates
            nt_meds[,"plate"] <- as.numeric(as.factor(nt_meds[,"plate"]))
        } else {
            x <- final()[with(final(),which(
                final()[,TabDimensions$plate] %in% input$platesQC)),]
            ind = which(storeMeans$nt_plates$plate %in% input$platesQC)
            storeMeans$nt_plates[ind,"med"] <- mean(x[with(x, which(
                x[,TabDimensions$well] %in% unlist(nt_wellStore[[input$platesQC]])) ),"value"],na.rm=T )
            storeMeans$nt_plates[ind,"status"] <- "non-targeting"
            storeMeans$nt_plates[ind,"color"] <- nt_col
            storeMeans$nt_plates[ind,"cex"] <- 1
            nt_meds <- storeMeans$nt_plates
            nt_meds[,"plate"] <- as.numeric(as.factor(nt_meds[,"plate"]))
        }
    }
    
    sample_values = cbind.data.frame(plate = final()[,TabDimensions$plate],
                                     med = final()$value,
                                     color = rep("black",nrow(final())),
                                     status = rep("sample",nrow(final()))
    )
    
    sample_values[,"plate"] <- as.numeric(as.factor(sample_values[,"plate"]))
    
    ymax <- max(
        sample_values$med,na.rm = T)+(max(
            sample_values$med,na.rm = T)*0.2)
    ymin <- min(
        sample_values$med,na.rm = T)+(0-abs(
            min(
                sample_values$med,na.rm = T)*0.2))
    
    feature_mp <- as.character(input$feature_selection_qc)
    
    
    meds_total <- rbind.data.frame(pos_meds,neg_meds,nt_meds)
    
    
    plot(med ~as.numeric(plate),
         data=meds_total,
         col=meds_total$color,
         xaxt="n",
         pch=16,
         cex=meds_total$cex,
         ylim=c(ymin,ymax),
         type="p",
         ylab=substitute(italic(feature_mp)~"per plate"),
         xlab = "plate index")
    axis(1, at = 1:(length(unique(sample_values$plate))))
    points(sample_values,col = "black", pch = 19,cex = 0.1)
    
}


################
##  Box Plot  ##
################
posWells_frame <- reactive({
    if( is.null(unlist(reactiveValuesToList(data_posWells))) ) {
        p <- NULL
        return(p)} else {
            p <- cbind.data.frame(value=as.numeric(unlist(reactiveValuesToList(data_posWells))),
                                  status=as.factor("positive")
            )
            return(p)}
})


negWells_frame <- reactive({
    if( is.null(unlist(reactiveValuesToList(data_negWells))) ) {
        n <- NULL
        return(n)} else {
            n <- cbind.data.frame(value=as.numeric(unlist(reactiveValuesToList(data_negWells))),
                                  status=as.factor("negative")
            )
            return(n)}
})


ntWells_frame <- reactive({
    if( is.null(unlist(reactiveValuesToList(data_ntWells))) ) {
        nt <- NULL
        return(nt)} else {
            nt <- cbind.data.frame(value=as.numeric(unlist(reactiveValuesToList(data_ntWells))),
                                   status=as.factor("non-targeting")
            )
            return(nt)}
})

total <- reactive({
    
    tot <- rbind.data.frame(posWells_frame(),
                            negWells_frame(),
                            ntWells_frame()
    )
    return(tot) })

#control conditional panel for box plot
observe({
    if(is.null(unlist(reactiveValuesToList(data_posWells)))   &
       is.null(unlist(reactiveValuesToList(data_negWells)))  &
       is.null(unlist(reactiveValuesToList(data_ntWells)))) {
        DummyPlots$box <- F
    } else {
        DummyPlots$box <- T
    }
})


output$boxPlot <- renderPlot({
    validate(need(input$feature_selection_qc, message=FALSE))
    validate(need(input$screen_selection_qc, message=FALSE))
    boxPlotOut()
})

boxPlotOut <- function(){
    if(!isTRUE(DummyPlots$box)){
        plot(1, type="n", axes=F, xlab="", ylab="" ,main = NA)
    } else {
        x_pos_nt <- as.numeric(which(
            levels(total()$status) == "non-targeting") )
        x_pos_pp <- as.numeric(which(
            levels(total()$status) == "positive") )
        x_pos_pn <- as.numeric(which(
            levels(total()$status) == "negative") )
        feature_bp <- as.character(input$feature_selection_qc)
        
        if(nrow(total() %>% na.omit)>0) {
            bp_ctrls <- boxplot(value~as.factor(status),
                                data=total(),
                                ylab=substitute(italic(feature_bp)~"per screen"),
                                labels=levels(total()$status),
                                plot = F)
            bxp(bp_ctrls,show.names=T)
            points(rep(x_pos_nt,length(ntWells_frame()$value)),ntWells_frame()$value,col = nt_col)
            points(rep(x_pos_pn,length(negWells_frame()$value)),negWells_frame()$value,col = pn_col)
            points(rep(x_pos_pp,length(posWells_frame()$value)),posWells_frame()$value,col = pp_col)
        }
    }
}


##downlaod function for qc plots 
output$downloadPlotQC <- downloadHandler(
    filename = function() {
        paste(input$fileNamePlotQC,".pdf",sep="")
    },
    content = function(file) {
        pdf(file)
        platePlotOut() 
        densPlotOut() 
        boxPlotOut() 
        dev.off()
    }
)


#control conditional panel for dummy plots (when populations are undefined)
output$showBoxPlots <- reactive({
    return(DummyPlots$box)
})
outputOptions(output, "showBoxPlots", suspendWhenHidden=FALSE)


output$hideBoxPlots <- reactive({
    return(!DummyPlots$box)
})
outputOptions(output, "hideBoxPlots", suspendWhenHidden=FALSE)


#dummy text for box plot
output$textBoxDummy <- renderUI(
    HTML(paste("
               <b> Box plot </b>",
               "Select control wells
               to create a box plot.",
               sep="<br/>"))
)
