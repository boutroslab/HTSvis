####Feature Table##############################################################################################################

features_Table <- reactiveValues(selected_features=0)
catchAllRows <- reactiveValues(state=F)
row_selection <- reactiveValues()


observeEvent(input$feature_selection_ft,{
    features_Table$selected_features = input$feature_selection_ft
})

observeEvent(input$selectAllFeatures,{
    features_Table$selected_features = tabInput$inputFeatures
    updateSelectInput(session, 
                      "feature_selection_ft",
                      selected = tabInput$inputFeatures)
})



feature_table_print <-   reactive({
    validate(need(input$feature_selection_ft, message=FALSE))
    if(!is.null(input$IsSingleExperiment)){
        if(isTRUE(input$IsSingleExperiment)) {
            feature_table2$data %>%
                select(one_of(c(TabDimensions$annotation,
                                TabDimensions$well,
                                TabDimensions$plate,
                                features_Table$selected_features))) %>%
                mutate_each_(
                    funs(round(.,digits=3)),
                    paste0("-",c(TabDimensions$annotation,
                                 TabDimensions$plate,
                                 TabDimensions$well))) %>% 
                mutate(rowsFT=seq(1:nrow(.)))
        } else {
            feature_table2$data %>%
                select(one_of(c(TabDimensions$annotation,
                                TabDimensions$well,
                                TabDimensions$plate,
                                TabDimensions$experiment,
                                features_Table$selected_features))) %>%
                mutate_each_(funs(round(.,digits=3)),
                             paste0("-",c(TabDimensions$annotation,
                                          TabDimensions$plate,
                                          TabDimensions$well,
                                          TabDimensions$experiment))) %>% 
                mutate(rowsFT=seq(1:nrow(.)))
        }
    }
})



output$featureTable <- DT::renderDataTable ({
    datatable(
        feature_table_print() %>% select(-rowsFT),
        rownames = T, filter = "top",
        selection = list(mode='multiple',
                         selected = row_selection$rowsFromButton)
    )
},server=T)


proxy = dataTableProxy('featureTable')

observeEvent(input$resetSelection, {
    selectRows(proxy, NULL)
    row_selection$rows <- c()
    row_selection$rowsFromButton <- c()
    catchAllRows$state <- F
})


observeEvent(input$feature_selection_ft,{
    replaceData(proxy, feature_table_print(), resetPaging = FALSE)
})


#subsetted feature table as shwon in app as reactive object
#filter selection is accessible via input$..._search_columns
#check for filters and asign value to all positions where no filter is set
#j th element of vectors matches j th column of feature table
#check if filter element is numeric
#numeric values define a range separated by ...
feature_table_save <- reactive({
    validate(need(input$feature_selection_ft, message=FALSE))
    
    selection_vector <- input$featureTable_search_columns
    for(i in 1:length(selection_vector)) {
        if(selection_vector[i] == "")
            selection_vector[i] <- "no_filter_set"    }
    row_indices <- list()
    filtered_cols <- which(selection_vector != "no_filter_set" )
    if(length(filtered_cols) == 0) {
        feature_table_final <- feature_table_print()
    } else {
        for(j in filtered_cols) {
            numeric_test <-unlist(strsplit(selection_vector[j]," ... "))
            if(numeric_test[2] %in% NA) {
                filter_element <- gsub("[^[:alnum:]]",
                                       "",
                                       selection_vector[j])
                row_indices[[as.character(j)]] <- grep(
                    filter_element,
                    feature_table_print()[,j],
                    ignore.case=T)
            } else {
                row_indices[[as.character(j)]] <- intersect(
                    which(feature_table_print()[,j] > as.numeric(numeric_test[1])),
                    which(feature_table_print()[,j] < as.numeric(numeric_test[2]))
                )
            }
        }
        inter <- Reduce(intersect, row_indices)
        
        if(!is.null(input$IsSingleExperiment)){
            if(isTRUE(input$IsSingleExperiment)) {
                feature_table_final <- feature_table_print()[inter,
                                                             c(TabDimensions$annotation,
                                                               TabDimensions$well,
                                                               TabDimensions$plate,
                                                               features_Table$selected_features,
                                                               "rowsFT")]
            } else {
                feature_table_final <- feature_table_print() %>% 
                    slice(inter) %>% 
                    select(one_of( c(TabDimensions$annotation,
                                     TabDimensions$well,
                                     TabDimensions$plate,
                                     TabDimensions$experiment,
                                     features_Table$selected_features,
                                     "rowsFT")))
                
                
            }
        }
        return(feature_table_final)
    }
}) #end of reactive


observe({
    validate(need(input$feature_selection_ft, message=FALSE))
    validate(need(input$featureTable_search_columns, message=FALSE))
    if(!is.null(feature_table_save())) {
        catchAllRows$state=F
    } 
})


observeEvent(input$selectAllRows,{
    catchAllRows$state=T
})


fileNameFeatureTable <-  reactive({
    if(nchar(input$filnameFeatureTable) == 0) {
        return("filename")
    } else {
        return(input$filnameFeatureTable)}
})



#save subsetted feature table (file name as reactive input from ui.R)
output$downloadFeatureTable <- downloadHandler (
    filename = function() {
        paste(fileNameFeatureTable(),
              "csv",
              sep=".") },
    content = function(file) {
        write.table(feature_table_save() %>% select(-rowsFT),
                    file,
                    row.names = F,
                    sep=",",
                    dec ="."
        )}
)



##Heatmap of feature vector for clicked rows

# clicked rows in reactive element input$..._rows_selected
observe({
    validate(need(input$feature_selection_ft, message=FALSE))
    if(isTRUE(catchAllRows$state) && nrow(feature_table_save())<500) {
        row_selection$rowsFromButton <- c(isolate(row_selection$rowsFromButton),
                                          feature_table_save() %>% select(rowsFT) %>% unlist(use.names=F))
    } 
})




# heatmap from clicked rows
# dfHeatmap reactive data frame of clicked rows
dfHeatmap <- reactive({
    validate(need(input$feature_selection_ft, message=FALSE))
    if(length(features_Table$selected_features)<2) {
        return(NULL)
    } else {
        rows <- input$featureTable_rows_selected
        if(length(rows)>1) {
            df_heatmap <- rbind.data.frame(
                feature_table2$data[rows ,features_Table$selected_features])
            if(TabDimensions$annotation %in% TabDimensions$well) {
                rownames  <- paste(feature_table2$data[rows ,TabDimensions$well],
                                   feature_table2$data[rows ,TabDimensions$experiment],
                                   sep = "_")
            } else {
                rownames  <- paste(feature_table2$data[rows ,TabDimensions$well],
                                   feature_table2$data[rows ,TabDimensions$annotation],
                                   feature_table2$data[rows ,TabDimensions$experiment],
                                   sep = "_")
            }
            test_duplicates  <- duplicated(rownames)
            indices_duplicates  <- which(test_duplicates)
            if(isTRUE(any(test_duplicates))) {
                counter  <-  seq(1:length(indices_duplicates))
                together  <- cbind(indices_duplicates,counter)
                for (i in 1:length(indices_duplicates)) {
                    indis  <- together[i,"indices_duplicates"]
                    appendix_duplicates  <- together[i,"counter"]
                    rownames[indis]  <- paste(
                        rownames[indis],
                        appendix_duplicates,
                        sep="_")
                }
                rownames(df_heatmap) <- rownames
                return(df_heatmap)
            } else {
                rownames(df_heatmap) <- rownames
                return(df_heatmap)
            }
        } else {
            return(NULL)
        }
    }
})



dfHeatmapSize <- function(){
    validate(need(input$feature_selection_ft, message=FALSE))
    if(is.null(dfHeatmap())){
        return(400)
    } else {
        length_hp <- nrow(dfHeatmap())
        if(length_hp < 6){
            return(400)
        } else {
            if(length_hp < 12) {
                return(500)
            } else {
                return(600)
            }
        }
    }
}


FTheatmap_plot <- function(){
    validate(need(input$feature_selection_ft, message=FALSE))
    if(is.null(dfHeatmap())) {plot(0,
                                   xaxt='n',
                                   yaxt='n',
                                   bty='n',
                                   pch='',
                                   ylab='',xlab='')}
    else {
        
        min_dfHeatmap <- min(dfHeatmap(),na.rm = T)
        max_dfHeatmap <- max(dfHeatmap(),na.rm = T)
        breaks_dfHeatmap <- sort(
            c( seq( min_dfHeatmap - (max_dfHeatmap-min_dfHeatmap)/(500),
                    max_dfHeatmap + (max_dfHeatmap-min_dfHeatmap)/(500),
                    (max_dfHeatmap-min_dfHeatmap)/(500-1)
            )
            )
        )
        
        dfHeatmap_test <- dfHeatmap()[rowSums(is.na(dfHeatmap())) != ncol(dfHeatmap()),]
        doCluster <- c("both",TRUE,TRUE)
        if( any(sapply(dfHeatmap(), function(x)all(is.na(x))) )  |
            nrow(dfHeatmap()) != nrow(dfHeatmap_test)) {
            doCluster <- c("none",F,F)}
        
        keySize <- vector()
        ifelse(nrow(dfHeatmap()) < 6, keySize<-1.25,
               ifelse(nrow(dfHeatmap())< 12, keySize<-1, keySize<-0.75 ))
        
        kexSize <- vector()
        ifelse(nrow(dfHeatmap()) < 6, kexSize<-1.5,
               ifelse(nrow(dfHeatmap())< 12, kexSize<-1, 
                      ifelse(nrow(dfHeatmap())< 50, kexSize<-0.75, 
                             ifelse(nrow(dfHeatmap()) < 100, kexSize<-0.5,kexSize<-0.25 ))))
        
        if(input$scaleFTheatmap %in% "none"){
            heatmap.2(as.matrix(dfHeatmap()),
                      col = col_dfHeatmap,
                      breaks = breaks_dfHeatmap,
                      dendrogram = doCluster[1],
                      Rowv = as.logical(doCluster[2]),
                      Colv = as.logical(doCluster[3]),
                      na.color="black",
                      density.info="none",
                      trace = "none",
                      cexCol = ifelse(kexSize < 1, kexSize*2,kexSize),
                      cexRow = kexSize,
                      lmat=rbind(c(4,3),c(2,1)),
                      lhei=c(keySize,4),
                      lwid=c(1,4),
                      margins = c(20,20),
                      colsep = 1:ncol(dfHeatmap()),
                      rowsep = 1:nrow(dfHeatmap()),
                      sepcolor = "grey",
                      sepwidth = c(0.01,0.05),
                      scale = as.character(input$scaleFTheatmap))
        } else {
            heatmap.2(as.matrix(dfHeatmap()),
                      col = col_dfHeatmap,
                      dendrogram = doCluster[1],
                      Rowv = as.logical(doCluster[2]),
                      Colv = as.logical(doCluster[3]),
                      na.color="black",
                      density.info="none",
                      trace = "none",
                      cexCol = kexSize,
                      cexRow = kexSize,
                      lmat=rbind(c(4,3),c(2,1)),
                      lhei=c(keySize,4),
                      lwid=c(1,4),
                      margins = c(20,20),
                      colsep = 1:ncol(dfHeatmap()),
                      rowsep = 1:nrow(dfHeatmap()),
                      sepcolor = "grey",
                      sepwidth = c(0.01,0.05),
                      scale = as.character(input$scaleFTheatmap)
            )
        }
    }
}#end of fucntion


#plot feature table heatmap
output$featureHeatmap <- renderPlot ({
    FTheatmap_plot()
},height = dfHeatmapSize
) #end of renderPlot


#downlaod function for feature table heatmap
output$downloadFTheatmap <- downloadHandler(
    filename = function() {
        paste(input$fileNameFTheatmap,input$fileFormatFTheatmap,sep="")
    },
    content = function(file) {
        if(input$fileFormatFTheatmap %in% ".png"){
            png(file,
                height=dfHeatmapSize(),
                width=dfHeatmapSize()*2.5,
                units="px")
            FTheatmap_plot()
            dev.off()
        }else{
            if(input$fileFormatFTheatmap %in% ".tiff"){
                tiff(file,
                     height=dfHeatmapSize(),
                     width=dfHeatmapSize()*2.5,
                     units="px")
                FTheatmap_plot()
                dev.off()
            }else{
                if(input$fileFormatFTheatmap %in% ".jpeg"){
                    jpeg(file,height=dfHeatmapSize(),
                         width=dfHeatmapSize()*2.5,
                         units="px")
                    FTheatmap_plot()
                    dev.off()
                }
            }
        }
    }
)#end of download function

#control of conditional panel for heatmap dummy
DummyFt <- reactiveValues(state=F,state2=F)

observe({
    if(!is.null(dfHeatmap())) {
        DummyFt$state = T
    } else {
        DummyFt$state = F
    }
})

observe({
    validate(need(input$feature_selection_ft, message=FALSE))
    if(length(features_Table$selected_features)>1)
        DummyFt$state2 <- T
})


output$hideFt <- reactive({
    return(!DummyFt$state)
})
outputOptions(output, "hideFt", suspendWhenHidden=FALSE)


output$showFt <- reactive({
    return(DummyFt$state)
})
outputOptions(output, "showFt", suspendWhenHidden=FALSE)


output$showSelectAll <- reactive({
    return(DummyFt$state2)
})
outputOptions(output, "showSelectAll", suspendWhenHidden=FALSE)


#dummy text for heatmap
output$textFtDummy <- renderUI(
    HTML(paste("
               <b> Select rows (by clicking) to create a heatmap </b>",
               "(a heatmap can only be created with more than one measured value
               per well)",
               sep="<br/>"))
)






