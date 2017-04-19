## test teh data and define conditions to start the app


#define reactiveValues objects required for data tests and control of conditional panels 
showApp <- reactiveValues(panels=F,dummy=T)
tabInput <- reactiveValues()
test_data <- reactiveValues(na=T,
                            numeric=F,
                            duplicateCols=F,
                            wellForm=F,
                            missingPlates=F,
                            multiFalse=F,
                            cellHTSform=T,
                            cellHTScols=F,
                            whiteSpace=F)
col2 <- reactiveValues()
TabDimensions <- reactiveValues(experiment=F,well=F,plate=F,annotation=F)
brush_container <- reactiveValues()
observer <- reactiveValues(click=NULL, click2=NULL)
showPopMan <- reactiveValues(state=NULL)

#invalidate all drop down lists upon changes of the user-input on the data upload panel
observeEvent(input$MeasuredValues,{
  showApp$panels = F
  showApp$dummy = T

  session$sendCustomMessage(type = "resetValue",
                            message = "featuresPlate1")

  session$sendCustomMessage(type = "resetValue",
                            message = "featuresPlate2")

  session$sendCustomMessage(type = "resetValue",
                            message = "featuresPlate3")

  session$sendCustomMessage(type = "resetValue",
                            message = "featuresPlate4")
  
  session$sendCustomMessage(type = "resetValue",
                            message = "platesQC")

  session$sendCustomMessage(type = "resetValue",
                            message = "feature_selection_ft")

  session$sendCustomMessage(type = "resetValue",
                            message = "feature_selection_qc")

  session$sendCustomMessage(type = "resetValue",
                            message = "feature_selection_sp_x")

  session$sendCustomMessage(type = "resetValue",
                            message = "feature_selection_sp_y")
})

observeEvent(input$ExperimentDimension,{
  showApp$panels = F
  showApp$dummy = T

  session$sendCustomMessage(type = "resetValue",
                            message = "screensPlate1")

  session$sendCustomMessage(type = "resetValue",
                            message = "screensPlate2")

  session$sendCustomMessage(type = "resetValue",
                            message = "screensPlate3")

  session$sendCustomMessage(type = "resetValue",
                            message = "screensPlate4")

  session$sendCustomMessage(type = "resetValue",
                            message = "screen_selection_qc")

  session$sendCustomMessage(type = "resetValue",
                            message = "screens_selection_sp_x")

  session$sendCustomMessage(type = "resetValue",
                            message = "screens_selection_sp_y")
  session$sendCustomMessage(type = "resetValue",
                            message = "featuresPlate1")
  
  session$sendCustomMessage(type = "resetValue",
                            message = "featuresPlate2")
  
  session$sendCustomMessage(type = "resetValue",
                            message = "featuresPlate3")
  
  session$sendCustomMessage(type = "resetValue",
                            message = "featuresPlate4")
  
  session$sendCustomMessage(type = "resetValue",
                            message = "feature_selection_ft")
  
  session$sendCustomMessage(type = "resetValue",
                            message = "feature_selection_qc")
  
  session$sendCustomMessage(type = "resetValue",
                            message = "feature_selection_sp_x")
  
  session$sendCustomMessage(type = "resetValue",
                            message = "feature_selection_sp_y")
})

observeEvent(input$PlateDimension,{
  showApp$panels = F
  showApp$dummy = T
  session$sendCustomMessage(type = "resetValue",
                            message = "platesPlate1")

  session$sendCustomMessage(type = "resetValue",
                            message = "platesPlate2")

  session$sendCustomMessage(type = "resetValue",
                            message = "platesPlate3")

  session$sendCustomMessage(type = "resetValue", message =
                                "platesPlate4")

})

observeEvent(input$AnnoDimension,{
  showApp$panels = F
  showApp$dummy = T
})


observeEvent(input$IsSingleExperiment,{
  showApp$panels = F
  showApp$dummy = T
})

observeEvent(input$ScreensOnColumns,{
  showApp$panels = F
  showApp$dummy = T
})



#ouput variable to show the action button to start the app
#(action button returns "input$startApp")
output$showStartButtons <- reactive({
  if(isTRUE(hideMeasuredValues$state) &
     !is.null(input$WellDimension)  &
     !is.null(input$PlateDimension)  &
     length(input$ExperimentDimension)>1 ){
        return(TRUE)
            }else{
                if(!isTRUE(hideMeasuredValues$state) &
                   !is.null(input$WellDimension) &
                   !is.null(input$PlateDimension) &
                   !is.null(input$ExperimentDimension) &
                   !is.null(input$MeasuredValues))
                {
                    return(TRUE)
                } else {
                        return(FALSE)
                    }
  }
})
outputOptions(output, "showStartButtons", suspendWhenHidden=FALSE)






#validate data upon triggering of action button "input$startApp"
# remove whitespaces from entire data set
#check table format if input file is cellHTS topTable
#if id variables contain factors, convert them to charcater
#rename columns according to internal annotation: well > plate > screen.id

observeEvent(input$startApp,{
  test_ft <- feature_table2$data_pre
  test_data$multiFalse = F
  test_data$cellHTSform = F
  test_data$cellHTScols = F
  
  testColnames <- gsub(" ",
                       "",
                       c(input$WellDimension,
                         input$PlateDimension,
                         input$ExperimentDimension,
                         input$AnnoDimension,
                         input$MeasuredValues),
                       fixed = TRUE)

  
  if(isTRUE(any(nchar(testColnames) != nchar(c(input$WellDimension,
                                               input$PlateDimension,
                                               input$ExperimentDimension,
                                               input$AnnoDimension,
                                               input$MeasuredValues))))) { 
    test_data$whiteSpace = T
  } else {

  if(isTRUE(hideMeasuredValues$state) &
     !is.null(input$WellDimension)  &
     !is.null(input$PlateDimension) &
     length(input$ExperimentDimension)>1 ){
      

            if( length(grep("r1_ch1",colnames(test_ft)))<1) {
                test_data$cellHTSform = T
            } else {
        
            cellHTSstring <- input$ExperimentDimension   
            #check if score column is present, if yes rename columns 
            if(length(grep("score",cellHTSstring))>0 ) {
                test_ft <- test_ft %>% rename(score_ch1 =score)
                cellHTSstring <- c("score_ch1",cellHTSstring[-which(cellHTSstring %in% "score")])
                if(length(grep("ch2",cellHTSstring))>0 ) {
                    test_ft <-test_ft %>% mutate(score_ch2=test_ft[,"score_ch1"])
                    cellHTSstring <- c("score_ch2",cellHTSstring)
                }
            }
            
            test_ft <-test_ft %>%
                        dplyr::select(one_of(input$WellDimension,
                                            input$PlateDimension,
                                            cellHTSstring,
                                            input$AnnoDimension)) %>%
                            gather_("CSvB110",
                                    "CSvalueB110",
                                    cellHTSstring) %>%
                            extract(CSvB110,
                                    c("experiment.id", "col"),
                                    regex="(.*)_([[:alnum:]]{3})")%>%
                                        spread(col, CSvalueB110) 
            #for dual channel experiments only 
            if(any(colnames(test_ft) %in% "ch2")) {
                ch1_cols <- length(grep("ch1",input$ExperimentDimension))-length(grep("normalized",input$ExperimentDimension))
                ch2_cols <- length(grep("ch2",input$ExperimentDimension))
                
                if(ch1_cols != ch2_cols) {
                    test_data$cellHTScols = T
                } else {
                    test_ft <-test_ft %>% mutate(ch2 = replace(ch2, 
                                           which(experiment.id%in%c("normalized_r1",
                                                                    "normalized_r2")), 
                                           test_ft[which(test_ft$experiment.id %in% c("normalized_r1","normalized_r2")),"ch1"]
                                            )
                                        )
                        } 
                }
          colnames_string <- c(input$WellDimension,
                               input$PlateDimension,
                               input$AnnoDimension,
                               "experiment.id")
          test_ft <-  test_ft %>% dplyr::mutate_each(funs(as.character),
                                                     one_of(colnames_string))
          feature_table2$data <- test_ft
        }
  } else {


  if( !is.null(input$WellDimension) &
      !is.null(input$PlateDimension) &
      !is.null(input$ExperimentDimension) &
      !is.null(input$MeasuredValues)) {
            test_ft <- feature_table2$data_pre
            colnames_string <- c(input$WellDimension,
                                 input$PlateDimension,
                                 input$ExperimentDimension,
                                 input$AnnoDimension)
        if(!is.null(input$IsSingleExperiment)){
            if(isTRUE(input$IsSingleExperiment)) {
                colnames_string <- colnames_string[-which(colnames_string==input$ExperimentDimension)]
                    test_ft <- test_ft[!duplicated(test_ft[,c(input$WellDimension,input$PlateDimension)]),]

        }
    }
    test_ft <-  test_ft %>%
                    dplyr::mutate_each(funs(as.character),
                                       one_of(colnames_string)) %>%
                        dplyr::select(one_of(colnames_string,
                                             input$MeasuredValues))
    feature_table2$data <- test_ft
        }
      }
    }
})



#validate well format
observeEvent(input$startApp,{
  validate(need(input$WellDimension, message=FALSE))
  validate(need(input$PlateDimension, message=FALSE))
  validate(need(input$ExperimentDimension, message=FALSE))

  test_data$wellForm <- F

  if(!isTRUE(test_data$multiFalse) & 
     !isTRUE(test_data$cellHTSform) & 
     !isTRUE(test_data$whiteSpace) &
     !isTRUE(test_data$cellHTScols)) {

    testWellForm <- feature_table2$data %>%
                        dplyr::select_(input$WellDimension) %>%
                            distinct
    if(nrow(testWellForm) > 384) {
      test_data$wellForm <- T
      return(test_data$wellForm)
      } else {

        testWellForm <- testWellForm %>%
            dplyr::mutate_(row=lazyeval::interp(~gsub("[^A-z]","",x),
                                x = as.name(input$WellDimension))) %>%
                dplyr::mutate_(column=lazyeval::interp(~gsub("[^0-9]","",x),
                                    x = as.name(input$WellDimension)))

    if(nrow(testWellForm) == 6) {
      if(any(nchar(testWellForm$row) == 0)|
         any(nchar(testWellForm$column) == 0) ) {
          test_data$wellForm <- T
      } else {
            if(length(testWellForm$row %>% unique) != 2) { test_data$wellForm <- T }
                    if(length(testWellForm$column %>% unique) != 3) {
                        test_data$wellForm <- T
                    }  else {
                        tabInput$rows <- 2
                        tabInput$cols <- 3
          }
      }
    } else {
        if(nrow(testWellForm) == 12) {
          if(any(nchar(testWellForm$row) == 0)|
             any(nchar(testWellForm$column) == 0)){
              test_data$wellForm <- T
            } else {
                if(nrow(testWellForm %>%
                            dplyr::select(row) %>%
                                distinct) != 3){
                    test_data$wellForm <- T}
                        if(nrow(testWellForm %>%
                                dplyr::select(column) %>%
                                    distinct) != 4) {
                            test_data$wellForm <- T
                        } else {
                            tabInput$rows <- 3
                            tabInput$cols <- 4
            }
          }
        } else {
            if(nrow(testWellForm) == 24) {
              if(any(nchar(testWellForm$row) == 0)|
                 any(nchar(testWellForm$column)==0) ) {test_data$wellForm <- T
              } else {
                if(nrow(testWellForm %>%
                            dplyr::select(row) %>%
                                distinct) != 4) {test_data$wellForm <- T}
                    if(nrow(testWellForm %>%
                            dplyr::select(column) %>%
                                distinct) != 6) {test_data$wellForm <- T
                    } else {
                        tabInput$rows <- 4
                            tabInput$cols <- 6
                }
              }
            } else {
                if(nrow(testWellForm) == 48) {
                  if(any(nchar(testWellForm$row)==0)|any(nchar(testWellForm$column)==0) ) {test_data$wellForm <- T
                  } else {
                    if(nrow(testWellForm %>% dplyr::select(row) %>% distinct) != 6) {test_data$wellForm <- T}
                    if(nrow(testWellForm%>% dplyr::select(column) %>% distinct) != 8) {test_data$wellForm <- T}
                    else {
                      tabInput$rows <- 6
                      tabInput$cols <- 8
                    }
                  }
                } else {
                    if(nrow(testWellForm) == 96) {
                      if(any(nchar(testWellForm$row)==0)|any(nchar(testWellForm$column)==0) ) {test_data$wellForm <- T
                      } else {
                        if(nrow(testWellForm %>% dplyr::select(row) %>% distinct) != 8) {test_data$wellForm <- T}
                        if(nrow(testWellForm%>% dplyr::select(column) %>% distinct) != 12) {test_data$wellForm <- T}
                        else {
                          tabInput$rows <- 8
                          tabInput$cols <- 12
                        }
                      }
                    } else {
                        if(nrow(testWellForm) == 384) {
                          if(any(nchar(testWellForm$row)==0)|
                             any(nchar(testWellForm$column)==0) ) {
                              test_data$wellForm <- T
                                } else {
                                    if(nrow(testWellForm %>%
                                                dplyr::select(row) %>%
                                                    distinct) != 16) {
                                        test_data$wellForm <- T}
                                        if(nrow(testWellForm %>%
                                                    dplyr::select(column) %>%
                                                        distinct) != 24) {
                                            test_data$wellForm <- T
                                        }   else {
                                            tabInput$rows <- 16
                                            tabInput$cols <- 24
                              }
                            }
                         } else {
                            test_data$wellForm <- T
                            tabInput$rows <- 1
                            tabInput$cols <-1 }
                        }
                    }
                }
            }
        }
     }
  }
})


observeEvent(input$startApp,{
  validate(need(input$WellDimension, message=FALSE))
  validate(need(input$PlateDimension, message=FALSE))
  validate(need(input$ExperimentDimension, message=FALSE))

  if(!isTRUE(test_data$multiFalse) &
     isTRUE(hideMeasuredValues$state) &
     !isTRUE(test_data$cellHTSform) &
     !isTRUE(test_data$cellHTScols) &
     !isTRUE(test_data$whiteSpace) &
     !is.null(input$WellDimension) &
     !is.null(input$PlateDimension) & 
     length(input$ExperimentDimension)>1 ) {
        tabInput$inputPlates <- mixedsort(
                                feature_table2$data %>%
                                    dplyr::distinct_(input$PlateDimension) %>%
                                    unlist(use.names = FALSE))
    tabInput$inputFeatures <- feature_table2$data %>%
                                    dplyr::select(-one_of(
                                        input$WellDimension,
                                      input$PlateDimension,
                                      input$AnnoDimension,
                                      "experiment.id")) %>%
                                                names
    tabInput$inputScreens <- feature_table2$data %>%
                                dplyr::distinct(experiment.id) %>%
                                    unlist(use.names = FALSE)
  } else {
    if(!is.null(input$WellDimension) &
       !is.null(input$PlateDimension) &
       !is.null(input$ExperimentDimension) &
       !is.null(input$MeasuredValues) &
       !isTRUE(test_data$whiteSpace)
       ){
        tabInput$inputFeatures=input$MeasuredValues
        tabInput$inputPlates <- mixedsort(
            feature_table2$data %>%
                dplyr::distinct_(input$PlateDimension) %>%
                    unlist(use.names = FALSE))
        if(!is.null(input$IsSingleExperiment)){
          if(isTRUE(input$IsSingleExperiment)) {
          tabInput$inputScreens="single_experiment"
          } else {
            tabInput$inputScreens=feature_table2$data %>%
                dplyr::distinct_(input$ExperimentDimension) %>%
                    unlist(use.names = FALSE)
              }
          }
    }
  }
})




#test the data
observeEvent(input$startApp,{
  validate(need(input$WellDimension, message=FALSE))
  validate(need(input$PlateDimension, message=FALSE))
  validate(need(input$ExperimentDimension, message=FALSE))
  colnames_string <- c(input$WellDimension,
                       input$PlateDimension,
                       input$AnnoDimension,
                       "experiment.id")

    test_data$na <- T
    test_data$numeric <- F
    test_data$duplicateCols <- F
    test_data$missingPlates <- F
  if(isTRUE(hideMeasuredValues$state) & 
     !isTRUE(test_data$multiFalse) & 
     !isTRUE(test_data$cellHTSform) & 
     !isTRUE(test_data$cellHTScols) & 
     !isTRUE(test_data$whiteSpace)){
    test_data$numeric <- any(!sapply(
                                feature_table2$data %>%
                                    dplyr::select(-one_of(colnames_string)),
                                                    is.numeric))
    test_data$na <- any(is.na(feature_table2$data %>%
                                  dplyr::select(-one_of(colnames_string)) ))


    test_data$duplicateCols <- any(duplicated(names(feature_table2$data)))
  } else {
      if(!isTRUE(test_data$multiFalse) & 
         !isTRUE(test_data$cellHTSform) & 
         !isTRUE(test_data$cellHTScols) &
         !isTRUE(test_data$whiteSpace)) {
      test_data$numeric <- any(!sapply(
                                    feature_table2$data %>%
                                        dplyr::select(
                                            one_of(input$MeasuredValues)),
                                                    is.numeric))

      test_data$na <- any(is.na(feature_table2$data %>%
                                    dplyr::select(
                                        one_of(input$MeasuredValues))))

      test_data$duplicateCols <- any(duplicated(c(input$WellDimension,
                                                  input$PlateDimension,
                                                  input$ExperimentDimension,
                                                  input$AnnoDimension,
                                                  input$MeasuredValues)))

      unique_plates <- length(
                            unique(feature_table2$data[,input$PlateDimension]))

      if(!is.null(input$IsSingleExperiment)){
        if(!isTRUE(input$IsSingleExperiment)) {
          test_data$missingPlates <- feature_table2$data %>%
                dplyr::group_by_(input$ExperimentDimension) %>%
                    dplyr::summarize_(
                        CSuni_platesB110 = lazyeval::interp(~n_distinct(var),
                            var = as.name(input$PlateDimension))) %>%
                        dplyr::mutate(
                            CSdiffB110 = CSuni_platesB110 != unique_plates) %>%
                                dplyr::select(CSdiffB110) %>%
                                    unlist(use.names = F) %>% any
        }
      }
    }
  }
    js_string <- 'alert("SOMETHING");'
    if(isTRUE(test_data$whiteSpace)) {
        warnWS <- "Column names and identifers can't contain whitespaces, please separate by e.g. '_'"
        warnWS_js_string <- sub("SOMETHING",warnWS,js_string)
        session$sendCustomMessage(type='jsCode', list(value = warnWS_js_string ))
    }else{
    if(isTRUE(test_data$cellHTScols)) {
        warnTTC <- "Please select the same columns for both channels of your cellHTS topTable (e.g. 'raw_r1_ch1' and 'raw_r1_ch2)"
        warnTTC_js_string <- sub("SOMETHING",warnTTC,js_string)
        session$sendCustomMessage(type='jsCode', list(value = warnTTC_js_string ))
    }else{
    if(isTRUE(test_data$cellHTSform)) {
        warnTT <- "Input data is not in 'cellHTS topTable' format"
        warnTT_js_string <- sub("SOMETHING",warnTT,js_string)
        session$sendCustomMessage(type='jsCode', list(value = warnTT_js_string ))
    }else{
    if(isTRUE(test_data$duplicateCols)) {
      warnDC <- "Columns can't be selected twice"
      warnDC_js_string <- sub("SOMETHING",warnDC,js_string)
      session$sendCustomMessage(type='jsCode', list(value = warnDC_js_string ))
    } else {
      if(isTRUE(test_data$wellForm)) {
        warnWF <- "Incorrect well dimension"
        warnWF_js_string <- sub("SOMETHING",warnWF,js_string)
        session$sendCustomMessage(type='jsCode', list(value = warnWF_js_string))
        } else {
          if(isTRUE(test_data$numeric)) {
            warnNum <- "Measured Values have to be numeric"
            warnNum_js_string <- sub("SOMETHING",
                                     warnNum,
                                     js_string)
            session$sendCustomMessage(type='jsCode',
                                      list(value = warnNum_js_string))
          } else  {
            if(isTRUE(test_data$missingPlates)) {
              warnMP <- paste("Error: Experiments do not",
                                "contain equal number of plates or plate identifiers between experiments not identical",sep = " ")
              warnMP_js_string <- sub("SOMETHING",warnMP,js_string)
              session$sendCustomMessage(type='jsCode',
                                        list(value = warnMP_js_string ))
          } else {
              if(isTRUE(test_data$na)) {
                warnNA <- paste("The data input contains missing values,",
                                "those will be ignored",
                                "in the shown representations",sep=" ")
                warnNA_js_string <- sub("SOMETHING",warnNA,js_string)
                session$sendCustomMessage(type='jsCode',
                                          list(value = warnNA_js_string ))
            } else{
              warnMP <- ""
              warnWF <- ""
              warnNum <- ""
              warnNA <- ""
              warnDC <- ""
              warnTT <- ""
              warnWS <- ""
              warnTTC <- ""
           }
          }
         }
        }
       }
      }
     }
    }
})


#Info to be shown when data input was successful
output$StartInfo1 <- renderUI({
  h4("Data input successful")
})
output$StartInfo2 <- renderUI({
  h6(paste(tabInput$rows*tabInput$cols,"well format detected",sep="-"))
})
output$StartInfo3 <- renderUI({
  h6("Data set with:")
})
output$StartInfo4 <- renderUI({
  h6(paste(length(tabInput$inputScreens),"Experiments",sep=" "))
})
output$StartInfo5 <- renderUI({
  h6(paste(length(tabInput$inputPlates),"Plates per experiment",sep=" "))
})
output$StartInfo6 <- renderUI({
  h6(paste(length(tabInput$inputFeatures),"Measured Values per well",sep=" "))
})
output$StartInfo7 <- renderUI({
    h4("You can now open the tabs")
})



observeEvent(input$startApp,{
  if(isTRUE(hideMeasuredValues$state) &
     !is.null(input$WellDimension)  &
     !is.null(input$PlateDimension) &
     length(input$ExperimentDimension)>1 ){
        if(isTRUE(any(test_data$numeric,
                      test_data$duplicateCols,
                      test_data$wellForm,
                      test_data$missingPlates,
                      test_data$multiFalse,
                      test_data$cellHTSform,
                      test_data$cellHTScols,
                      test_data$whiteSpace))) {
            showApp$panels = F
            showApp$dummy = T
            } else {
                showApp$panels = T
                showApp$dummy = F
    }
  }

  if(!is.null(input$WellDimension) &
     !is.null(input$PlateDimension) &
     !is.null(input$ExperimentDimension) &
     !is.null(input$MeasuredValues)) {
        if(isTRUE(any(test_data$numeric,
                      test_data$duplicateCols,
                      test_data$wellForm,
                      test_data$missingPlates,
                      test_data$multiFalse,
                      test_data$cellHTSform,
                      test_data$cellHTScols,
                      test_data$whiteSpace))) {
                            showApp$panels = F
                            showApp$dummy = T
            } else {
                showApp$panels = T
                showApp$dummy = F
    }
  }
})


#control of conditional panels
output$hidePanels <- reactive({
  return(showApp$dummy)
})
outputOptions(output, "hidePanels", suspendWhenHidden=FALSE)

output$showPanels <- reactive({
  return(showApp$panels)
})
outputOptions(output, "showPanels", suspendWhenHidden=FALSE)




# isolate dimensions to prevent a direct effect of changing them
observeEvent(input$startApp,{
  TabDimensions$well <- input$WellDimension
  TabDimensions$plate <- input$PlateDimension
  if(isTRUE(hideMeasuredValues$state)){
    TabDimensions$experiment <- "experiment.id"
    } else {TabDimensions$experiment <- input$ExperimentDimension}

  if(is.null(input$AnnoDimension)){
      TabDimensions$annotation <- input$WellDimension
        } else {TabDimensions$annotation <- input$AnnoDimension}
})

#reset scatter plot page upon troggering of input$startApp
observeEvent(input$startApp, {
    for(i in names(brush_container)) {
        brush_container[[i]] <- NULL
        testCheckbox[[i]] <- "off"
        geneTable$data <- data.frame()
    }
    observer$click <- NULL
    observer$click2 <- NULL

    showPopMan$state <- NULL
})



