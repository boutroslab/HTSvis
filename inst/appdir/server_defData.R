## Drop Downs and Tables of Overview Panel 
source("./DropDownInput.R",local=T)

hideMeasuredValues <- reactiveValues(state=F)

overviewTablesOptions = list(
  searching = FALSE, 
  bFilter = FALSE,
  bInfo = FALSE,
  bLengthChange = FALSE,
  bSort = FALSE,
  aLengthMenu = 5, 
  iDisplayLength = 5)



#Dropdown lists
#usage of custom wrapper function 'DropDownInput'
 

observe({
  validate(need(input$file1, message=FALSE))
    output$WellDimensionOut <-  DropDownInput(
                                    identifier="WellDimension",
                                    input=colnames(feature_table2$data_pre), 
                                    label=HTML(paste("Choose",
                                               "<em> well </em>",
                                               "<br/> column")),
                                    multiState=F)
})


observe({
  validate(need(input$file1, message=FALSE))
    output$PlateDimensionOut <-  DropDownInput(
                                    identifier="PlateDimension",
                                    input=colnames(feature_table2$data_pre),
                                    label=HTML(paste("Choose",
                                               "<em> plate </em>",
                                               "<br/> column")),
                                    multiState=F)  
})


observe({
  validate(need(input$file1, message=FALSE))
    output$ExperimentDimensionOut <-  DropDownInput(
                                        identifier="ExperimentDimension",
                                        input=colnames(feature_table2$data_pre),
                                        label=HTML(paste("Choose",
                                                   "<em> experiment </em>",
                                                   "<br/> column")),
                                        multiState=F)
})

observe({
  validate(need(input$file1, message=FALSE))
    output$MeasuredValuesOut <-  DropDownInput(
                                    identifier="MeasuredValues",
                                    input=colnames(feature_table2$data_pre),
                                    label=HTML(paste("Choose",
                                       "<em> feature </em>",
                                       "<br/>column(s)")),
                                    multiState=T)  
})

observe({
    validate(need(input$file1, message=FALSE))
        output$AnnoDimensionOut <-  DropDownInput(
                                        identifier= "AnnoDimension", 
                                        input=colnames(feature_table2$data_pre), 
                                        label=HTML(paste("Choose",
                                       "<em> annotation </em>",
                                       "<br/>column")),
                                       multiState=F)
})



#Tables 
observe({
  validate(need(input$WellDimension, message=FALSE))
  InputStartTables$well <- input$WellDimension
})

observe({
  validate(need(input$PlateDimension, message=FALSE))
  InputStartTables$plate <- input$PlateDimension
})  

observe({
  validate(need(input$ExperimentDimension, message=FALSE))
  InputStartTables$experiment <- input$ExperimentDimension
})

  observe({  
  validate(need(input$AnnoDimension, message=FALSE))
    InputStartTables$anno <- input$AnnoDimension
})


output$definedWells <- DT::renderDataTable({
  validate(need(input$WellDimension, message=FALSE))
  if(!is.null(InputStartTables$well)) {
    well_table <-   feature_table2$data_pre %>% 
                            distinct_(InputStartTables$well) %>% 
                                unlist(use.names=F) %>% mixedsort 
    well_table <- data.frame(well_table)
    names(well_table) <- names(feature_table2$data_pre %>% 
                                   distinct_(InputStartTables$well))
    return(well_table)

  }else{
      data.frame()
      }
},   
selection = 'none',rownames=F,options=overviewTablesOptions)

output$definedPlates <- DT::renderDataTable({
  validate(need(input$PlateDimension, message=FALSE))
  if(!is.null(InputStartTables$plate )) {
    data.frame(feature_table2$data_pre %>% distinct_(InputStartTables$plate))
  }else{
      data.frame()
      }
},   
selection = 'none',rownames=F,options=overviewTablesOptions)


output$definedExp<- DT::renderDataTable({
  validate(need(input$ExperimentDimension, message=FALSE))
    if(!is.null(InputStartTables$experiment)) {
        if(InputStartTables$experiment[1] == "single experiment") {
            data.frame()
            } else {
                if(length(InputStartTables$experiment)>1){
                    data.frame(experiments=InputStartTables$experiment)
                    }
                    else {
                        data.frame(feature_table2$data_pre %>% 
                                       distinct_(InputStartTables$experiment))
                    }
            }
    }else{
        data.frame()
        }
},   
selection = 'none',rownames=F,options=overviewTablesOptions)


output$definedAnno <- DT::renderDataTable({
  validate(need(input$AnnoDimension, message=FALSE))
  if(!is.null(InputStartTables$anno)) {
    data.frame(feature_table2$data_pre %>% distinct_(InputStartTables$anno))
  }else{
      data.frame()
      }
},   
selection = 'none',rownames=F,options=overviewTablesOptions)




# checkbox for "single experiment" choice 
output$IsSingleExperimentOut <- renderUI({
  validate(need(input$file1, message=FALSE))
    if(!is.null(feature_table2$data_pre))
        checkboxInput("IsSingleExperiment",
                      label = "set check if single experiment",
                      value = F)
})

# checkbox for "multiple experiment" columns
output$ScreensOnColumnsOut <- renderUI({
  validate(need(input$file1, message=FALSE))
    if(!is.null(feature_table2$data_pre))
      checkboxInput("ScreensOnColumns",
                    "set check if one column per data series and measured value",
                    F)
})


observe({
  if(!is.null(input$IsSingleExperiment)){
    if(isTRUE(input$IsSingleExperiment)) {
      updateSelectInput(session, 
                        "ExperimentDimension",
                        choices = "single experiment",
                        selected = "single experiment")
      updateCheckboxInput(session,
                          "ScreensOnColumns",
                          value = F)    
    } else {
      updateSelectInput(session, 
                        "ExperimentDimension",
                        choices = colnames(feature_table2$data_pre))
      }
  }
})

 observe({
   if(!is.null(input$ScreensOnColumns)){
     if(isTRUE(input$ScreensOnColumns)) {
        output$ExperimentDimensionOut <- DropDownInput(
                                            "ExperimentDimension",
                                            colnames(feature_table2$data_pre),
                                            HTML(paste("Choose",
                                               "<em> experiment/feature </em>",
                                                       "<br/> column(s)")),
                                            T)
        hideMeasuredValues$state <-  T
        updateCheckboxInput(session,
                            "IsSingleExperiment",
                            value = F) 
    }else{
      output$ExperimentDimensionOut <-  DropDownInput(
                                            "ExperimentDimension",
                                            colnames(feature_table2$data_pre),
                                            HTML(paste("Choose",
                                                       "<em> experiment </em>",
                                                       "<br/> column")),
                                            F)
      hideMeasuredValues$state = F
     }
    }
})


 #control for conditional panel of measured values drop down list
output$showMeasuredValues <- reactive({
  if(!isTRUE(hideMeasuredValues$state)){
    return(T)
  } else {
      return(F)
      }
})
outputOptions(output, "showMeasuredValues", suspendWhenHidden=FALSE)
 
output$hideMeasuredValues <- reactive({
  if(isTRUE(hideMeasuredValues$state)){
    return(T)
  } else {
      return(F)
      }
})
outputOptions(output, "hideMeasuredValues", suspendWhenHidden=FALSE)


#control for conditional panel of js question mark
output$showQM <- reactive({
  validate(need(input$file1, message=FALSE))
    if(is.null(feature_table2$data_pre)){
        return(FALSE)
    } else {
        return(T)
    }
})
outputOptions(output, "showQM", suspendWhenHidden=FALSE)



