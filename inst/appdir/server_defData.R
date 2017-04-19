## Drop Downs and Tables of Overview Panel 
source("./DropDownInput.R",local=T)

hideMeasuredValues <- reactiveValues(state=F)

getParams <- reactiveValues()



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
    getParams$well_input <- NULL
    getParams$plate_input <- NULL
    getParams$experiment_input <- NULL
    getParams$anno_input <- NULL
    getParams$cellHTS_state <- NULL
    getParams$singleExperiment_state <- NULL
})



## getParams$well has to be called once 
observe({
  validate(need(input$file1, message=FALSE))
    getParams$well_input
    output$WellDimensionOut <-  DropDownInput(
                                    identifier="WellDimension",
                                    input=colnames(feature_table2$data_pre), 
                                    label=HTML(paste("Choose",
                                               "<em> well </em>",
                                               "<br/> column")),
                                    multiState=F,
                                    sel=getParams$well_input)
})



observe({
  validate(need(input$file1, message=FALSE))
    getParams$plate_input
    output$PlateDimensionOut <-  DropDownInput(
                                    identifier="PlateDimension",
                                    input=colnames(feature_table2$data_pre),
                                    label=HTML(paste("Choose",
                                               "<em> plate </em>",
                                               "<br/> column")),
                                    multiState=F,
                                    sel=getParams$plate_input)  
})


observe({
  validate(need(input$file1, message=FALSE))
    getParams$experiment_input
    output$ExperimentDimensionOut <-  DropDownInput(
                                        identifier="ExperimentDimension",
                                        input=colnames(feature_table2$data_pre),
                                        label=HTML(paste("Choose",
                                                   "<em> experiment </em>",
                                                   "<br/> column")),
                                        multiState=F,
                                        sel=getParams$experiment_input)
})

observe({
  validate(need(input$file1, message=FALSE))
    getParams$measuredValues_input
    output$MeasuredValuesOut <-  DropDownInput(
                                    identifier="MeasuredValues",
                                    input=colnames(feature_table2$data_pre),
                                    label=HTML(paste("Choose",
                                       "<em> channel </em>",
                                       "<br/>column(s)")),
                                    multiState=T,
                                    sel=getParams$measuredValues_input)  
})

observe({
    validate(need(input$file1, message=FALSE))
    getParams$anno_input
        output$AnnoDimensionOut <-  DropDownInput(
                                        identifier= "AnnoDimension", 
                                        input=colnames(feature_table2$data_pre), 
                                        label=HTML(paste("Choose",
                                       "<em> annotation </em>",
                                       "<br/>column")),
                                       multiState=F,
                                       sel=getParams$anno_input)
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
        if(InputStartTables$experiment[1] == "single_experiment") {
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

# checkbox for "cellHTS top table" as input 
output$cellHTSstyleOut <- renderUI({
  validate(need(input$file1, message=FALSE))
    if(!is.null(feature_table2$data_pre))
      checkboxInput("cellHTSstyle",
                    label = HTML(paste("set check if",
                                       "<em> 'cellHTS topTable'</em>"
                                       )),
                    F)
})


#if input from parameter file
observe({
       if(!is.null(getParams$cellHTS_state))
        if(getParams$cellHTS_state %in% "on")
            updateCheckboxInput(session,
                                "cellHTSstyle",
                                value = T)
})

observe({
    if(!is.null(getParams$singleExperiment_state))
        if(getParams$singleExperiment_state %in% "on")
            updateCheckboxInput(session,
                                "IsSingleExperiment",
                                value = T)
})

#capture of 'cellHTS' or 'singleExperiment' input type 
observe({
    if(!is.null(input$IsSingleExperiment)){
        if(isTRUE(input$IsSingleExperiment)) {
            updateSelectInput(session, 
                              "ExperimentDimension",
                              choices = "single_experiment",
                              selected = "single_experiment")
            updateCheckboxInput(session,
                                "cellHTSstyle",
                                value = F)
            hideMeasuredValues$state = F
        } else {
            updateSelectInput(session, 
                              "ExperimentDimension",
                              choices = colnames(feature_table2$data_pre))
        }
    }
})

 observe({
   if(!is.null(input$cellHTSstyle)){
     if(isTRUE(input$cellHTSstyle)) {
        output$ExperimentDimensionOut <- DropDownInput(
                                            "ExperimentDimension",
                                            colnames(feature_table2$data_pre),
                                            HTML(paste("Choose",
                                               "<em> experiment/channel </em>",
                                                       "<br/> column(s)")),
                                            T,
                                            sel=getParams$experiment_input)
        hideMeasuredValues$state <-  T
        updateCheckboxInput(session,
                            "IsSingleExperiment",
                            value = F) 
    }else{
        if(!is.null(input$IsSingleExperiment)){
            if(!isTRUE(input$IsSingleExperiment)) {
              output$ExperimentDimensionOut <-  DropDownInput(
                                                    "ExperimentDimension",
                                                    colnames(feature_table2$data_pre),
                                                    HTML(paste("Choose",
                                                               "<em> experiment </em>",
                                                               "<br/> column")),
                                                    F,
                                                    sel=getParams$experiment_input)
              hideMeasuredValues$state = F
            }
        }
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



