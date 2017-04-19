#########################################
## server to save parameters in .txt file
#########################################

## paramters are captured as a data frame in a reactiveValues object
params = reactiveValues()
ParmsUpload <- reactiveValues(state=F)

observe({
    
    params_df <- data.frame(
                    c("well_input","plate_input","experiment_input","anno_input",
                      "measuredValues_input",
                      "cellHTS_state","singleExperiment_state"),
                    c(rep(NA,7))
                    )
    colnames(params_df) <- NULL
    
    if(!is.null(input$WellDimension))
        params_df[1,2] <- input$WellDimension
        params$data = params_df
        
    if(!is.null(input$PlateDimension))     
        params_df[2,2] <- input$PlateDimension
        params$data = params_df
        
    if(!is.null(input$ExperimentDimension))    
        if(isTRUE(input$cellHTSstyle)) {
            params_df[3,2] <-  paste(input$ExperimentDimension,collapse=":_:")
            params$data = params_df
        } else {    
            if(isTRUE(input$IsSingleExperiment)) {
                params_df[3,2] <- NA
            } else {
                params_df[3,2] <- input$ExperimentDimension
                params$data = params_df }
        }
     
    if(!is.null(input$AnnoDimension))           
        params_df[4,2] <- input$AnnoDimension
        params$data = params_df
        
    if(!is.null(input$MeasuredValues))
        params_df[5,2] <-  paste(input$MeasuredValues,collapse=":_:")
        params$data = params_df
        
    if(!is.null(input$cellHTSstyle))
        if(isTRUE(input$cellHTSstyle)) 
            params_df[6,2] <- "on"
            params$data = params_df

    if(!is.null(input$IsSingleExperiment))
        if(isTRUE(input$IsSingleExperiment)) 
            params_df[7,2] <- "on"
            params$data = params_df
})



filename_params <- reactive({
    Rdate <- gsub(" ", "_", date(), fixed = TRUE)
    Rdate_trim <-gsub("^[[:alpha:]]*_","",Rdate)
    paste0("HTSvis_",Rdate_trim)
})


## parameters which were captured in a reactiveValues object are saved as .csv
output$downloadParms <- downloadHandler (
    filename = function() {
        paste(filename_params(),
              "csv",
              sep=".") },
    content = function(file) {
        write.table(params$data,
                    file,
                    row.names = F,
                    sep=",",
                    dec ="."
        )}
)


## load and read-in parameter file upon trigger 
## parameters are handed over to reactiveValues object 
observe({
    inFile <- input$file2
    if (is.null(inFile))
        return(NULL)
            if(file_ext(inFile$name) == "csv") 
                params$input <-  testInput(
                    data.frame(
                        fread(
                            inFile$datapath,
                            na.strings = c("NA","N/A",
                                           "NaN","null",""),
                            header=F
                        )
                        ,row.names = NULL)
                )

})


observe({
    falseParmas = F
    validate(need(input$file2, message=FALSE))
    if(!is.null(input$applyParms))
        if(isTRUE(input$applyParms)) 
            if(!is.null(params$input[,1]) && !is.null(params$input[,2])){
                if(length(params$input[,1]) > 0 && length(params$input[,2]) > 0) {
                    for(i in 1:nrow(params$input)) {
                        if(is.na(params$input[i,2])) {
                            getParams[[params$input[i,1]]] <- NULL
                        } else {
                            inputParm <-  unlist(
                                strsplit(params$input[i,2],
                                         ":_:")
                                    )
                            if(inputParm[1] %in% c(colnames(feature_table2$data_pre),
                                                "cellHTS_state",
                                                "singleExperiment_state")) {
                                getParams[[params$input[i,1]]] <- inputParm
                            } else {falseParmas = T}
                        }
                    }
                } else {falseParmas = T}
            } else {falseParmas = T}
    

    if(isTRUE(falseParmas)) {
        js_string <- 'alert("SOMETHING");'
        warnParms <- paste("The loaded session parameter file",
                           "does not match the input data",sep=" ")
        warnParms_js_string <- sub("SOMETHING",warnParms,js_string)
        session$sendCustomMessage(type='jsCode',
                                  list(value = warnParms_js_string ))
    } else {
        warnParms <- ''
    }
})


observe({
    if(!is.null(feature_table2$data_pre) && nrow(feature_table2$data_pre)>0)
        ParmsUpload$state <- T
})

output$showParmsUpload <- reactive({
    return(ParmsUpload$state)
})
outputOptions(output, "showParmsUpload", suspendWhenHidden=FALSE)


output$ParmCheckLabel<- renderUI({
    "set check to apply loaded session parameter"
})



