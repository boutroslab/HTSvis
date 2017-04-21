#########################################
## server to save parameters in .txt file
#########################################

## paramters are captured as a data frame in a reactiveValues object
params = reactiveValues()
ParmsUpload <- reactiveValues(state=F)


observe({
    params_df <- data.frame(
        c("well_input","plate_input","experiment_input","anno_input","measuredValues_input",
          "cellHTS_state","singleExperiment_state",
          tabInput$inputPlates),
        c(rep(NA,7+length(tabInput$inputPlates)))
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
    
    posWellsOut <- c()
    #here comes stuff from server_qualityControl.R
    validate(need(input$platesQC, message=FALSE))
    if(isTRUE(plateStateQC$state))
        if(!is.null(pos_wellStore))
            posWellsOut <- unlist(reactiveValuesToList(pos_wellStore),use.names=F)
    if(!is.null(posWellsOut))
        if(length(posWellsOut)>0)
            params_df[8:length(tabInput$inputPlates),2] <- paste(posWellsOut,collapse = "_:_")
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
validate(need(input$file2, message=FALSE))
returnParms <-    function(x){  
    falseParams = F
    falseParams2 = F
    validate(need(input$file2, message=FALSE))
            if(!is.null(x)) {
                if(is.data.frame(x)) {
                    if(!is.null(x[,1]) && !is.null(x[,2])){
                        if(length(x[,1]) > 0 && length(x[,2]) > 0) {
                            for(i in 1:7) {
                                if(is.na(x[i,2])) {
                                    getParams[[x[i,1]]] <- NULL
                                } else {
                                    inputParm <-  unlist(
                                        strsplit(x[i,2],
                                                 ":_:")
                                    )
                                    if(inputParm[1] %in% c(colnames(feature_table2$data_pre),
                                                           "on")) {
                                        getParams[[x[i,1]]] <- inputParm
                                    } else {falseParams2 = T}
                                }
                            }
                        } else {falseParams = T}
                    } else {falseParams = T}
                } else {falseParams = T}
            } else {falseParams = T}
    
    if(isTRUE(falseParams)) {
        js_string <- 'alert("SOMETHING");'
        warnParms <- paste("The loaded session parameter file",
                           "does not match the input data",sep=" ")
        warnParms_js_string <- sub("SOMETHING",warnParms,js_string)
        session$sendCustomMessage(type='jsCode',
                                  list(value = warnParms_js_string ))
    } else {
        if(isTRUE(falseParams2)) {
            js_string <- 'alert("SOMETHING");'
            warnParms2 <- paste("Not all loaded parameter from the session file",
                                "do match the input data",sep=" ")
            warnParms2_js_string <- sub("SOMETHING",warnParms2,js_string)
            session$sendCustomMessage(type='jsCode',
                                      list(value = warnParms2_js_string ))
        } else {
            warnParms <- ''
            warnParms2 <- ''
        }
    }
}

try(returnParms(params$input),
    silent=T)

})


observe({
    validate(need(input$file2, message=FALSE))
    if(inherits(params$input,"try-error",which=F)) {
        js_string <- 'alert("SOMETHING");'
        warnParms3 <- paste("Upload of the session parameter file
                            failed due to an unknown error",sep=" ")
        warnParms3_js_string <- sub("SOMETHING",warnParms3,js_string)
        session$sendCustomMessage(type='jsCode',
                                  list(value = warnParms3_js_string ))
    } else {
        warnParms3 <- ''
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





