
feature_table2 <- reactiveValues()

observe({
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  if(file_ext(inFile$name) == "RData" |
      file_ext(inFile$name) == "Rdata") {
    feature_table2$data_pre <- get(load(inFile$datapath))
    if(class(feature_table2$data_pre) != "data.frame"){
        feature_table2$data_pre <- NULL
    } else {
    return(feature_table2) }
  } else {
    if(file_ext(inFile$name) == "txt" | file_ext(inFile$name) == "tsv"){
      feature_table2$data_pre <-  read.table(inFile$datapath,header=T)
    } else {
      if(file_ext(inFile$name) == "csv") {
        feature_table2$data_pre <-  data.frame(
                                            fread(
                                                inFile$datapath,
                                                na.strings = c("NA","N/A",
                                                               "NaN","null","")
                                                )
                                            ,row.names = NULL)
        } else {feature_table2$data_pre <- NULL}
    }
  }
})



InputStartTables <- reactiveValues()
observeEvent(input$file1,{
  InputStartTables$plate <- NULL
  InputStartTables$well <- NULL
  InputStartTables$experiment <- NULL
  InputStartTables$anno <- NULL

  showApp$panels = F
  showApp$dummy = T
})


output$dataInfo <- renderUI({
  validate(need(input$file1, message=FALSE))
    if (is.null(feature_table2$data_pre)){
        h6("Incorrect data format (.RData data frames, .txt and .csv are supported)")
    } else {
        test_cols <- feature_table2$data_pre
        if(any(duplicated(colnames(test_cols)))) {
            h6("Duplicated colnames are not allowed")
        } else {
        HTML(paste0("The uploaded data table has <b>",
                    ncol(feature_table2$data_pre),
                   " columns</b> and <b>",
                   nrow(feature_table2$data_pre),
                   " rows</b>",
                   "<br/> Select columns with the annotation
                    and measured values from the drop down lists.")
       )
        }
    }
})




