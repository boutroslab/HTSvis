    ##shiny server function
shinyServer(function(input, output,session) {

##helper functions for js in scatter plot tab
    source("./helperFunctions.R",local=T)

##Data upload
    source("./server_dataUpload.R",local=T)


##overview Panel
    #tables shown in overview Panel
    source("./server_defData.R",local=T)

    #check conditions to start the app
    source("./server_launchApp.R",local=T)

    #Dropdown lists for all tab panels
    source("./server_DropDownLists.R",local = T)


##Plate viewer
    #Plot heatmps in well/format
    source("./server_plateViewer.R",local=T)


##Feature Table
    source("./server_featureTable.R",local=T)


##Quality Control
    source("./server_qualityControl.R",local=T)


##Scatter Plot
    source("./server_scatterPlot.R",local=T)

##Help Page
    source("./server_helpPage.R",local=T)

}) # end of shinyServer
