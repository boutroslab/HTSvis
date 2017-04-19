source("./DropDown.R",local=T)

#reactive Value object for the control of the tabs 
#in respect to "single experiment" choice 
IsSingleExperimentTabs <- reactiveValues(state = F)


observeEvent(input$startApp,{
#UI Outputs for Plate Viewer tab
  
  if(!is.null(input$IsSingleExperiment)) {
    if(isTRUE(input$IsSingleExperiment)) {
      IsSingleExperimentTabs$state <- TRUE
    } else {
      IsSingleExperimentTabs$state <- FALSE
    }
  }
  
  
output$featuresPlate1 <- DropDown(identifier="featuresPlate1",
                                  input=tabInput$inputFeatures,
                                  toSelect = "Select channel",
                                  multiState=F)

output$featuresPlate2 <- DropDown("featuresPlate2",
                                  tabInput$inputFeatures,
                                  "Select channel",
                                  F)

output$featuresPlate3 <- DropDown("featuresPlate3",
                                  tabInput$inputFeatures,
                                  "Select channel",
                                  F)

output$featuresPlate4 <- DropDown("featuresPlate4",
                                  tabInput$inputFeatures,
                                  "Select channel",
                                  F)


output$platesPlate1 <- DropDown(identifier="platesPlate1",
                                input=tabInput$inputPlates,
                                toSelect="Select plate",
                                multiState=F)

output$platesPlate2 <- DropDown("platesPlate2",
                                tabInput$inputPlates,
                                "Select plate",
                                F)

output$platesPlate3 <- DropDown("platesPlate3",
                                tabInput$inputPlates,
                                "Select plate",
                                F)

output$platesPlate4 <- DropDown("platesPlate4",
                                tabInput$inputPlates,
                                "Select plate",
                                F,
                                F)

output$platesQC <- DropDown("platesQC",
                                tabInput$inputPlates,
                                "Select plate",
                                F)


output$screensPlate1 <- DropDown(identifier="screensPlate1",
                                 input=tabInput$inputScreens,
                                 toSelect="Select experiment",
                                 multiState=F)

output$screensPlate2 <- DropDown("screensPlate2",
                                 tabInput$inputScreens,
                                 "Select experiment",
                                 F,
                                 F)

output$screensPlate3 <- DropDown("screensPlate3",
                                 tabInput$inputScreens,
                                 "Select experiment",
                                 F)

output$screensPlate4 <- DropDown("screensPlate4",
                                 tabInput$inputScreens,
                                 "Select experiment",
                                 F)




#UI Outputs for Feature Table tab
output$features_ft <- DropDown("feature_selection_ft",
                               tabInput$inputFeatures,
                               "Select channel",
                               T)


#UI Outputs for Quality Control tab
output$features_qc <- DropDown("feature_selection_qc",
                               tabInput$inputFeatures,
                               "Select channel",
                               F)

output$screens_qc <- DropDown("screen_selection_qc",
                              tabInput$inputScreens,
                              "Select experiment",
                              F)




#UI Outputs for Scatter Plot tab
output$features_sp_x <- DropDown("feature_selection_sp_x",
                                 tabInput$inputFeatures,
                                 "Select channel",
                                 F,
                                 F)

output$features_sp_y <- DropDown("feature_selection_sp_y",
                                 tabInput$inputFeatures,
                                 "Select channel",
                                 F)

output$screens_sp_x <- DropDown("screens_selection_sp_x",
                                tabInput$inputScreens,
                                "Select experiment",
                                F)

output$screens_sp_y <- DropDown("screens_selection_sp_y",
                                tabInput$inputScreens,
                                "Select experiment",
                                F)


if(isTRUE(IsSingleExperimentTabs$state)) {
  output$screens_sp_x <- DropDown("screens_selection_sp_x",
                                  tabInput$inputPlates,
                                  "Select Plate",
                                  F)
  
  output$screens_sp_y <- DropDown("screens_selection_sp_y",
                                  tabInput$inputPlates,
                                  "Select Plate",
                                  F)
    }
})





