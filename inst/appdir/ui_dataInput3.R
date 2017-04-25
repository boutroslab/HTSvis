fixedRow(
    ####Spacer row
    HTML("<div style='height: 25px;'>"),
    HTML("</div>"),
    
    
    column(2,
           uiOutput("WellDimensionOut")
    ),
    conditionalPanel(condition = "output.showQM",
                     column(1,
                            helpPopup("Help: Wells",
                                      id="helpWells",
                                      content = "Select the column of the uploaded data table
                                      that contains the well annotation.
                                      Row and column annotation for the wells
                                      need to have the following format: 'RowColmn' with rows as
                                      letters and columns as numbers (e.g. A1 or A01).
                                      Letters can be upper- or lowercase.
                                      Letters and numbers may be separated by
                                      characters like '-' or '_'.",
                                      iconClass = "fa fa-question"),
                            tags$style(
                                type='text/css',
                                "#helpWells{ margin-left:-25px;}")
                     )
                     ),
    column(2,
           uiOutput("PlateDimensionOut")
    ),
    conditionalPanel(condition = "output.showQM",
                     column(1,
                            helpPopup("Help: Plates",
                                      id="helpPlates",
                                      content = "Select the column of the uploaded data table
                                      that contains the plate annotation.
                                      Plate identifiers have to be unique within one experiment.
                                      If more than one experiment is investigated, all
                                      experiments have to contain the identical
                                      set of plates.",
                                      iconClass = "fa fa-question"),
                            tags$style(
                                type='text/css',
                                "#helpPlates{ margin-left:-25px;}")
                     )
                     ),
    column(2,
           uiOutput("ExperimentDimensionOut")
    ),
    conditionalPanel(condition = "output.showQM",
                     column(1,
                            helpPopup("Help: Experiments",
                                      id="helpExperiments",
                                      content = "
                                      If you want to investigate multiple experiments,
                                      select the column of the uploaded data table
                                      that contains the experiment information.
                                      Experiment identifiers have to be unique.
                                      If more than one experiment is investigated, all
                                      experiments have to contain the identical
                                      set of plates.
                                      If you upload the result table of cellHTS, set the upper check at the right. 
                                      If you want
                                      to investigate only a single experiment
                                      (i.e. each plate is only present once in the data set and has a unique identifier) you should set
                                      the lower check at the right.
                                      (more detailed information can be found
                                      on the help page)",
                        iconClass = "fa fa-question"),
              tags$style(
                  type='text/css',
                  "#helpExperiments{ margin-left:-25px;}")
                            )
                     ),
    column(3,
           uiOutput("cellHTSstyleOut"),
           uiOutput("IsSingleExperimentOut")
    )
    )#end of fixedRow




