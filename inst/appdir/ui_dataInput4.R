fixedRow(

HTML("<div style='height: 25px;'>"),
HTML("</div>"),
    column(2,
            uiOutput("AnnoDimensionOut"),
           div(DT::dataTableOutput("definedAnno"),
               style = "font-size: 95%; width: 80%")
    ),
    conditionalPanel(condition = "output.showQM",
        column(1,
            helpPopup("Help: Annotation",
                      id="helpAnno",
                      content = "This is an optional annotation column and
                        not required to start the app.
                        Select the column of the uploaded data table that
                        contains a 'per-well' annotation, e.g. 'siRNA01'",
                      iconClass = "fa fa-question"),
            tags$style(
                type='text/css',
                "#helpAnno{ margin-left:-25px;}")
                )
        ),
  conditionalPanel(condition = "output.showMeasuredValues",
  column(3,
         uiOutput("MeasuredValuesOut"),
         tags$style(type='text/css',"#MeasuredValuesOut {margin-bottom: 50px;}")
    ),
  conditionalPanel(condition = "output.showQM",
                   column(1,
                          helpPopup("Help: Masured Values",
                                    id="helpMV",
                                    content = "Select the column(s)
                                    which contain the measured values per well.
                                    Those columns have to contain numeric
                                    values. Missing values have to be filled by
                                    NA",
                                    iconClass = "fa fa-question"),
                          tags$style(
                              type='text/css',
                              "#helpMV{ margin-left:-25px;}")
                          )
      )
  ),
  conditionalPanel(condition = "output.hideMeasuredValues",
                   column(3,
                          ####Spacer row
                          HTML("<div style='height: 100px;'>"),
                          HTML("</div>")),
                   column(1,
                          helpPopup("",
                                    id="helpDummy",
                                    content = "",
                                    iconClass = ""),
                          tags$style(
                              type='text/css',
                              "helpDummy{ margin-left:-25px;}")
                   )
  ),
  conditionalPanel(
    condition = "output.showStartButtons",
    column(3,
           offset = 2,
           actionButton("startApp",label="Explore Data"),
           tags$style(type='text/css', "#startApp { margin-top: 25px;}"),
            h6("Click \"Explore Data\" button to start")
    )
  ),#end of conditionalPanel
    column(3,
           offset = 3,
           conditionalPanel(
             condition = "output.showPanels",
               uiOutput("StartInfo1"),
               tags$style(type='text/css', "#StartInfo1 { margin-top: 25px;}"),
                tags$style("#StartInfo1 {color: green;
                                  }"
                         ),
             uiOutput("StartInfo7"),
             tags$style(type='text/css', "#StartInfo1 { margin-top: 25px;}"),
             tags$style("#StartInfo7{color: green;
                        }"
                         ),
               uiOutput("StartInfo2"),
               uiOutput("StartInfo3"),
               uiOutput("StartInfo4"),
               uiOutput("StartInfo5"),
               uiOutput("StartInfo6")
           )
        )
)#end of conditional panel

