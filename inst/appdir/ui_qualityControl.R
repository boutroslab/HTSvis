conditionalPanel(condition='output.showPanels',
                 HTML("<div style='height: 50px;'>"),
                 HTML("</div>"),
                 fixedRow(
                     column( 2,
                             uiOutput("screens_qc")
                     ),
                     column( 2,
                             uiOutput("features_qc")
                     ),
                     column(1,
                            helpPopup("Help: Quality Control Plots",
                                      id="helpQC",
                                      content = "Select an experiment to generate
                                      quality control plots
                                      based on control wells.
                                      (scatter plot, density distribution plot and
                                      a box plot).
                                      Control wells are defined by clicking on the
                                      heatmap at the bottom of the page.
                                      If a single experiment is investigated, the
                                      'select experiment'
                                      option is inactive. The measured value
                                      for which the plots are generated is
                                      selected from the 'select feature' list.",
                                      iconClass = "fa fa-question"),
                            tags$style(
                                type='text/css',
                                "#helpQC{ margin-top: 38px;
                                margin-left:-25px;}")
                            ),
                     column(2,
                            conditionalPanel(condition='output.showControlsUpload',
                                             h6("load controls from session
                                                parameter file"),
                                             actionButton('applyCtrls',
                                                          'load wells')
                                             )
                     ),
                     column(1,
                            conditionalPanel(condition='output.showControlsUpload',
                                             helpPopup("Help: Load Controls",
                                                       id="helpLC",
                                                       content = "Control wells can be saved 
                                                       in the session parameter file and applied
                                                       by clicking the button on the left. If 
                                                       controls differ between plates (i.e. 'all plates'
                                                       is not checked) the plates have to be selected from
                                                       the dropdown lists to load the controls per plate.",
                                                       iconClass = "fa fa-question"),
                                             tags$style(
                                                 type='text/css',
                                                 "#helpLC{ margin-top: 38px;
                                                 margin-left:-25px;}")
                                             )
                            ),
                     column( 2,
                             align = "center",
                             textInput(   'fileNamePlotQC',
                                          label = h6('Enter filename to download plots'),
                                          placeholder  = "filename") 
                     ),
                     column(2,
                            downloadButton('downloadPlotQC', 'Click to Download'),
                            tags$style(
                                type='text/css', 
                                "#downloadPlotQC { margin-top:25px;}")
                     )
                            ),
                 fixedRow(
                     column(   6,
                               plotOutput('platePlot')
                     ),
                     conditionalPanel(condition='output.showDensPlots',
                                      column(   6,
                                                plotOutput('densityPlot')
                                      )
                     ),
                     conditionalPanel(condition='output.hideDensPlots',
                                      column(   6,
                                                align="center",
                                                htmlOutput("textDensDummy"),
                                                tags$style(HTML(
                                                    "#textDensDummy {   margin-top:125px;
                                                    font-size:12px;"))
                                                )
                                      )
                     ),
                 
                 fixedRow(
                     conditionalPanel(condition='output.showBoxPlots',
                                      column(     6,
                                                  plotOutput('boxPlot')
                                      )
                     ),
                     conditionalPanel(condition='output.hideBoxPlots',
                                      column(     6,
                                                  align="center",
                                                  htmlOutput("textBoxDummy"),
                                                  tags$style(HTML(
                                                      "#textBoxDummy {  margin-top:125px;
                                                      font-size:12px;"))
                                                  )
                                      ),
                     column(     6,
                                 fixedRow(
                                     column( 6,
                                             h5(
                                                 radioButtons("radio",
                                                              label = "",
                                                              choices = list(
                                                                  "Define positive controls" = "pos",
                                                                  "Define negative controls" = "neg",
                                                                  "Define non-targeting controls" = "nt")
                                                 )
                                             ),
                                             actionButton( "resetControls",
                                                           label = "Click to reset"
                                             ),
                                             tags$style(type='text/css',
                                                        ('#resetControls  {margin:0 auto; display:block; }'))
                                     ),
                                     column(1,
                                            helpPopup("Help: Quality Control Plots",
                                                      id="helpQC2",
                                                      content = "Control wells can either be 
                                                      defined for all plates of one experiment or 
                                                      for each plate individually. The define 
                                                      controls for all plates simultaneously,
                                                      set the 'all plates' check.",
                                                      iconClass = "fa fa-question"),
                                            tags$style(
                                                type='text/css',
                                                "#helpQC2{ margin-top: 38px;
                                                margin-left:-25px;}")
                                            ),
                                     column(3,
                                            align="center",
                                            uiOutput("platesQC") ,
                                            tags$style(
                                                type='text/css',
                                                "#platesQC{ margin-top: 25px;}"),
                                            h6("set check to select all plates"),
                                            checkboxInput("allPlates","")
                                     )
                                     ),
                                 HTML("<div style='height: 25px;'>"),
                                 HTML("</div>"),
                                 fixedRow(
                                     column( 6,
                                             ggvisOutput("heatmap_qc"))
                                 )
                     )
                     ),
                 HTML("<div style='height: 25px;'>"),
                 HTML("</div>")
)
