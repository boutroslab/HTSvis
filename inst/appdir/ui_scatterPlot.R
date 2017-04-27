conditionalPanel(condition='output.showPanels',
                 fixedRow(
                     conditionalPanel(condition = 'output.controlSB',
                                      column(6,
                                             switchButton( inputId = "popman",
                                                           type = "TF",
                                                           value = TRUE,
                                                           col="GB",
                                                           label = h6("Switch to Population Manager")
                                             )
                                      )
                     ),
                     conditionalPanel(condition = 'output.startMessage',
                                      column( 6,
                                              offset = 6,
                                              align = 'center',
                                              h5("Brush to zoom or to define subpopulation")
                                      )
                     ),
                     conditionalPanel(condition = 'output.showZoomer',
                                      column( 6,
                                              offset = 6,
                                              align = "center",
                                              actionButton(   'Zoomer',
                                                              label = "click to zoom")
                                      )
                     ),
                     conditionalPanel(condition = 'output.showDoubleClick',
                                      column(  6,
                                               offset = 6,
                                               align = "center",
                                               h5("Double click on plot to reset zoom")
                                      )
                     )
                 ),
                 
                 #Population Manager
                 #popTable object = Table showing all created populations
                 fixedRow(
                     conditionalPanel(condition ='output.showPopMan',
                                      column( 2,
                                              helpPopup("Help population manager:",
                                                        id="helpPM",
                                                        content = "Populations of measured values
                                                        can be investigated and colored by drawing
                                                        a rectangle on plot regions with your mouse
                                                        (use drag and
                                                        drop). The defined population will then appear
                                                        in a table. From this table you can highlight
                                                        the colored points in the plot or get
                                                        information on the selected values in a second
                                                        table which will appear below. Populations can
                                                        further be defined based on their annotation
                                                        or well position (if no annotation is defined
                                                        during data input). Colored values will be
                                                        preserved according to their well position when
                                                        the plotted experiments are changed.
                                                        Use the botton
                                                        at the top to get back to the control page of
                                                        the scatter plot.",
                                                        iconClass = "fa fa-question"),
                                              tags$style(type='text/css',"#helpPM {
                                                         margin-top: 0px;}"),
                                              textInput ( 'namePopulation',
                                                          label = h6(
                                                              HTML(paste("Enter population names",
                                                                         "(wo/ whitespace)",
                                                                         sep="<br/>"))
                                                          ),
                                                          placeholder = "Population_1"),
                                              HTML("<div style='height: 10px;'>"),
                                              HTML("</div>"),
                                              actionButton ( 'makeSubpopulation',
                                                             label = h6("click to create sub population") ),
                                              HTML("<div style='height: 5px;'>"),
                                              HTML("</div>"),
                                              DT::dataTableOutput("popTable")
                                              ),
                                      column(   2,
                                                offset= 1,
                                                selectizeInput('GenesToColor',
                                                               choices=NULL,
                                                               label=h6("", id = "labelGTC")
                                                ),
                                                tags$style(HTML("#labelGTC {
                                                                margin-top: 52px;}")),
                                                HTML("<div style='height: 10px;'>"),
                                                HTML("</div>"),
                                                actionButton(   'ColorGenes',
                                                                label = h6("click to color")),
                                                HTML("<div style='height: 5px;'>"),
                                                HTML("</div>")
                                                ),
                                      column(1)
                 ),
                 conditionalPanel(condition ='output.hidePopMan',
                                  column( 3,
                                          helpPopup("Help scatter plot:",
                                                    id="helpSP",
                                                    content = "The experiments and the measured
                                                    value for which the scatter plot should be
                                                    created can be defined from drop down lists.
                                                    You can zoom into a plot region by
                                                    drawing a window in a drag-and-drop manner
                                                    with your mouse. This action also triggers a switch to the
                                                    population manager panel. The population manager can be closed again
                                                    by a switch-button appearing at the top. If only one
                                                    experiment is loaded, individual plates can be
                                                    plotted against each other.",
                                                    iconClass = "fa fa-question"),
                                          h5("Set x axsis"),
                                          uiOutput("features_sp_x"),
                                          HTML("<div style='height: 20px;'>"),
                                          HTML("</div>"),
                                          h5("Set y axis"),
                                          uiOutput("features_sp_y"),
                                          HTML("<div style='height: 50px;'>"),
                                          HTML("</div>"),
                                          checkboxInput(inputId = "getCorr",
                                                        label = "get linear model statistics",
                                                        value = FALSE),
                                          htmlOutput("corrText")
                                  ),
                                  column(   3,
                                            uiOutput("screens_sp_x"),
                                            tags$style(type='text/css',"#screens_sp_x {
                                                       margin-top: 69px;}"),
                                            HTML("<div style='height: 20px;'>"),
                                            HTML("</div>"),
                                            uiOutput("screens_sp_y"),
                                            tags$style(type='text/css',"#screens_sp_y {
                                                       margin-top: 35px;}")
                                            ),
                                  column(   1,
                                            HTML("<div style='height: 20px;'>"),
                                            HTML("</div>"),
                                            textOutput("warnScreens")
                                  )
                                            ),
                 column(  6,
                          plotOutput("scatterPlot",
                                     brush = brushOpts(id="plotSP_brush",
                                                       resetOnNew = TRUE),
                                     dblclick = "plotSP_dblclick",
                                     height = "auto",
                                     width = "100%")
                 )
                 ),
                 
                 fixedRow(
                     conditionalPanel(condition='output.showGeneTable && output.showPopMan',
                                      column(   6,
                                                align = "center",
                                                DT::dataTableOutput('geneTable'),
                                                HTML("<div style='height: 10px;'>"),
                                                HTML("</div>"),
                                                downloadButton(  'downloadGeneTable',
                                                                 label = h6("click to download")),
                                                align = "center",
                                                textInput( "filnameGeneTable",
                                                           label = h6("Enter Filename"),
                                                           placeholder  = "filename")
                                      )
                     ),
                     HTML("<div style='height: 25px;'>"),
                     HTML("</div>"),
                     column(6),
                     column( 6,
                             align = "center",
                             sliderInput("pointSizeSPin",
                                         label = h6("set point size"),
                                         min=0.5,max=5,value=1),
                             sliderInput("opaSPin",
                                         label = h6("set opacity"),
                                         min=0,max=1,value=0.5),
                             textInput(   'fileNamePlot',
                                          label = h6('Enter filename for Download'),
                                          placeholder  = "filename") ,
                             radioButtons( 'fileFormatPlot',
                                           label = h6("Coose file format"),
                                           choices = c(".png" = ".png",
                                                       ".tiff" = ".tiff",
                                                       ".jpeg" = ".jpeg",
                                                       ".pdf" = ".pdf"),
                                           selected = ".png",
                                           inline = T ),
                             downloadButton('downloadPlot', 'Click to Download Plot'),
                             HTML("<div style='height: 20px;'>"),
                             HTML("</div>")
                     )
                 )
                 )
