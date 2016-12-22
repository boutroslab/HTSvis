conditionalPanel(condition='output.showPanels',
             HTML("<div style='height: 50px;'>"),
             HTML("</div>"),
                 fixedRow(
                   column(3),
                   column( 2,
                           uiOutput("screens_qc"),
                           tags$style(type='text/css',
                                      "#screens_qc { margin-top: 50px;}")
                   ),
                   column( 2,
                           uiOutput("features_qc"),
                           tags$style(type='text/css',
                                      "#features_qc { margin-top: 50px;}")
                   ),
                   column(3,
                          helpPopup("Help: Quality Control Plots",
                                    id="helpQC",
                                    content = "Select an experiment to generate
                                    quality control plots
                                    based on control wells.
                                    (scatter plot, density distribution plot and
                                    a box plot).
                                    Control wells are defined by clicking on the
                                    heatmap at the bottom of the page.
                                    Control wells are always defined per experiment
                                    and the selection is valid for all plates within
                                    the experiment. The plate selection drop down
                                    list is only relevant for the heatmap to be shown.
                                    If a single experiment is investigated, the
                                    'select experiment'
                                    option is inactive. The measured value
                                    for which the plots are generated is
                                    selected from the 'select feature' list.",
                                    iconClass = "fa fa-question"),
                          tags$style(
                              type='text/css',
                              "#helpQC{ margin-top: 50px;
                              margin-left:-25px;}"))
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
                                 column( 3,
                                         uiOutput("plates_qc")
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
