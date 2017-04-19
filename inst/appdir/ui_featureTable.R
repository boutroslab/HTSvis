conditionalPanel(condition='output.showPanels',
        fixedPage(
            HTML("<div style='width:1048px;'>"),
            fixedRow(
              column(12,
                     height = "50px",
                     helpPopup("Help: Feature Table",
                               id="helpFT",
                               content = "Below you will find your uploaded
                            data represented as an interactive table. 
                            If the uploaded table contains more than one 
                            measured value per well 
                            only the first one will be shown by default. 
                            Measured values can be added to the table
                            by selecting them from the list below 
                            ('select feature').
                            If the shown table contains more than one measured 
                            value per well, a reactive heatmap of 
                            selected rows (select/unselect rows by clicking) 
                            can be created. The entire selection can be reset 
                            with the 'reset' button.",
                               iconClass = "fa fa-question"),
                     tags$style(type='text/css',"#helpFT {
                                margin-top: 25px;}")
                     )  
            ),
     
            #Reset Button & Downloads
            HTML("<div style='width:1048px;'>"), 
            fixedRow(
                column( 3,
                        style="vertical-align: middle;",
                        uiOutput("features_ft")
                    ),  
                column( 3,
                        style="vertical-align: middle;",
                        actionButton(   'resetSelection',                       
                                        label="Click to reset row selection"),
                        tags$style(
                            type='text/css', 
                            "#resetSelection { margin-top: 38px;}")
                        ),  
                column(1,
                       helpPopup("Attention:",
                                 id="helpDowFT",
                                 content = "Depending on the size of the table
                                 the download might take several minutes. The
                                 table will be downloaded as a .csv table
                                 with columns separated by ',' and '.' as 
                                 decimal separator.",
                                 iconClass = "fa fa-exclamation"
                            ),
                       tags$style(type='text/css',"#helpDowFT {
                                  margin-top: 38px;
                                  margin-left:50px;")
                ), 
                column( 2,
                        style="vertical-align: middle;",
                        downloadButton( 'downloadFeatureTable',                     
                                        label = h6("click to download")),
                        tags$style(
                            type='text/css', 
                            "#downloadFeatureTable { margin-top:25px;
                                                        margin-left:-10px;}")
                     ),
                column( 3,
                        style="vertical-align: middle;",
                        textInput("filnameFeatureTable",                        
                                   label = h6("Enter Filename"),
                                   placeholder  = "filename"),
                        tags$style(
                            type='text/css', 
                            "#filnameFeatureTable {     margin-left:-50px;
                                                        margin-right:50px}")
                     )
                   ),
            # #Spacer row  
            HTML("</div>"),
            HTML("<div style='height: 25px;'>"),
            HTML("</div>"),
            HTML("<div style='width:1048px;'>"), 
            fixedRow(
                column( 3,
                        style="vertical-align: middle;",
                        actionButton(   'selectAllFeatures',                       
                                        label="Or click to select all channels")
                )
            ),
            #Spacer row  
            HTML("</div>"),
            HTML("<div style='height: 50px;'>"), 
            HTML("</div>"),
                   
            #Feature Table  
            HTML("<div style='width:1000px;'>"), 
            fixedRow(
                column( 12,   
                        div(style = 'overflow-x: scroll',
                        DT::dataTableOutput('featureTable') )                                             
                     )
                   ),
            HTML("</div>"),
            #Spacer row
            HTML("</div>"),
            HTML("<div style='height: 50px;'>"), 
            HTML("</div>"), 
            #Heatmap
            HTML("<div style='width:1000px;'>"), 
            fixedRow(
                conditionalPanel(condition='output.hideFt',
                                 column(12,
                                        align="center",
                                        htmlOutput("textFtDummy"),
                                        tags$style(HTML(
                                            "#textFtDummy {     margin-top:50px;
                                            margin-bottom:50px;
                                            font-size:12px;"))
                                        )
                ),
                conditionalPanel(condition='output.showFt',
                    column( 12,
                            plotOutput('featureHeatmap'),

                            textInput(   'fileNameFTheatmap',                    
                                         label = h6('Enter filename'),
                                         placeholder  = "filename") ,
                            radioButtons( 'fileFormatFTheatmap',        
                                          label = h6("Coose file format"),
                                          choices = c(".png" = ".png",
                                                      ".tiff" = ".tiff",
                                                      ".jpeg" = ".jpeg"),
                                          selected = ".png",
                                          inline = T ),  
                            downloadButton('downloadFTheatmap',
                                           'Click to Download Plot'),
                            tags$style(type='text/css',
                                "#downloadFTheatmap {    margin-bottom:25px;")
                         )
                )
               ),
            HTML("</div>")
        )#end of fixedPage  
)#end of conditional panel 