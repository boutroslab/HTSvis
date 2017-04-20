conditionalPanel(condition='output.showParmsUpload',
                 fixedRow(
                     column(4,
                            fileInput('file2', 'optional: upload a session parameter file',
                                      accept = c('.csv')
                            )
                     ),
                     column(1,
                            helpPopup("Help: Session Paramter File",
                                      id="helpParms",
                                      content = "You can upload a session parameter file
                                      saved from a previous session (see 'option' tab).
                                      If you don't want to load a session parameter file you 
                                      you can ignore this field and perform your settings below.",
                                      iconClass = "fa fa-question"),
                            tags$style(
                                type='text/css',
                                "#helpParms{ margin-top: 25px;
                                margin-left:-25px;}")
                            ),
                     column(1,
                            checkboxInput("applyParms",""),
                            tags$style(
                                type='text/css',
                                "#applyParms{ margin-top: 25px;
                                margin-right:-25px;}")
                            ),
                     column(6,
                            htmlOutput("ParmCheckLabel"),
                            tags$style(HTML("#ParmCheckLabel {
                                            margin-top: 33px;
                                            margin-left:-60px;}")))
                     )
)#end of conditionalPanel