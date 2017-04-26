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
                                        Please start a new session (open a new browser window) 
                                        when loading a session parameter file. 
                                      If you don't want to load a session parameter file,
                                      you can ignore this field and perform your settings below.",
                                      iconClass = "fa fa-question"),
                            tags$style(
                                type='text/css',
                                "#helpParms{ margin-top: 25px;
                                margin-left:-25px;}")
                            )
                     )
)#end of conditionalPanel



