fixedPage(
    HTML("<div style='width:1048px;'>"),
    conditionalPanel(condition='output.showPanels',
                     fixedRow(
                         #Spacer row
                         # HTML("</div>"),
                         HTML("<div style='height: 50px;'>"), 
                         HTML("</div>"), 
                         column( 12,
                                 h2("Session Parameter Download"),
                                 h5("The option to download a session 
                                    parameter file is provided here. The downloaded
                                    file contains the Data Input parameters as set 
                                    on the 'Data Input tab' and the controls 
                                    defined in the 'Quality Control tab'.
                                    The session parameter file is a .csv file and
                                    should not be edited. As ';' will be used to 
                                    format the .csv, the columns of the input table
                                    for which the parameters will be saved 
                                    should not contain ';'."),
                                 style="vertical-align: middle;",
                                 downloadButton( 'downloadParms',                     
                                                 label = h6("click to download")),
                                 tags$style(
                                     type='text/css', 
                                     "#downloadParms { margin-top:25px;
                                     margin-left:-10px;}")
                                 )
                                 )
                         ),#end of conditional panel 
    HTML("</div>")
    )#end of fixedPage  