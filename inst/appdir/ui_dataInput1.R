#file uploader ui
fixedRow(
  column( 4,
                  fileInput('file1', 'Choose file to upload',
                            accept = c(
                              'text/csv',
                              'text/comma-separated-values',
                              'text/plain',
                              '.csv',
                              '.Rdata')
                  )
  ),
  column(1,
                   helpPopup("Help: Data upload",
                             id="helpUpload",
                             content = "Select a data table to upload.
                             Supported file formats are: .txt, .csv and .RData.
                             Delimited tables should be uniformly separated
                            by tab, comma, semicolon or space and all columns
                            should be named.
                            Detailed information concerning data
                             structure requirements can be found on the help page.",
                             iconClass = "fa fa-question"),
                      tags$style(
                          type='text/css',
                          "#helpUpload{ margin-top: 25px;
                                        margin-left:-25px;}")
           ),
  column(  7,
           htmlOutput("dataInfo"),
           tags$style(HTML("#dataInfo {
                           margin-top: 25px;}"))
    )
)
