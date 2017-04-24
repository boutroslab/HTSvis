#ui part of global help page
fixedRow(
    column(12,
           htmlOutput("helpHeader"),
           tags$style(HTML(
               "#helpHeader {  margin-top:50px;
               font-size:30px;")),

           htmlOutput("helpSubHeader"),
           tags$style(HTML(
               "#helpSubHeader {  margin-top:25px;
               font-size:15px;")),
   #action buttons
            actionButton("showStructure",
                         label="Data Structure"),
            tags$style(type="text/css","#showStructure {margin-top:50px;"),

           actionButton("showUpload",
                        label="Data Upload"),
           tags$style(type="text/css","#showUpload {
                      margin-top:50px;
                      margin-left:25px;"),

           actionButton("showPlateViewer",
                        label="Plate Viewer"),
           tags$style(type="text/css","#showPlateViewer {
                      margin-top:50px;
                      margin-left:25px;"),

           actionButton("showHelpFT",
                        label="Feature Table"),
           tags$style(type="text/css","#showHelpFT {
                      margin-top:50px;
                      margin-left:25px;"),

           actionButton("showHelpQC",
                        label="Quality Control"),
           tags$style(type="text/css","#showHelpQC {
                      margin-top:50px;
                      margin-left:25px;"),

           actionButton("showHelpSP",
                        label="Scatter Plot"),
           tags$style(type="text/css","#showHelpSP {
                              margin-top:50px;
                              margin-left:25px;"),
   
           actionButton("showTestData",
                        label="Test Data"),
           tags$style(type="text/css","#showTestData {
                      margin-top:50px;
                      margin-left:25px;"),


    #Data structure
        conditionalPanel(
          condition = "output.showStructureOut",
               htmlOutput("helpChOneNum"),
               tags$style(HTML(
                   "#helpChOneNum {  margin-top:50px;
                   font-size:25px;")),

               htmlOutput("helpChOneOneNum"),
               tags$style(HTML(
                   "#helpChOneOneNum {  margin-top:25px;
                   font-size:20px;")),

               htmlOutput("helpChOne"),
               tags$style(HTML(
                   "#helpChOne {  margin-top:10px;
                   font-size:15px;")),

               HTML("<div>"),
               HTML(paste(img(src='structure.png',
                              style='height: 300px;',
                              vspace="60",
                              style="display: block;
                              margin-left: auto; margin-right: auto;"), "")
                ),
               HTML("</div>"),

               htmlOutput("helpChOne2"),
               tags$style(HTML(
                   "#helpChOne2 { font-size:15px;")),

               HTML("<div>"),
               HTML(paste(img(src='table_cellHTS.png',
                              style='height: 300px;',
                              vspace="60",
                              style="display: block;
                              margin-left: auto; margin-right: auto;"), "")
                ),
               HTML("</div>"),

               htmlOutput("helpChOne3"),
               tags$style(HTML(
                   "#helpChOne3 { font-size:15px;")),

               HTML("<div>"),
               HTML(paste(img(src='table.png',
                              style='height: 300px;',
                              vspace="40",
                              style="display: block;
                              margin-left: auto; margin-right: auto;"), "")
               ),
               HTML("</div>"),

               htmlOutput("helpChOne4"),
               tags$style(HTML(
                   "#helpChOne4 { font-size:15px;
                   margin-top:100px;")),

               HTML("<div>"),
               HTML(paste(img(src='table_gen.png',
                              style='height: 300px;',
                              vspace="40",
                              style="display: block;
                              margin-left: auto; margin-right: auto;"), "")
               ),
               HTML("</div>"),

               htmlOutput("helpChOneTwoNum"),
               tags$style(HTML(
                   "#helpChOneTwoNum {  margin-top:100px;
                   font-size:20px;")),

              htmlOutput("helpStructure2"),
              tags$style(HTML(
                  "#helpStructure2  {  margin-top:25px;
                  font-size:15px;")),

              HTML("<div>"),
              HTML(paste(img(src='plate.png',
                             style='height: 300px;',
                             vspace="40",
                             style="display: block;
                             margin-left: auto; margin-right: auto;"), "")
              ),
              HTML("</div>"),

              htmlOutput("helpStructure3"),
              tags$style(HTML(
                  "#helpStructure3  {  margin-top:100px;
                  font-size:15px;"))
            ),#end of conditional panel



#Data upload 
       conditionalPanel(
           condition = "output.showUploadOut",

                htmlOutput("headerUpload"),
                tags$style(HTML(
                    "#headerUpload {  margin-top:50px;
                    font-size:25px;")),

               htmlOutput("uploadNum1"),
               tags$style(HTML(
                   "#uploadNum1 {  margin-top:25px;
                   font-size:20px;")),

               htmlOutput("uploadText1"),
               tags$style(HTML(
                   "#uploadText1 {  margin-top:10px;
                   font-size:15px;")),

               HTML("<div>"),
               HTML(paste(img(src='upload1.png',
                              style='height: 400px;',
                              vspace="40",
                              style="display: block;
                              margin-left: auto; margin-right: auto;"), "")
               ),
               HTML("</div>"),

               htmlOutput("uploadText2"),
               tags$style(HTML(
                   "#uploadText2 {  margin-top:100px;
                   font-size:15px;")),

           HTML("<div>"),
           HTML(paste(img(src='upload4.png',
                          style='height: 400px;',
                          vspace="40",
                          style="display: block;
                              margin-left: auto; margin-right: auto;"), "")
           ),
           HTML("</div>"),

           htmlOutput("uploadText3"),
           tags$style(HTML(
               "#uploadText3 {  margin-top:100px;
               font-size:15px;")),

           HTML("<div>"),
           HTML(paste(img(src='upload3.png',
                          style='height: 400px;',
                          vspace="40",
                          style="display: block;
                          margin-left: auto; margin-right: auto;"), "")
           ),
           HTML("</div>"),


           htmlOutput("uploadText4"),
           tags$style(HTML(
               "#uploadText4 {  margin-top:100px;
               font-size:15px;")),


           HTML("<div>"),
           HTML(paste(img(src='upload2.png',
                          style='height: 400px;',
                          vspace="40",
                          style="display: block;
                          margin-left: auto; margin-right: auto;"), "")
           ),
           HTML("</div>"),

           htmlOutput("uploadText5"),
           tags$style(HTML(
               "#uploadText5 {  margin-top:100px;
               font-size:15px;")),


           HTML("<div>"),
           HTML(paste(img(src='upload5.png',
                          style='height: 400px;',
                          vspace="40",
                          style="display: block;
                          margin-left: auto; margin-right: auto;"), "")
           ),
           HTML("</div>")
       ),#end of conditional panel



   #Plate Veiwer
   conditionalPanel(
       condition = "output.showHelpPVOut",


       htmlOutput("headerPV"),
       tags$style(HTML(
           "#headerPV {  margin-top:50px;
           font-size:25px;")),

       htmlOutput("pvNum1"),
       tags$style(HTML(
           "#pvNum1 {  margin-top:25px;
           font-size:20px;")),

       htmlOutput("PVtext1"),
       tags$style(HTML(
           "#PVtext1 {  margin-top:10px;
                   font-size:15px;")),

       HTML("<div>"),
       HTML(paste(img(src='PV1.png',
                      style='height: 400px;',
                      vspace="40",
                      style="display: block;
                      margin-left: auto; margin-right: auto;"), "")
       ),
       HTML("</div>"),

       htmlOutput("PVtext2"),
       tags$style(HTML(
           "#PVtext2 {  margin-top:100px;
                   font-size:15px;")),

       HTML("<div>"),
       HTML(paste(img(src='PV2.png',
                      style='height: 500px;',
                      vspace="40",
                      style="display: block;
                      margin-left: auto; margin-right: auto;"), "")
       ),
       HTML("</div>"),

       htmlOutput("PVtext3"),
       tags$style(HTML(
           "#PVtext3 {  margin-top:100px;
                   font-size:15px;")),

       HTML("<div>"),
       HTML(paste(img(src='PV3.png',
                      style='height: 500px;',
                      vspace="40",
                      style="display: block;
                      margin-left: auto; margin-right: auto;"), "")
       ),
       HTML("</div>")

    ),#end of conditional panel



   #Feature Table
   conditionalPanel(
       condition = "output.showHelpFTOut",


       htmlOutput("headerFT"),
       tags$style(HTML(
           "#headerFT {  margin-top:50px;
           font-size:25px;")),

       htmlOutput("ftNum1"),
       tags$style(HTML(
           "#ftNum1 {  margin-top:25px;
           font-size:20px;")),

       htmlOutput("FTtext1"),
       tags$style(HTML(
           "#FTtext1 {  margin-top:10px;
           font-size:15px;")),

       HTML("<div>"),
       HTML(paste(img(src='FT1.png',
                      style='height: 450px;',
                      vspace="40",
                      style="display: block;
                      margin-left: auto; margin-right: auto;"), "")
       ),
       HTML("</div>"),

       htmlOutput("FTtext2"),
       tags$style(HTML(
           "#FTtext2 {  margin-top:100px;
           font-size:15px;")),

       HTML("<div>"),
       HTML(paste(img(src='FT2.png',
                      style='height: 350px;',
                      vspace="40",
                      style="display: block;
                      margin-left: auto; margin-right: auto;"), "")
       ),
       HTML("</div>"),

       htmlOutput("FTtext3"),
       tags$style(HTML(
           "#FTtext3 {  margin-top:100px;
           font-size:15px;")),

       HTML("<div>"),
       HTML(paste(img(src='FT3.png',
                      style='height: 650px;',
                      vspace="40",
                      style="display: block;
                      margin-left: auto; margin-right: auto;"), "")
       ),
       HTML("</div>")

   ),#end of conditional panel

   #Quality Control
   conditionalPanel(
       condition = "output.showHelpQCOut",


       htmlOutput("headerQC"),
       tags$style(HTML(
           "#headerQC {  margin-top:50px;
           font-size:25px;")),

       htmlOutput("qcNum1"),
       tags$style(HTML(
           "#qcNum1 {  margin-top:25px;
           font-size:20px;")),


       htmlOutput("QCtext1"),
       tags$style(HTML(
           "#QCtext1 {  margin-top:10px;
           font-size:15px;")),

       htmlOutput("qcNum2"),
       tags$style(HTML(
           "#qcNum2 {  margin-top:25px;
           font-size:20px;")),

       htmlOutput("QCtext2"),
       tags$style(HTML(
           "#QCtext2 {  margin-top:10px;
           font-size:15px;")),

       HTML("<div>"),
       HTML(paste(img(src='QC1.png',
                      style='height: 600px;',
                      vspace="40",
                      style="display: block;
                      margin-left: auto; margin-right: auto;"), "")
       ),
       HTML("</div>"),

       htmlOutput("QCtext3"),
       tags$style(HTML(
           "#QCtext3 {  margin-top:100px;
           font-size:15px;")),

       HTML("<div>"),
       HTML(paste(img(src='QC2.png',
                      style='height: 600px;',
                      vspace="40",
                      style="display: block;
                      margin-left: auto; margin-right: auto;"), "")
       ),
       HTML("</div>"),

       htmlOutput("QCtext4"),
       tags$style(HTML(
           "#QCtext4 {  margin-top:100px;
           font-size:15px;")),

       HTML("<div>"),
       HTML(paste(img(src='QC3.png',
                      style='height: 600px;',
                      vspace="40",
                      style="display: block;
                      margin-left: auto; margin-right: auto;"), "")
       ),
       HTML("</div>")

   ),#end of conditional panel

   #Scatter Plot
   conditionalPanel(
       condition = "output.showHelpSPOut",

       htmlOutput("headerSP"),
       tags$style(HTML(
           "#headerSP {  margin-top:50px;
           font-size:25px;")),

       htmlOutput("spNum1"),
       tags$style(HTML(
           "#spNum1 {  margin-top:25px;
           font-size:20px;")),

       htmlOutput("SPtext1"),
       tags$style(HTML(
           "#SPtext1 {  margin-top:10px;
           font-size:15px;")),

       htmlOutput("spNum2"),
       tags$style(HTML(
           "#spNum2 {  margin-top:25px;
           font-size:20px;")),

       htmlOutput("SPtext2"),
       tags$style(HTML(
           "#SPtext2 {  margin-top:10px;
           font-size:15px;")),

       HTML("<div>"),
       HTML(paste(img(src='SP1.png',
                      style='height: 400px;',
                      vspace="50",
                      style="display: block;
                      margin-left: auto; margin-right: auto;"), "")
       ),
       HTML("</div>"),

       htmlOutput("SPtext3"),
       tags$style(HTML(
           "#SPtext3 {  margin-top:10px;
           font-size:15px;")),

       HTML("<div>"),
       HTML(paste(img(src='SP2.png',
                      style='height: 400px;',
                      vspace="50",
                      style="display: block;
                      margin-left: auto; margin-right: auto;"), "")
       ),
       HTML("</div>"),

       htmlOutput("SPtext4"),
       tags$style(HTML(
           "#SPtext4 {  margin-top:10px;
           font-size:15px;")),

       HTML("<div>"),
       HTML(paste(img(src='SP3.png',
                      style='height: 400px;',
                      vspace="50",
                      style="display: block;
                      margin-left: auto; margin-right: auto;"), "")
       ),
       HTML("</div>")



   ),#end of conditional panel

    conditionalPanel(
        condition = "output.showHelpTestDataOut",
        
        htmlOutput("headerTD"),
        tags$style(HTML(
            "#headerTD {  margin-top:50px;
            font-size:25px;")),
        
        htmlOutput("TDNum1"),
        tags$style(HTML(
            "#TDNum1 {  margin-top:25px;
            font-size:20px;")),
        
        htmlOutput("TDtext1"),
        tags$style(HTML(
            "#TDtext1 {  margin-top:10px;
            font-size:15px;")),
        HTML("<div>"),
        HTML(paste(img(src='test_data.png',
                       style='height: 300px;',
                       vspace="50",
                       style="display: block;
                       margin-left: auto; margin-right: auto;"), "")
        ),
        HTML("</div>"),
        htmlOutput("TDtext2"),
        tags$style(HTML(
            "#TDtext2 {  margin-top:10px;
            font-size:15px;"))
    )#end of conditional panel

),#end of column

    #Spacer row
    HTML("</div>"),
    HTML("<div style='height: 100px;'>"),
    HTML("</div>")
)#end of fixedRow


