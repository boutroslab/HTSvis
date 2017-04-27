


fixedRow(
                     # ####Spacer row
                     # HTML("<div style='height: 25px;'>"),
                     # HTML("</div>"),
                     column(12,
                            align = "left",
                            h2(paste("HTSvis: a web app for exploratory data analysis",
                                      "and visualization of arrayed high-throughput screens",sep = " ")),
                            HTML("<div style='height: 25px;'>"),
                            HTML("</div>"),
                            h3(paste0("About")),
                            h4(paste(
                                "Arrayed high-throughput screens (HTS) cover a broad range of applications
                                using RNAi or small molecules as perturbations and specialized software packages
                                for statistical analysis have become available. 
                                However, exploratory data analysis and integration of screening results has 
                                remained challenging due to the size of the data sets and the lack of 
                                user-friendly tools for interpretation and visualization of screening results. 
                                Here we present HTSvis, a web application to interactively visualize raw data, 
                                perform quality control and assess screening results from single to multi-channel 
                                measurements such as image-based screens. 
                                Per well aggregated raw and analyzed data of various assay types and 
                                scales can be loaded in a generic tabular format.", sep=" ")),
                            HTML("<div style='height: 25px;'>"),
                            HTML("</div>"),
                            h3("Manual"),
                            h4(paste("A comprehensive manual",
                                     "is provided on the help page.",
                                "Example data sets can be found under the following link:",
                                     " "),sep=" "),
                            h4(a("http://b110-wiki.dkfz.de/confluence/display/HTSvis",
                                 href="http://b110-wiki.dkfz.de/confluence/display/HTSvis")),
                            HTML("<div style='height: 25px;'>"),
                            HTML("</div>"),
                            h3("Additional notes"),
                            h4(paste("A  local version of HTSvis for download and information concerning licesning",
                                     "can be found at:"),sep=" "),
                            h4(a("https://github.com/boutroslab/HTSvis", href="https://github.com/boutroslab/HTSvis")),
                            HTML("<div style='height: 25px;'>"),
                            HTML("</div>"),
                            h3("Contact"),
                            h4("c.scheeder@dkfz.de"),
                            h4("f.heigwer@dkfz.de"),
                            h4("m.boutros@dkfz.de"),
                            HTML("<div style='height: 50px;'>"),
                            HTML("</div>"),
                            align = "center",
                            HTML(paste(icon("copyright"),"DKFZ - HTSvis Version 1.2.0")),
                            HTML("<div style='height: 25px;'>"),
                            HTML("</div>")
                     )
)