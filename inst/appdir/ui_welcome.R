


fixedRow(
                     # ####Spacer row
                     # HTML("<div style='height: 25px;'>"),
                     # HTML("</div>"),
                     column(12,
                            h2(paste0("HTSvis: a web app for exploratory data analysis",
                                      "and visualization of arrayed high-throughput screens")),
                            h3(paste0("About")),
                            h4(paste(
                                "Arrayed high-throughput screens (HTS) cover a broad range", 
                                "of applications such as RNAi or small molecule screening.",
                                "Specialized software for statistical analysis and visualization",
                                "is available to people trained in bioinformatics.",
                                "For many biologists however, exploratory data analysis",
                                "and integration of screening results is challenging",
                                "due to the size of produced data tables and the lack of",
                                "user-friendly tools to interpret and visualize screening results.",
                                "Here we present HTSvis, a web application to interactively",
                                "visualize raw data, perform quality control and assess screening",
                                "results from single- to multichannel measurements such as image-based screens.",
                                "Per well aggregated raw and analyzed data of various assay types", 
                                "and scales can be loaded in a generic tabular format.", sep=" ")),
                            h3("Manual"),
                            h4(paste("A comprehensive manual including a link to example data",
                                     "is provided on the help page"),sep=" "),
                            h3("Additional notes"),
                            h4(paste("A  local version of HTSvis",
                                     "can be downloaded at:"),sep=" "),
                            h4(a("https://github.com/boutroslab/HTSvis", href="https://github.com/boutroslab/HTSvis")),
                            h3("Contact"),
                            h4("c.scheeder@dkfz.de"),
                            h4("f.heigwer@dkfz.de"),
                            h4("m.boutros@dkfz.de")
                     )
)