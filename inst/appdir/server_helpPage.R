###server of global help page

##control of conditional panels
helpTabs <- reactiveValues(structure=T,upload=F, plateViewer=F,featureTable=F,
                           qualityControl=F,scatterPlot=F,options=F,testData=F)

observeEvent(input$showStructure,{
    for(i in names(helpTabs)) {
        helpTabs[[i]] <- F
    }
    helpTabs$structure <- T
})


observeEvent(input$showUpload,{
    for(i in names(helpTabs)) {
        helpTabs[[i]] <- F
    }
    helpTabs$upload <- T
})


observeEvent(input$showPlateViewer,{
    for(i in names(helpTabs)) {
        helpTabs[[i]] <- F
    }
    helpTabs$plateViewer <- T
})


observeEvent(input$showHelpPV,{
    for(i in names(helpTabs)) {
        helpTabs[[i]] <- F
    }
    helpTabs$plateViewer <- T
})


observeEvent(input$showHelpFT,{
    for(i in names(helpTabs)) {
        helpTabs[[i]] <- F
    }
    helpTabs$featureTable<- T
})


observeEvent(input$showHelpQC,{
    for(i in names(helpTabs)) {
        helpTabs[[i]] <- F
    }
    helpTabs$qualityControl<- T
})

observeEvent(input$showHelpSP,{
    for(i in names(helpTabs)) {
        helpTabs[[i]] <- F
    }
    helpTabs$scatterPlot<- T
})

observeEvent(input$showHelpOp,{
    for(i in names(helpTabs)) {
        helpTabs[[i]] <- F
    }
    helpTabs$options<- T
})

observeEvent(input$showTestData,{
    for(i in names(helpTabs)) {
        helpTabs[[i]] <- F
    }
    helpTabs$testData<- T
})


output$showStructureOut <- reactive({
    return(helpTabs$structure)
})
outputOptions(output, "showStructureOut", suspendWhenHidden=FALSE)


output$showUploadOut <- reactive({
    return(helpTabs$upload)
})
outputOptions(output, "showUploadOut", suspendWhenHidden=FALSE)




output$showHelpPVOut <- reactive({
    return(helpTabs$plateViewer)
})
outputOptions(output, "showHelpPVOut", suspendWhenHidden=FALSE)


output$showHelpFTOut <- reactive({
    return(helpTabs$featureTable)
})
outputOptions(output, "showHelpFTOut", suspendWhenHidden=FALSE)


output$showHelpQCOut <- reactive({
    return(helpTabs$qualityControl)
})
outputOptions(output, "showHelpQCOut", suspendWhenHidden=FALSE)


output$showHelpSPOut <- reactive({
    return(helpTabs$scatterPlot)
})
outputOptions(output, "showHelpSPOut", suspendWhenHidden=FALSE)

output$showOptionsOut <- reactive({
    return(helpTabs$options)
})
outputOptions(output, "showOptionsOut", suspendWhenHidden=FALSE)


output$showHelpTestDataOut <- reactive({
    return(helpTabs$testData)
})
outputOptions(output, "showHelpTestDataOut", suspendWhenHidden=FALSE)



##Global headers
output$helpHeader <- renderUI(
    HTML(paste("
               <b> Help page for <i> HTSvis </i> </b>"
               ))
)

output$helpSubHeader <- renderUI(
    HTML(paste("
                Below you will find detailed explanations 
                and instructions for each tab which can be opened 
                by clicking on the tabs below. 
                The tab 'examples' contains a link to example data sets
                and an example step-by-step analysis workflow.
                <br/>
                <br/>
                <b>Tips</b>: 
                <br/>
                <br/>
                - You can restart the app by refreshing your browser
                <br/>
                <br/>
                - If you're running HTSvis on a laptop, low battery might slow down the application 
               "
    ))
    )



##Data structure texts
output$helpChOneNum <- renderUI(
    HTML(paste("
               <b>Data structure</b>"
    ))
)


output$helpChOneOneNum <- renderUI(
    HTML(paste("
               <b> I. Global structure of the data set</b>"
    ))
)


output$helpChOne <- renderUI(
    HTML(paste("
                <i> HTSvis </i> is a web application for the visualization of
               data from arrayed high-throughput screening (HTS) experiments
                with per well measurements.
                Tools to aggregate data with sub-well single cell measurements from microscopy screens are 
               available in CellProfiler Analyst, for example.
               Such experiments typically spread over sets of plates, which are
               screened in replicates and possibly under different conditions
               (see illustration below). Formats supported by the application
               are 6-,12-,48-,96- and 384-well plates."
    ))
)

output$helpChOne2 <- renderUI(
    HTML(paste("
                Data input requires a tabular
                format (.csv, .txt or .RData).
                The input data table can be either a result file
                from a statistical analysis using the Bioconductor <i>R</i> package 
                cellHTS 
                or a generic spread sheet table (e.g. raw data). 
                The two possible types of input tables, require different structures: 
                <br/>
                <br/>
                <b>1. cellHTS result file (<i>topTable.txt</i>)</b>
                </br>
                The cellHTS package
                provides a summary table provided as delimited text file (<i>topTable.txt</i>). 
                This text file has a defined structure and column names and can be loaded directly in application. 
                Both, the structure and
                the column names of the topTable object should not be changed as 
                the application relies on those.
                The expected structure of the topTable is illustrated below."


                # Measured values (features) per well are represented as
                # column entries in the data table.
                # In any case, at least two identifier columns are
                # required: the well and plate annotation.
                # Those columns assign the measured values
                # (termed <i>feature</i> in the application) to the well position
                # and the plate.
                # Columns containing
                # additional information such as an annotation can be
                # present.
                # Accordingly, the
                # data table contains row-wise entries per well as
                # illustrated in the example below.
                # If an experiment contains multiple plates, the rows of each plate
                # are pasted one below the other.<br/>
                # <br/>
                # If a
                # single experiment is uploaded, you can upload your data table in the format
                # as illusrated below. The annotation column is optional."

    ))
)


output$helpChOne3 <- renderUI(
    HTML(paste("
                Well and plate annotation columns are absolutely required.
                The annotation column is optional and can contain any kind of 
                per-well annotation. 
                The topTable will contain additional columns with data points assigned 
                to each well. Depending on the design of the experiment, those columns
                contain measured values of replicates. Data from single and dual channel experiments 
                can be loaded in the application. Besides raw measured values, 
                further columns with normalized values and further metrics are available.
                <br/>
                </br>
                <b>2. Generic data table from arrayed screens </b>
                </br>
                At least two identifier columns are
                required for a table to be loaded in the application: the well and plate annotation.
                Those columns assign the sample values
                (termed <i>channels</i> in the application) to the well position
                and the plate.
               Sample values  per well are represented as
                column entries in the data table. The number of channels per well 
                is not limited. 
                Columns containing
                additional information such as an annotation can be
                present.
                Accordingly, the
                data table contains row-wise entries per well as
                illustrated in the example below.
                If an experiment contains multiple plates, the rows of each plate
                are pasted one below the other.<br/>
               "

    ))
)

output$helpChOne4 <- renderUI(
    HTML(paste("
                If the table contains individual sets of plates 
                (e.g. replicates) each well has to be assigned 
                to those. Accordingly, an additional annotation column 
                has to be present. The required structure is illustrated below. 
                Importantly, in the case of such a data structure the individual
                sets have to contain the same collection of plates 
                (same identifiers are required)."

    ))
)



output$helpChOneTwoNum <- renderUI(
    HTML(paste("
               <b> II. Specific requirements </b>"
    ))
)

output$helpStructure2 <- renderUI(
    HTML(paste("
An <b>important requirement</b> concerning the data structure is
that the dataset is <b>symmetrical</b> in a way that each experiment contains
the same set of plates and each plate has the same well format.
This is especially relevant if multiple experiments are investigated because
the plate identifiers need to be consistent between the
experiments.
Furthermore the data set has to be <b>complete</b>
which means that all experiments need to contain the same number of plates.
All plates further have to be complete in respect to the well format with
numeric entries for all wells.
Missing values (or such flagged during statistical analysis) should be
filled with NA or NaN.<br/>
<br/>
The <b>well annotation</b> has to follow an alphanumerical
coding with column positions indicated by numbers and row positions
by letters (see illustration below).
The specific row and column annotation format is: 'RowColmn' with rows as
letters and columns as numbers (e.g. A1 or A01).
Letters can be upper- or lowercase.
Letters and numbers may be separated by
characters like '-' or '_'.
               "
    ))
)

output$helpStructure3 <- renderUI(
    HTML(paste("
Delimited tables (.txt, .csv) should be uniformly separated
by tab, comma, semicolon or space and all columns
should be named. <br/>
"
    ))
)




##Data upload texts
output$headerUpload <- renderUI(
    HTML(paste("
               <b>Data upload</b>"
    ))
)

output$uploadNum1 <- renderUI(
    HTML(paste("
               <b>I. General notes</b>"
    ))
)


output$uploadText1 <- renderUI(
    HTML(paste("
                A file browser is available to read data into the application.
                Once the data is uploaded, a short notice with a summary of the
                uploaded data table's dimensions will appear on the right. 
                Upon successful data upload, a second file browser 
                will appear below the first one. This uploader can be used 
                to upload a session parameter file which is described in the 
                help page. If no session parameter file is used, the second uploader
                can be ignored (as in the illustrations below).
                Drop down lists containing the column
                names will appear below the file browser (see illustration below).
               "
    ))
)

output$uploadText2 <- renderUI(
    HTML(paste("
        After a successful data upload, the columns containing the annotations and sample values per well
        can be selected from all columns using the drop down lists. 
        As indicated, the annotation is defined
        by the <i>well</i>, <i>plate</i> and <i>experiment</i> dimension. The
        measured values per well are selected in the <i>channel</i> dimension.
        An additional <i>annotation</i> per well is optional.
        <br/>
        <br/>
        Depending on the structure of the input data table, the experiment dimension
        varies. 
        The default setting is this respect matches a generic data table with multiple series of
        plates (e.g. multiple experiments/replicates). 
        In this case, a column with the experiment allocation has to be present 
        (compare I.2 in the 'data structure' help) which is selected from the corresponding 
        drop down list. Sample values per well are selected from a 
        separate drop down list as illustrated below. 
        
        "
    ))
)

output$uploadText3 <- renderUI(
    HTML(paste("
                If the data input table is a <b>cellHTS</b> result table
                (<i>topTable.txt</i>), the first checkbox next to the experiment 
                drop down list has to be set.
                As the experiment annotation and <i>channel</i> dimension are combined
                in one column, the channel selection drop down list will be hidden
                subsequently (see illustration below). All columns of interest 
                containing numeric values per well can be chosen from the drop down list.
                An annotation column can be chosen optionally. 
               "
    ))
)


output$uploadText4 <- renderUI(
    HTML(paste("
        If a single experiment is investigated (i.e. a single series
        of plates with one or more measures values per well),
        the well, plate and channel dimension have to be
        defined. To indicate that only a single experiment is investigated,
        the lower checkbox on the overview panel has to be set.
        A 'single_experiment' placeholder will
        be pasted in all dropdown lists for the experiment dimension.
        (see illustration below)
               "
    ))
)


output$uploadText5 <- renderUI(
    HTML(paste("
                Once the data input is complete and all required inputs
                are defined, the data can be submitted to the application
                with the 'Explore Data' button (button will only appear if 
                all required settings are defined). If inputs are changed afterwards
                those have to be submitted by pushing this button again.
                Upon successful data input, a message with a summary
                of the uploaded table will appear next to
                the button.

               "
    ))
)


##Plate Viewer tab help
output$headerPV <- renderUI(
    HTML(paste("
               <b>Plate viewer</b>"
    ))
)

output$pvNum1 <- renderUI(
    HTML(paste("
               <b>I. General notes and usage</b>"
    ))
)

output$PVtext1 <- renderUI(
    HTML(paste("
The plate viewer tab provides heatmap representation of the data
in the muti-well plate format. Four plates can be investigated
simultaneously and each of the four plates has a separate adjustment panel.
This panel allows the user to define which plate of the screen is shown as a
heatmap. In addition, the channel for which the data should be shown can be set
(e.g. cell number). The selection of screen, plate and channel is done by
choosing from dropdown lists. The content of the dropdown lists is extracted
from the annotations of the loaded data set.
Hovering over the heatmap will show the well ids
(and annotation if defined, see <i>data upload</i> section) and measured values.
               "
    ))
)



output$PVtext2 <- renderUI(
    HTML(paste("
As an additional functionality,
the color scale for each plate can be defined by the user. By default, the color
scale is spanned between the minimum and maximum values of each plate.
The actual numeric values of the minimum and maximum data points are shown in a
legend below the color key. The user has the ability to define the range over
which the color scale is spanned by typing the intended values for the lower
and/or upper limit in text boxes. The color scale can be reset to the default
setting by clicking the button
below each color scale."
    ))
)

output$PVtext3 <- renderUI(
    HTML(paste("
By opening the synchronization panel, the selections and settings (selected plate
and color scale) made on the upper left plate can be transferred to the
remaining plates. Heatmaps can be downloaded by clicking on the small
icons in the upper right corner of each heatmap. If the <i>single experiment </i>
setting is made (see <i>data upload </i> help page), the 'select experiment'
drop down list will be inactive."
    ))
)



##Feature Table tab help
output$headerFT <- renderUI(
    HTML(paste("
               <b>Feature Table</b>"
    ))
)

output$ftNum1 <- renderUI(
    HTML(paste("
               <b>I. General notes and usage</b>"
    ))
)


output$FTtext1 <- renderUI(
    HTML(paste("
               The <i>Feature Table</i> tab provides a browseable table
            of the uploaded table. The table provides a global search bar,
           column sort and filter options and is downloadable."
    ))
)

output$FTtext2 <- renderUI(
    HTML(paste("
               By default, only the first measured value is shown in the table.
               If more than one measured values per well are defined, those can
               be selected from a drop down list ('select channel') and added. 
                The button 'select all channels' can be used to select all 
                loaded channels. 
                Each column carries
               either a sort (for identifier columns) or a filter option
               (for numeric columns). Note that the data values shown in the table are rounded to 3 digits.
                The sorted and filtered table can be
               downloaded as a <i>.csv </i>  table with columns separated by ','
                and '.' as decimal separator."
    ))
)

output$FTtext3 <- renderUI(
    HTML(paste("
               If the table contains more than one measured value per well, a
                reactive heatmap can be created from selected rows. Rows are
               selected and unselected by clicking. If the shown table has less 
                than 200 row entries, the button 'select all rows' below 
                the table can be used to select all rows. The table size can be reduced by 
                the column filters above each column.
                The heatmap will be
               hierarchically clustered and can be downloaded using the download
               handler below. By default, the color scale of the heatmap is spanned
               between the minimum and maximum value of the data used for the heatmap.
               Row and column scaling can be applied using radio buttons below the heatmap. 
               For further information concerning the row/column scaling can be found 
            in the documentation of the R package <i>heatmap.2</i>"
    ))
)



##QC tab help
output$headerQC <- renderUI(
    HTML(paste("
               <b>Quality Control</b>"
    ))
)

output$qcNum1 <- renderUI(
    HTML(paste("
               <b>I. General notes</b>"
    ))
)

output$QCtext1 <- renderUI(
    HTML(paste("
A statistical assessment of the data is provided in the 'Quality Control' tab.
Statistical metrics and plots are calculated on the basis of positive and negative controls which
are user-defined.
Controls are selected by defining their position on the plate. 
This can be done by either defining the control wells per single plate
or cumulatively for all plates within one experiment.
The upper left plot shows all data points of one screen as plate-wise data
series (data points of one plate are plotted at one x-axis position). If control populations are defined,
the arithmetic mean of control samples will be plotted as colored dots.
The upper right plot shows the density distribution (Kernel density distribution)
of positive and negative controls and further provides
the robust Z’-factor (Birmingham, A. et al., <i>Nat. Methods</i>, 2009) for the experiment.
A boxplot of chosen controls for all plates is provided in the lower left plot.
All plots are created for data of one screens and channel, both can be selected from drop-down lists."
    ))
)

output$qcNum2 <- renderUI(
    HTML(paste("
               <b>I. Usage</b>"
    ))
)

output$QCtext2 <- renderUI(
    HTML(paste("
The experiment and measured channel for which the quality control plots should
be created are defined from drop down lists at the top.
Up to three control populations can be defined: positive, negative and non-targeting
controls. Populations are defined by selecting the corresponding wells. A
heatmap which represents the plate layout is provided to select (and unselect)
the wells by clicking. The population to define is determined by setting a check above the heatmap.
If at least one population is defined, a box plot will be generated and the median
of the population will be plotted as colored dots in the upper left plot.</br>
The controls can be defined for each plate separately from the drop down list 
above the plate map.
A checkbox above the plate map can be used to enable the option to define 
the controls for all plates within the data set. This is useful, 
if the controls are always on the same position within the set. When the 
'all plates' check is set, the plate map will be white and the dropdown list will
be inactive."
    ))
)

output$QCtext3 <- renderUI(
    HTML(paste("
    Two populations, namely positive and negative controls are
    required to generate the KDE plot at the upper right panel.
    A quantitative assessment of the statistical effect size is provided
    by the robust Z'-factor which is printed in the legend of the KDE plot.

"
    ))
)

output$QCtext4 <- renderUI(
    HTML(paste("
A third population is reserved for non-targeting controls. The data points
allocated to this population will be added to all representations. Single wells
can be unselected by a second click, all populations can be emptied by with the
reset button.

               "
    ))
)


##Scatter Plot tab help
output$headerSP <- renderUI(
    HTML(paste("
               <b>Scatter Plot</b>"
    ))
)

output$spNum1 <- renderUI(
    HTML(paste("
               <b>I. General notes</b>"
    ))
)

output$spNum2 <- renderUI(
    HTML(paste("
               <b>II. Usage</b>"
    ))
)

output$SPtext1 <- renderUI(
    HTML(paste("
The 'Scatter Plot' tab is an additional visualization tool to interpret the data.
A two-dimensional graphical visualization of the relation between two data series is possible.
Each of the two data series represents values for one channel.
The two data series are supposed to be of uniform length and are plotted against each other in a Cartesian grid.
As an example, the correlation between replicate screens can be estimated based on the plot.
               "
    ))
)


output$SPtext2 <- renderUI(
    HTML(paste("
Which experiment and channel the data should be plotted for, is defined by choosing from drop-down lists.
If the 'single experiment' choice is set, data series of single plates within one experiment
can be plotted against each other. A regression line can be added to the plot
by setting the corresponding action in the control panel. The equation,
goodness of the linear model fit and correlation coefficients (Pearson and Spearman) will be displayed
along with the regression line.
"
    ))
)


output$SPtext3 <- renderUI(
    HTML(paste("
By drawing a rectangle (move the pushed mouse cursor) on the plot
region further options become available. Those options include a zoom window and
the 'population manager'. Upon brushing, a zoom button and a second control panel appear.
A button which appears on to top allows to switch between the initial panel and the
population manager."
    ))
)

output$SPtext4 <- renderUI(
    HTML(paste("
The population manager allows to define the data points which are embedded
by the rectangle as a subpopulation. The population name is typed in the
provided text input bar. Upon submission by clicking the 'create subpopulation'
button, the population will be created. A random color will be assigned to the
population. All defined populations are listed in a reactive table.
               "
    ))
)

## Options
output$headerOp <- renderUI(
    HTML(paste("
               <b>Options</b>"
    ))
)

output$OpNum1 <- renderUI(
    HTML(paste("
               <b>I. General notes</b>"
    ))
)

output$Optext1 <- renderUI(
    HTML(paste("
             HTSvis provides an option to download a session 
                parameter file. The downloaded
               file contains the Data Input parameters as set 
               on the 'Data Input tab' and the controls 
               defined in the 'Quality Control tab'.
               The session parameter file is a .csv file and
               should not be edited. As ';' will be used to 
               format the .csv, the columns of the input table
               for which the parameters will be saved 
               should not contain ';'. 
               "
    ))
)


output$OpNum2 <- renderUI(
    HTML(paste("
               <b>II. Downloading the session parameter file</b>"
    ))
)
output$Optext2 <- renderUI(
    HTML(paste("
                The option to download a session parameter file is provided via 
                a download button on the 'Options' tab. 
                The session parameter file will be saved as a .csv and contains 
                the settings made on the 'Data Input' as well as 
                the control wells chosen on the 'Quality Control' tab.
                Please start a new session 
                (open a new browser window) when loading a session 
               parameter file. 
               "
    ))
)

output$OpNum3 <- renderUI(
    HTML(paste("
               <b>III. Uploading the session parameter file</b>"
    ))
)
output$Optext3 <- renderUI(
    HTML(paste("
                After uploading a data set as described on 
                'Data Input' help tab, a second file uploader appears below. 
                This file uploader can be used to upload a session parameter file. 
                If the parameter saved in the session parameter file match the 
                input data those will be directly applied as soon as the session parameter 
                file is uploaded. 
                If the session parameter do not match the input data a pop
                with a corresponding notice will appear.              "
    ))
)

output$Optext4 <- renderUI(
    HTML(paste("
                In addition to the parameter on the 'Data Input' tab,
                the session parameter file further contains the position of controls wells 
                as set in 'Quality Control' tab. 
                If the session parameter file contains this information (not mandatory),
                this will be recognized and a corresponding button ('load controls) to apply the loaded control
                positions, will appear in the quality control tab. 
                           "
    ))
)

output$Optext5 <- renderUI(
    HTML(paste("
               Once the button is clicked,
                the position of the control wells be applied 
                to the loaded data and also indicated on the plate map.
                If the session parameter file contains 
                individual controls per plate (i.e. 'all plates' is unchecked upon
                saving the session parameter file), each plate hat to be selected from 
                the drop down list to apply the controls per plate after pushing the 
                button. 
               "
               ))
)

output$Optext6 <- renderUI(
    HTML(paste("
               "
    ))
)




## Examples 
output$headerTD <- renderUI(
    HTML(paste("
               <b>Example Data</b>"
    ))
)

output$TDNum1 <- renderUI(
    HTML(paste("
               <b>I. General notes</b>"
    ))
)


output$TDtext1 <- renderUI(
    HTML(paste("
               Example data sets and description file of those can be found 
                under the following link:
               "
    ))
)

output$TDlink <- renderUI(
    h5(a("http://b110-wiki.dkfz.de/confluence/display/HTSvis", href="http://b110-wiki.dkfz.de/confluence/display/HTSvis"))
    )

output$TDtext2 <- renderUI(
    HTML(paste("
              The following example data sets are provided:
               "
    ))
)

output$TDNum2 <- renderUI(
    HTML(paste("
               <b>II. Example workflow</b>"
    ))
)

output$TDtext3 <- renderUI(
    HTML(paste("
               Below  you will find an step-by-step example workflow using HTSvis to explore and
                analyze the humanSGI data set from the example data repository.
               "
    ))
)

output$TDwf1 <- renderUI(
    HTML(paste("
               <b>Data Upload</b>
               "
    ))
)

output$TDwfT1 <- renderUI(
    HTML(paste("
            After downloading the example data set 'humanSDI.RData' from 
            the above mentioned link, the app is started by opening it 
            in a web browser. When the app is launched, the file uploader appears on the 
            'Data Input' tab and we can
            upload the 'humanSGI.RData' data set. All other besides than the 'Data Input' and
            the 'Help Page' are initially inactive. After the upload,
            further options appear as shown on the screenshot below. An info text
            with a brief summary of the uploaded table to the right of the 
               file uploader indicates a successful upload."
    ))
)

output$TDwfT2 <- renderUI(
    HTML(paste("
               Below the file uploader which we used to upload the main data set, a
                second file uploader has appeared. This uploader can be used to 
                upload a session parameter file from a previous session. 
                As we do not have such a session parameter file yet, 
                we do not use this uploader. 
               "
    ))
)

output$TDwf2 <- renderUI(
    HTML(paste("
               <b>App Configuration</b>
               "
    ))
)

output$TDwfT3 <- renderUI(
    HTML(paste("
               In the next step we have to define which column of the data set 
               contains which information.
              As mentioned in the 'Data Structure' part of the 'Help Page',
               HTSvis has been designed for data from arrayed high-throughput screens 
            in a tabular format. Columns containing the annotation of each 
               measured value have to be chosen accordingly. 
               A dropdown list with the column names of the uploaded table is available
               for each of the required annotation columns:
               well, plate, experiment and channel. The annotation column is optional.
                <br/>
                <br/>
               We now select the corresponding columns from the data set for each of the dimensions.
               The columns containing the 'well' and 'plate' dimension are named accordingly. 
               The 'humanSGI' contains multiple experiments, in this case replicates. This is annotated in
               'replicate' column. As the 'humanGSI' data set contains more than one experiment and 
               has not been analyzed using cellHTS, none of the check boxes on the checkboxes on has to be set. 
                For the loaded set a per-well annotation is available and we choose the column. 
                In the final step the column/s containing the measured values per well are defined. 
                in this case we have multiple measured values per well (called 'channels' in HTSvis) and we select all
                columns that do not contain annotations.
              If all required settings are made, the 'explore data' button will appear on the lower right corner. 
               When the user hits this button, the data will be loaded into the application and all 
               tabs will be activated. As the data set contains some missing values you will get a note indicating this."
              
    ))
)

output$TDwf3 <- renderUI(
    HTML(paste("
               <b>Data exploration</b>
               "
    ))
)

output$TDwfT4 <- renderUI(
    HTML(paste("
                After the data upload and app configuration, we can now explore the data set
                using the visualization  tools available on the tabs. 
                The order in which the tabs is up to the user,
                for reasons of simplicity we will use them from left to right as they appear 
                in the application. 
              "
               
    ))
)

output$TDwf4 <- renderUI(
    HTML(paste("
               <b>Plate Viewer</b>
               "
    ))
)

output$TDwfT5 <- renderUI(
    HTML(paste("
                The 'Plate Viewer' tab provides a tool to visualize the data in the 
                muti-well format and to check for experimental artifacts, e.g. edge
                effects. For our example data sets we see that all values of 
                the first row and last column are plotted in black, by hovering over 
                those wells we see that those wells contain 'NA' values. In this
                case this resulted from the experimental design and the corresponding 
                wells were left empty and flagged with 'NA' during data collection. 
                Another possible application of the 'Plate Viewer' is to identify strong outliers, 
                as for example wells 'N23' and 'P23' on all plates. Further features are explained in
                'Plate Viewer tab' section of the help page. 
               "
               
    ))
)

output$TDwf5 <- renderUI(
    HTML(paste("
               <b>Feature Table</b>
               "
    ))
    )

output$TDwfT6 <- renderUI(
    HTML(paste("
               The 'Feature Table' tab provides a tabular representation of the 
                input table. We can for example apply column filters to define 
                a subset of the table and save the filtered table. 
                Another helpful tool is the heatmap which can be created 
                by selecting rows (if more than one measured channel per well 
                is available) which will then be added to the heatmap. 
                The shown table initially only contains the first measured channel 
                per well. We click the 'select all channels' button to add all 
                measured channels to the table. We can then select interesting 
                wells by clicking, for example well 'B1' on all plates containing the
                string 'N1, to get a heatmap representation of those.  Further features are explained in
                'Feature Table tab' section of the help page. 
               "
               
    ))
)

output$TDwf6 <- renderUI(
    HTML(paste("
               <b>Quality Control</b>
               "
    ))
    )

output$TDwfT7 <- renderUI(
    HTML(paste("
               The 'Quality Control' tab provides the possibility to 
                perform quality control checks per experiment based on visualizations and 
                quality control metrics using control wells.
                We first select a measured channel and an experiment. Due to it's biological 
                meaning we select the 'cell count' channel for this example.  
                As explained in the example data repository, positive controls 
                for cell viability of different strength are located in wells of every second row 
                of column 23 starting from row B (B23 – P23). 
                Negative controls are located in the alternating pattern of the 
                positive controls in column 23 (C23 - 023). 
                Controls can be selected by clicking on the corresponding 
                wells on a plate map at the lower right. 
                For this example, 
                the controls are on the same position on each plate, we set the 
                'all plates' check above the plate map. 
                We then select the controls well as mentioned above. 
                For this example, we have only two control populations: positive 
                and negative controls. When the controls are set, the quality control 
                plots are created.
                The scatter plot at the upper left gives an overview over the entire 
                screen. This can be helpful to assess the overall performance of the screen, 
                for example to estimate whether and how many plates failed. 
                The box plot complements the information from the scatter plot 
                by providing summary statistics. 
                The density plot at the upper right shows the density distribution of the control populations.
                In this example we see that the positive control populations show 
                a bipartite distribution. In addition, the Z'-factor
                is given in the legend of the density plot (for more information 
                concerning the Z'-factor see Birmingham, A. et al., <i>Nat. Methods</i>, 2009). 
                As indicated in the description text of the 
                'humanSGI' data set, the positive controls are of different strength and 
                we can unselect wells again by clicking on the corresponding positions on the 
                plate map. This allows to compare how well the control populations are 
                separated depending on which wells are selected. 
                Further features are explained in
                'Quality control tab' section of the help page. 
               "
               
    ))
)

output$TDwf7 <- renderUI(
    HTML(paste("
               <b>Scatter Plot</b>
               "
    ))
)

output$TDwfT8 <- renderUI(
    HTML(paste("
               The 'Scatter Plot' tab provides features for 
                further quality controls steps as well as 
                for exploratory data analysis. 
                In a first step we use the scatter plot tab to 
                assess the correlation between to two replicates 
                based on the cell count and set the 
                required selections in the drop down lists.
                We also set the check below the drop down 
                lists to get numerical values describing the 
                relation between the two replicate data sets. 
               "
               
    ))
)


output$TDwfT9 <- renderUI(
    HTML(paste("
               We next would like investigate the data in a more 
                exploratory manner. For this purpose we initially uncheck 
                the linear model checkbox for reasons of clarity. 
                In this example we are interested in the data points 
                located close to the point of origin for which a low cell viability
                was observed in both replicate measurements. 
                We can get the identity of those points by drawing a rectangle 
                around those using the left mouse button.
                Once the rectangle is drawn, the panel on the left will change
                to the so called 'population manager'.
                population (here we name it 'low_viability'). 
                As soon as we create the population by clicking the 'create population' 
                button a table will appear. 
                This table will contain all population we create. 
                Inside the table, we can push the 'get info' button
                in the row of our 'low-viability' population. 
                Triggering of the 'get info' button will open a table providing 
                the info from the data table associated with the brushed points. 
                In this way we can review certain hypothesis, for example which reagents 
                induce a strong viability effect. This information can then be used to review 
                certain hypotheses.  "
               
    ))
)

output$TDwfT10 <- renderUI(
    HTML(paste("
               "
               
    ))
)

output$TDwfT11 <- renderUI(
    HTML(paste("
               Once a population is defined it will be saved also if the input 
                data changes. To change the input data we can use the button at the 
                top of the panel (initially has the status TRUE because the
                population manager is active). We use this button and then change 
                the data in respect to the selected channel. We select 'cell eccentricity'
                to review whether the reagent that induced a low cell viability also
                influence cell eccentricity. As all other plots in the application the 
                scatter plot can be saved. 
                Further features are explained in
                'Scatter Plot tab' section of the help page. 
               "
               
    ))
)

output$TDwf8 <- renderUI(
    HTML(paste("
               <b>Options</b>
               "
    ))
)

output$TDwfT12 <- renderUI(
    HTML(paste("
               Once we end the session we can save some of the parameter 
                of the current session. This can be done on the 
            'options' tab by clicking the download button. A .csv table 
                will be saved subsequently that can be loaded when the app is started. 
                Further information concerning the session parameter option
                can be found on the 
                'option tab' section of the help page. 
               "
               
    ))
)

