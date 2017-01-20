###server of global help page

##control of conditional panels
helpTabs <- reactiveValues(structure=T,upload=F, plateViewer=F,featureTable=F,
                           qualityControl=F,scatterPlot=F)

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



##Global headers
output$helpHeader <- renderUI(
    HTML(paste("
               <b> Help page for <i> HTSvis </i> </b>"
               ))
)

output$helpSubHeader <- renderUI(
    HTML(paste("
               Click on the tabs to open the sub pages"
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
               data from arrayed high-throughput screening (HTS) experiments.
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
                provides a summary table provided as as delimted text file (<i>topTable.txt</i>). 
                This text file has a defined structure and column names can be loaded directly in applciation. 
                Both, the strucutre and
                the column names of the topTable object should not be changed as 
                the application relies on those.
                The expected structure of the topTable is illistrated below."


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
                contain meausured values of replicates. Data from single and dual channel experiments 
                can be loaded in the application. Besides raw measured values, 
                futher columns with normalized values and further metrics are available.
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
that the dataset is <b>symetrical</b> in a way that each experiment contains
the same set of plates and each plate has the same well format.
This is espicially relevant if mutiple experiments are investigated because
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
The specific row and collumn annotation format is: 'RowColmn' with rows as
letters and columns as numbers (e.g. A1 or A01).
Letters can be upper- or lowercase.
Letters and numbers may be separated by
charcaters like '-' or '_'.
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
        (compare I.2 in the 'data structure' help) which is selected from the corresponing 
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
                in one column, the channel selection drop dowm list will be hidden
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
        defined. To indiacte that only a single exeriment is investigated,
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
                are defined, the data can be subitted to the application
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
in the mutiwell plate format. Four plates can be investiagted
simultaneously and each of the four plates has a separate adjustment panel.
This panel allows the user to define which plate of the screen is shown as a
heatmap. In addition, the channel for which the data should be shown can be set
(e.g. cell number). The selection of screen, plate and channel is done by
choosing from dropdown lists. The content of the dropdown lists is extracted
from the annotations of the loaded data set.
Hovering ovter the heatmap will show the well ids
(and annotation if defined, see <i>data uplaod</i> section) and measured values.
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
setting is made (see <i>data uplaod </i> help page), the 'select experiment'
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
               If more than one meadured values per well are defined, those can
               be selected from a drop down list ('select channel') and added. Each column carries
               either a sort (for identifier columns) or a filter option
               (for numeric columns). The sorted and filtered table can be
               downloaded as a <i>.csv </i>  table with columns separated by ','
                and '.' as decimal separator."
    ))
)

output$FTtext3 <- renderUI(
    HTML(paste("
               If the table contains more than one measured value per well, a
                reactive heatmap can be created from selected rows. Rows are
               selected and unselected by clicking. The heatmap will be
               hierarchically clustered and can be downloaded using the downlaod
               handler below."
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
Controls are selected by defining their position on the plate. <b>Importantly</b>, the
position of the controls is fixed for all plates within one experiment upon selection.
The upper left plot shows all data points of one screen as plate-wise data
series (data points of one plate are plotted at one x-axis position). If control populations are defined,
the median of controls will be plotted as colored dots.
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
of the population will plotted as colored dots in the upper left plot.</br>
</br><b>Important note:</b> the
position of the controls is fixed for all plates within one experiment. The drop
down list to choose the plate changes exclusively the visualized heatmap and
does not allow to define plate specific control wells."
    ))
)

output$QCtext3 <- renderUI(
    HTML(paste("
    Two populations, namely positive and negative cotrols are
    required to generate the KDE plot at the upper right panel.
    A quantitative assesment of the statistical effect size is provided
    by the robust Z'-factor which is printed in the legend of the KDE plot.

"
    ))
)

output$QCtext4 <- renderUI(
    HTML(paste("
A third population is reserved for non-targeting controls. The data points
allocated to this population will be added to all representations. Single wells
can be unselected by a second click, all populations can be emtied by with the
rseset button.

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
The two data series are supposed to be of uniform length and are plotted against each other in a cartesian grid.
As an example, the correlation between replicate screens can be estimated based on the plot.
               "
    ))
)


output$SPtext2 <- renderUI(
    HTML(paste("
Which experiment and channel the data should be plotted for, is defined by choosing from drop-down lists.
If the'single experiment' choice is set, data series of single plates within one experiment
can be plotted against each other. A regression line can be added to the plot
by setting the corresponding action in the control panel. The equation,
goodness of the linear mdoel fit and correlation coefficients (Pearson and Spearman) will be displayed
along with the regression line.
"
    ))
)


output$SPtext3 <- renderUI(
    HTML(paste("
By drawing a rectangle (move the pushed mouse cursor) on the plot
region further options become available. Those options include a zoom window and
the 'population manager'. Upon brushing, a zoom button and a second control panel appear.
A button which appears on to top allows to switch between the inital panel and the
population manager."
    ))
)

output$SPtext4 <- renderUI(
    HTML(paste("
The population manager allows to define the data points which are embedded
by the rectangle as a subpopulation. The population name is typed in the
provided text input bar. Upon submission by clicking the 'create subpopulation'
bytton, the population will be created. A random color will be assigned to the
population. All defined populations are listed in a reactive table.
               "
    ))
)
