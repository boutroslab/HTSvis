
fixedRow(
  column(2,
         div(DT::dataTableOutput('definedWells'),
             style = "font-size: 95%; width: 85%")
  ),
  column(2,
         offset=1,
         div(DT::dataTableOutput('definedPlates'),
             style = "font-size: 95%; width: 85%")
  ),
  column(2,
         offset=1,
         div(DT::dataTableOutput('definedExp'),
             style = "font-size: 95%; width: 85%")
  )
)