source("chooser2.R")
#source("progress.R")

shinyUI(fluidPage(
  #progressInit(),
  chooserInput("mychooser1","mychooser2","mychooser3", "Available frobs", "Selected frobs",
               selectors, c(), variableNames, c(), c(), subjects, size = 10, multiple = TRUE
  )
  #,verbatimTextOutput("selection")
))

