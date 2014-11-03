
chooserInput <- function(inputId1, inputId2, inputId3, leftLabel, rightLabel, leftChoices, rightChoices, leftChoices2, rightChoices2, leftChoices3, rightChoices3, size = 5, multiple = FALSE) {
  
  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)
  
  leftChoices2 <- lapply(leftChoices2, tags$option)
  rightChoices2 <- lapply(rightChoices2, tags$option)
  
  leftChoices3 <- lapply(leftChoices3, tags$option)
  rightChoices3 <- lapply(rightChoices3, tags$option)
  
  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL
  
  tagList(
    singleton(tags$head(
      tags$script(src="chooser-binding.js"),
      tags$style(type="text/css",
        HTML(".chooser-container { display: inline-block;}")
      ),
      tags$style(type="text/css",
                 HTML(".chooser { display: inline-block; border: black; border-style: solid; border-width: 2px; padding: 4px; }")
      ),
      tags$style(type="text/css",
                 HTML(".btn { margin-top: 4px; float: left; }")
      ),
      tags$style(type="text/css",
                 HTML(".filtertitle { text-align: center; }")
      ),
      tags$style(type="text/css",
                 HTML(".subjectsList { width: 100px; }")
      ),tags$style(type="text/css",
                   HTML("#datasource { display: inline-block; border: black; border-style: solid; border-width: 2px; padding: 4px; }")
      ),tags$style(type="text/css",
                   HTML(".checkbox { display: inline-block; margin-left: 15px; margin-top: 9px; }")
      )
    )),
    div(id=inputId3, class="chooser",div(width="100%", class="filtertitle", "SUBJECTS"),tags$br(),
        div(class="chooser-container chooser-left-container",div("Excluded Subjects"),
            tags$select(class="left subjectsList", size=size, multiple=multiple, leftChoices3)
        ),
        div(class="chooser-container chooser-center-container",
            icon("arrow-circle-o-right", "right-arrow fa-3x"),
            tags$br(),
            icon("arrow-circle-o-left", "left-arrow fa-3x")
        ),
        div(class="chooser-container chooser-right-container",div("Included Subjects"),
            tags$select(class="right subjectsList", size=size, multiple=multiple, rightChoices3)
        )
    ),
    div(id=inputId1, class="chooser",div(width="100%", class="filtertitle", "SELECTORS"),tags$br(),
      div(class="chooser-container chooser-left-container",div("Available Selectors"),
        tags$select(class="left", size=size, multiple=multiple, leftChoices)
      ),
      div(class="chooser-container chooser-center-container",
        icon("arrow-circle-o-right", "right-arrow fa-3x"),
        tags$br(),
        icon("arrow-circle-o-left", "left-arrow fa-3x")
      ),
      div(class="chooser-container chooser-right-container",div("Selected Selectors"),
        tags$select(class="right", size=size, multiple=multiple, rightChoices)
      )
    ),
    div(id=inputId2, class="chooser",div(width="100%", class="filtertitle", "LINES"),tags$br(),
        div(class="chooser-container chooser-left-container",div("Available Lines"),
            tags$select(class="left", size=size, multiple=multiple, leftChoices2)
        ),
        div(class="chooser-container chooser-center-container",
            icon("arrow-circle-o-right", "right-arrow fa-3x"),
            tags$br(),
            icon("arrow-circle-o-left", "left-arrow fa-3x")
        ),
        div(class="chooser-container chooser-right-container",div("Selected Lines"),
            tags$select(class="right", size=size, multiple=multiple, rightChoices2)
        )
    ),
    radioButtons("datasource", "Data Source:",sources),
    tags$br(),
    
      actionButton("inputId", "Draw Graph", icon = NULL),
      checkboxInput("errorbars", "Error Bars", FALSE),
      checkboxInput("exportsubjectmeans", "Export Subject Means", FALSE)
    ,
    plotOutput("distPlot",height = "600px")
  )
}

registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
  #cat("CHOOSE",unlist(data),"\n")
  #browser() <---- breakpoint method
  if (is.null(data))
    NULL
  else {
    list(left=as.character(data$left), right=as.character(data$right))
  }
}, force = TRUE)
