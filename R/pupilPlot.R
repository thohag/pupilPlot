#' pupilPlot
#' 
#' This function starts a shiny server for plotting pupil data.
#' @param data A full data.table with all relevant variables.
#' @param variableNames Names of variables in the experiment that you would like to plot by.
#' @param excludeFromSelectors Names of variables not to be used in selectors (e.g.: Subject numbers)
#' @param trialTimeVariable Name of variable holding the trial-relative timepoints.
#' @param pupilSizeVariables List of variable desriptions and names
#' @param samplingFrequency How many samples per second.
#' @param trialDuration How long did each trial last.
#' @param subjectVariable Name of variable holding subject numbers.
#' @param trialVariable Name of variable holding trial numbers.
#' @param subjectMeansInterval Interval for exporting subject means.
#' 
#' @examples
#' Start the shiny server.
#' ##not run
#' variables = c("Stim1","Stim2","SubjectNr","Experiment","ACC")
#' pupilPlot(alldatas,variables,trialDuration=7000,subjectVariable="SubjectNr")

pupilPlot = function(data = NULL, 
                     variableNames = c(),
                     excludeFromSelectors = c("SubjectNr"),
                     trialTimeVariable = "TrialTime", 
                     pupilSizeVariables = list("Baseline Corrected" = "size"), 
                     samplingFrequency = 60, 
                     trialDuration = 5000,
                     subjectVariable="SubjectNr",
                     trialVariable="Trial",
                     subjectMeansInterval = c(0,trialDuration)
                     ) {
  
  #Perform checks
  if (is.null(data)) {stop("ERROR: data is not defined")}
  if (!is.data.table(data)) {stop("ERROR: data is not of type data.table")}
  
  #Make the variables global
  alldatas <<- data
  variableNames <<- variableNames
  excludeFromSelectors <<- excludeFromSelectors
  trialTimeVariable <<- trialTimeVariable
  sources <<- pupilSizeVariables
  samplingFrequency <<- samplingFrequency
  trialDuration <<- trialDuration
  subjectVariable <<- subjectVariable
  trialVariable <<- trialVariable
  subjectMeansInterval <<- subjectMeansInterval

  #alternative:
#   .GlobalEnv$.aecay.dataset <- dataset
#   on.exit(rm(.aecay.dataset, envir=.GlobalEnv))
  
  outputdir <<- getwd()
  
  shiny::runApp(system.file('pupilPlot', package='pupilPlot'), launch.browser=T)
}