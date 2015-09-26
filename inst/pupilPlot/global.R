#if(!"shiny" %in% rownames(installed.packages())) {install.packages("shiny")};library(shiny);runUrl("https://dl.dropboxusercontent.com/u/39406134/1plus2_n5.zip",filetype=".zip")
# if(!"data.table" %in% rownames(installed.packages())) {install.packages("data.table")}
library(data.table)

#excludeFromSelectors = c("SubjectNr")
#load("datastable.Rda") #save(alldatas, file="datastable.Rda")
#alldatas = data.table(alldatas)

# sources = list(
#   "Baseline Corrected" = "size", 
#   "Left Pupil Raw" = "PupilSize",
#   "Right Pupil Raw" = "PupilSizeR"
#   )


#Comment this out to generate lines automatically from "TrialPosition" tags
# verticalLines = data.table(
#   "TrialPosition"=c("Baseline","Stim 1","Stim 2"),
#   "TrialTime"=c(0,700,3700)
#   )

selectors = apply(data.frame(variableNames[!variableNames %in% excludeFromSelectors]),1,FUN=function(x) {
  #paste(x, unique(alldatas[!is.na(alldatas[,x]),x]), sep=" = ")
  paste(x,data.frame(na.omit(unique(alldatas[,x,with = F])))[,1], sep=" = ")
  
})
selectors = unlist(selectors)

subjects = unique(na.omit(alldatas[[subjectVariable]]))


setkeyv(x=alldatas, cols=c(subjectVariable,trialVariable))


samples = (trialDuration/1000)*samplingFrequency

defaultLineColors = c("red","darkgreen","orange","blue","yellow","brown","purple","pink",colors()[20:50])

defaultLineColors = apply(col2rgb(defaultLineColors), FUN=function(x) {paste("#",paste(as.character(as.hexmode(x)),collapse =""),sep="")},MARGIN=c(2))

plotSettings = list(
  "lineWidth" = 2,
  "xyLabelSize" = 1.5,
  "legendLabelSize" = 0.8,
  "lineColors"= defaultLineColors,
  "xyTicksLabelSize" = 1
)

