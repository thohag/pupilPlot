source("PupilServerTools.R")

shinyServer(function(input, output, session) {
  
  output$selection <- renderPrint(
    input$mychooser1
  )
  
  output$distPlot <- renderPlot({
    
    #Make plot rendering sensitive to button push by including this
    input$inputId
    
    #Display a progressbar
    withProgress(session=session, min = 0, max = 100, {
      
      #Setup the progressbar with a message
      setProgress(message="Generating Graph", detail = "Preparing...")
      
      stri = ""
      title = ""
      criterias = data.frame()
      
      #Get some variable that we need, isolated so we do not create unwanted conditional updates
      datasource = isolate({input$datasource})
      
      lines = isolate({input$mychooser2})
      lines = unlist(lines$right)
      
      excludedSubjects = isolate({input$mychooser3})
      excludedSubjects = excludedSubjects$left
      
      numberOfSubjects = isolate({length(input$mychooser3$right)})
      
      withErrorBars = isolate({input$errorbars})
      
      exportsubjectmeans = isolate({input$exportsubjectmeans})
      
      excluded = isolate({input$mychooser1})
      excluded = excluded$right
      
      if (length(excluded) > 0) {
        excluded = unlist(excluded)
        
        res = createSelectorString(excluded)
        stri = res[[1]]
        title = res[[2]]
        criterias = res[[3]]
        if (length(excludedSubjects) > 0) {
          
          excludedSubjects = paste(subjectVariable,data.frame(excludedSubjects)[,1], sep=" = ")
          
          
          res = createSelectorString(excludedSubjects,exclude=TRUE)
          stri2 = res[[1]]
          
          stri = paste(stri,stri2,sep=" & ")
        }
        
        title = paste(title," n=",numberOfSubjects,sep="")
        
        
        
        cat(stri,lines,"\n")
        
        #Line --------------------------------------------------------------------------------
        #lines = c("Cue","Congruency")
        trialPositions = NULL
        datas = list()
        se = list()
        submeans = list()
        max = -999
        min = 999
        legends = c("")
        farger = color
        
        setProgress(detail="Reducing data...", value = 0)
        if (length(lines) > 0) {
          lineTypes = list()
          #browser()
          for (i in 1:length(lines)) {
            #lineTypes[i] = list(as.character(unique(alldatas[!is.na(alldatas[,lines[i]]),lines[i]])))
            lineTypes[i] = list(as.character(data.frame(na.omit(unique(alldatas[,lines[i],with = F])))[,1]))
          }
          combos = expand.grid(lineTypes, stringsAsFactors = FALSE)
          
          
          
          for(i in 1:nrow(combos)) {
            selection = stri
            legends[i] = ""
            skip = FALSE
            for (k in 1:length(combos)) {
              #selection = paste(selection," & ","alldatas$",lines[k]," == '",combos[i,k],"'",sep="")
              #(nrow(criterias[criterias$Var == lines[k] & criterias$Val == combos[i,k],]) > 0)
              if (nrow(criterias[criterias$Var == lines[k],]) > 0) {
                if (nrow(criterias[criterias$Var == lines[k] & criterias$Val == combos[i,k],])) {
                  
                } else {
                  skip = TRUE
                  next
                }
              }
              selection = paste(selection," & ",lines[k]," == '",combos[i,k],"'",sep="")
              legends[i] = paste(legends[i],iconv(combos[i,k],"UTF-8"))
            }
            if(skip) {
              datas[i] = NA
              legends[i] = NA
              #farger[i] = NA
              next
            }
            cat(selection,"\n")
            
            
            datas[i] = tryCatch({
              
              list(data.frame(alldatas[eval(parse(text=selection)), eval(parse(text=paste("list(size = mean(",datasource,", na.rm=T))",sep=""))), by = c(trialTimeVariable)]))
            },
            error=function(cond) {NA}, warning={},finally={})
            
            #if (is.na(datas[i])) {
            if (is.na(datas[i]) | nrow(datas[[i]]) == 0) {
              
              datas[i] = NA
              legends[i] = NA
              #farger[i] = NA
              
            } else {
              if (!(exists("verticalLines")) & "TrialPosition" %in% names(alldatas)) trialPositions = rbind(trialPositions,getTrialPositions(alldatas,selection))
              if (nrow(datas[[i]] > samples)) datas[[i]] = datas[[i]][1:samples,]
              if (max(datas[[i]][,2],na.rm=T) > max) max = max(datas[[i]][,2],na.rm=T)
              if (min(datas[[i]][,2],na.rm=T) < min) min = min(datas[[i]][,2],na.rm=T)
              
              if (exportsubjectmeans) {
                sub = alldatas[eval(parse(text=selection)), eval(parse(text=paste("list(size = mean(",datasource,", na.rm=T))",sep=""))), by = c(subjectVariable, trialTimeVariable)]
                submeans[i] = list(sub[get(trialTimeVariable) >= subjectMeansInterval[1] & get(trialTimeVariable) <= subjectMeansInterval[2],list(size = mean(size, na.rm=T)), by = c(subjectVariable)])
                sub = NULL
              }
              
              if (withErrorBars) {
                test1 = alldatas[eval(parse(text=selection)), eval(parse(text=paste("list(size = mean(",datasource,", na.rm=T))",sep=""))), by = c(subjectVariable, trialTimeVariable)]
                se[i] = list(data.frame(test1[,list(se = sd(size,na.rm=T)/sqrt(.N)),by=c(trialTimeVariable)]))
                if (nrow(se[[i]] > samples)) se[[i]] = se[[i]][1:samples,]
                test1 = NULL
              }
            }
            setProgress(detail="Reducing data...", value = (90/nrow(combos))*i)
          }
        } else {
          #browser()
          
          if (withErrorBars) {
            test1 = alldatas[eval(parse(text=stri)), eval(parse(text=paste("list(size = mean(",datasource,", na.rm=T))",sep=""))), by = c(subjectVariable, trialTimeVariable)]
            se[1] = list(data.frame(test1[,list(se = sd(size,na.rm=T)/sqrt(.N)),by=c(trialTimeVariable)]))
            if (nrow(se[[1]] > samples)) se[[1]] = se[[1]][1:samples,]
            test1 = NULL
          }
          datas[1] = list(data.frame(alldatas[eval(parse(text=stri)), eval(parse(text=paste("list(size = mean(",datasource,", na.rm=T))",sep=""))), by = c(trialTimeVariable)]))
          if (nrow(datas[[1]] > samples)) datas[[1]] = datas[[1]][1:samples,]
          min = min(datas[[1]][,2],na.rm=T)
          max = max(datas[[1]][,2],na.rm=T)
          if (!(exists("verticalLines")) & "TrialPosition" %in% names(alldatas)) trialPositions = rbind(trialPositions,getTrialPositions(alldatas,stri))
        }
        
        #------------------------------------------------------------------------------------
        
        #browser()
        firstNonNA = which(!is.na(legends))[1]
        legends = legends[!is.na(legends)]
        farger = farger[!is.na(farger)]
        minimum = min#min(min(datas[[1]]$size))
        maximum = max#max(max(datas[[1]]$size)) 
        
        p = plot(datas[[firstNonNA]],type="n",xlab="Time (ms)",ylab="% Change from Baseline",ylim=c(minimum,maximum),main=title)
        legend(x="topleft",col=farger[1:length(legends)],pt.bg=farger[1:length(legends)],pch=c(22,22),legend=legends,pt.cex=2,box.col="darkgrey",bg="white",cex=.8)
        
        fargeCounter = 1
        
        submeansout = data.frame()
        
        for (i in 1:length(datas)) {
          if (is.na(datas[i])) next
          if (withErrorBars) {
            line_errorbar(datas[[i]][,1],datas[[i]][,2],se[[i]][,2],farger[fargeCounter])
          } else {
            lines(datas[[i]][,1],datas[[i]][,2],lwd=2,col=farger[fargeCounter])
          }
          fargeCounter = fargeCounter + 1
          
          pos = fargeCounter
          if (exportsubjectmeans) {
            test1 = submeans[[i]]
            names(test1)[names(test1) == "size"] = trim.leading(legends[i])
            #cat("i is",i,"pos is",pos,"legends is",legends,"with length",length(legends),"current legend with i is",legends[i])
            if (nrow(submeansout) == 0) {
              submeansout = test1
              #cat("nrow of submeans is",nrow(submeansout),"and names is",names(test1))
            } else {
              submeansout = merge(submeansout,test1,by=subjectVariable)
            }
            
          }
        }
        if (exportsubjectmeans) {
          write.csv(submeansout,file=paste(outputdir,"subjectmeans.txt",sep=.Platform$file.sep))
        }
        if (exists("verticalLines")) trialPositions = verticalLines
        if (!is.null(trialPositions) && nrow(trialPositions) > 0) {
          trialPositions = trialPositions[,eval(parse(text=paste("list(",trialTimeVariable,"=mean(",trialTimeVariable,"))",sep=""))),by=TrialPosition]
          
          for(i in trialPositions$TrialPosition) {
            xpos = as.numeric(trialPositions[TrialPosition==i,][,trialTimeVariable,with=F][[1]])
            abline(h = 0, v = xpos, col = "gray60")
            text(xpos,0, i, col = "gray60", adj = c(0.0, -0.1), srt=90)
          }
        }
        
        
        
        setProgress(value = 100)
        
        
        p
      } else {
        NULL
      }
      
    })#progress
  })#renderplot
  
})