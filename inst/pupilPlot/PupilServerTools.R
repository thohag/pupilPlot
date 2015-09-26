getTrialPositions = function (alldatas,selector) {
  pos = alldatas[eval(parse(text=selector)), list(TrialTime = min(TrialTime, na.rm=T)), by = c("TrialPosition","Trial")]
  occurances = pos[,list(TrialTime = .N), by = c("TrialPosition")][[2]]
  criteriera = as.numeric(names(table(occurances))[which.max(table(occurances))])
  pos[,list(freq = .N, TrialTime = median(TrialTime)), by = c("TrialPosition")][freq == criteriera]
}

line_errorbar <-function(x,y,y_err,color,lineWidth) {
  lines(x,y,lwd=lineWidth,col=color)
  
  err1 <- y - y_err
  err2 <- y + y_err
  
  lines(x,err1,type="l",col=c(color),lwd=1,lty=2)
  lines(x,err2,type="l",col=c(color),lwd=1,lty=2)
  
  rgb = col2rgb(color)
  polygon(c(x,rev(x)),c(err1,rev(err2)),col=rgb(rgb[1],rgb[2],rgb[3],30,maxColorValue=255),border=NA)
}

undupe = function(ex,target,currentPosition) {
  dupes = c()
  for (x in 1:length(ex)) {
    if (is.na(ex[x])) next
    if (currentPosition != x & substr(ex[x],1,nchar(target)) == target) {
      dupes[length(dupes)+1] = unlist(strsplit(ex[x]," = "))[2]
      ex[x] = NA
    }
  }
  list(dupes,ex)
}

createSelectorString = function(selectors,exclude=FALSE) {
  first = TRUE
  criterias = data.frame(list("Var" = "#X#","Val"="#X#"),stringsAsFactors=F)
  #browser()
  stri = ""
  title = ""
  
  for (i in 1:length(selectors)) {
    if (is.na(selectors[i])) next
    parts = unlist(strsplit(selectors[i]," = "))
    
    typefix = "'"
    numeric = is.numeric(alldatas[1,as.character(parts[1]),with=F][[1]])
    if (numeric) typefix = ""
    
    if (first == FALSE) {stri = paste(stri,"& ")}
    
    duperesult = undupe(selectors,parts[1],i)
    dupes = duperesult[[1]]
    if (length(dupes) > 0) {
      
      selectors = duperesult[[2]]
      dupes[length(dupes)+1] = parts[2]
      invalues = paste("c(",typefix,paste(dupes,collapse=paste(typefix,",",typefix,sep="")),typefix,")",sep="")
      title = iconv(paste(title, " ",parts[1],"=",paste(dupes,collapse=","),sep=""),"UTF-8")
      
      op=""
      if (exclude) op = "!"
      #stri = paste(stri,"alldatas$",parts[1]," %in% ",invalues, sep="")
      stri = paste(stri,op,parts[1]," %in% ",invalues, sep="")
      for (c in 1:length(dupes)) {
        criterias[nrow(criterias)+1,] = c("var" = parts[1], "val" = dupes[c])
      }
    } else {
      title = iconv(paste(title, " ", parts[1],"=",parts[2],sep=""),"UTF-8")
      #stri = paste(stri,"alldatas$",parts[1]," == '",parts[2],"'", sep="")
      op="=="
      if (exclude) op = "!="
      stri = paste(stri,parts[1]," ",op," ",typefix,parts[2],typefix, sep="")
      criterias[nrow(criterias)+1,] = c("var" = parts[1], "val" = parts[2])
    }
    first = FALSE
  }
  list(stri,title,criterias)
}

trim.leading <- function (x)  sub("^\\s+", "", x)