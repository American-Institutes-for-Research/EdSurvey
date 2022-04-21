
#outputs the verbose stats for the passed details to print to console for user
#for the first call set the 'outputHeader' = TRUE to include the column names
printMergeStats <- function(dataGroup, step, fileDesc, rows, cols, mergeType, matchedRecords, okFlag = TRUE, outputHeader = FALSE){
  
  c1 <- 16
  c2 <- 40
  c3 <- 12
  c4 <- 12
  c5 <- 12
  c6 <- 28
  c7 <- 4
  
  #generally only output the header on the first call
  if(outputHeader){
    header <- paste(
      c(paste0("|", format("DataGroup", justify = "centre", width=c1)),
        paste0("|", format("FileDesc", justify = "centre", width=c2)), #needs additional size
        paste0("|", format("Rows", justify = "centre", width=c3)),
        paste0("|", format("Columns", justify = "centre", width=c4)),
        paste0("|", format("MergeType", justify = "centre", width=c5)),
        paste0("|", format("MatchedRecords", justify = "centre", width=c6)),
        paste0("|", format("OK", justify = "centre", width=c7)),
        "|"),
      collapse = "", sep = "")
    header <- gsub(" ", "-", header, fixed = TRUE)
    cat(paste0(header, "\n"))
  }
  
  #the chk will be a visual indicator if everything looks OK (checkmark) or an issue (X symbol)
  chk <- as.character(ifelse(okFlag, "\U02713", "\U274C")) #\U02713 is checkmark; \U274C is X
  
  txt <- paste(
    c(paste0("|", format(dataGroup, justify = "left", width=c1)),
      paste0("|", format(fileDesc, justify = "left", width=c2)),
      paste0("|", format(rows, justify = "left", width=c3)),
      paste0("|", format(cols, justify = "left", width=c4)),
      paste0("|", format(mergeType, justify = "left", width=c5)),
      paste0("|", format(matchedRecords, justify = "left", width=c6)),
      paste0("|", encodeString(chk, justify = "centre", width=c7)),
      "|"),
    collapse = "", sep = "")
  
  cat(paste0(txt, "\n"))
  return(invisible(NULL))
}

#outputs the verbose data level and merge stats for an edsurvey.data.frame object
#this loops through each dataListItem of a fully structured edsurvey.data.frame and outputs each data level and their details
printMergeStats_ESDF <- function(esdf){
  
  if(!inherits(esdf, "edsurvey.data.frame")){
    stop("printMergeStats_ESDF requires an edsurvey.data.frame object.")
  }
  
  esdf <- openLaFConnections(esdf)
  on.exit(closeLaFConnections(esdf))
  colDropRegex <- "([.]junk$|^merge.type$)"
  
  c1 <- 16
  c2 <- 12
  c3 <- 12
  c4 <- 16
  c5 <- 28
  c6 <- 4
  
  cat(paste0("edsurvey.data.frame data level detail:", "\n"))
  
  header <- paste(
    c(paste0("|", format("DataLevel", justify = "centre", width=c1)),
      paste0("|", format("Rows", justify = "centre", width=c2)),
      paste0("|", format("Columns", justify = "centre", width=c3)),
      paste0("|", format("MergeType", justify = "centre", width=c4)),
      paste0("|", format("MatchedRecords", justify = "centre", width=c5)),
      paste0("|", format("OK", justify = "centre", width=c6)),
      "|"),
    collapse = "", sep = "")
  header <- gsub(" ", "-", header, fixed = TRUE)
  cat(paste0(header, "\n"))
  
  #loop through all dataListObjects as part of the edsurvey.data.frame
  for(i in seq_along(esdf$dataList)){
    dl <- esdf$dataList[[i]]
    
    if(i == 1){
      #the label name
      lvlLbl <- dl$levelLabel
      
      #get out variables we want to inspect
      vars <- dl$fileFormat$variableName
      vars <- vars[!vars %in% dl$ignoreVars] #drop out the ignored variable
      
      if(length(esdf$dataList)>1){
        allXvar <- getMergeVars(esdf, dl$levelLabel)
        xData <- dl$lafObject[ , allXvar]
      }
      
      #fill output variables
      nr <- dl$nrow
      nc <- length(vars)
      mt <- ""
      mr <- "*base level*"
      chk <- (nr > 0) #set the 'check' flag for visual output indicator
    }else{ #additional levels past 1
      lvlLbl <- paste0(">", dl$levelLabel)
      
      mVar.y <- dl$mergeVars
      mVar.x <- allXvar[allXvar %in% dl$parentMergeVars]
      yData <- dl$lafObject[ , mVar.y] #get full data
      
      mRes <- mergev(xData, yData, by.x = mVar.x, by.y = mVar.y, all.x = TRUE, all.y = FALSE, 
                     suffixes = c("", ".junk"), return.list = TRUE, verbose = FALSE)
      res <- mRes$data
      res <- res[ , names(res)[!grepl(colDropRegex,names(res), ignore.case = TRUE)], drop=FALSE] #drop duplicated columns, ensure it's still a data.frame (drop=FALSE)
      
      vars <- dl$fileFormat$variableName
      vars <- vars[!vars %in% dl$ignoreVars]
      
      nr <- nrow(res)
      nc <- length(vars)
      
      mt <- paste(mRes$list$merge.type, sep = "", collapse = "")
      chk <- (mRes$list$merge.type.table[3] > 0) #set the 'check' flag for visual output indicator
      mr <- paste0(mRes$list$merge.type.table[3], " of ", nrow(res))
      
      xData <- res
    }
    
    #the chk will be a visual indicator if everything looks OK (checkmark) or an issue (X symbol)
    chk <- as.character(ifelse(chk, "\U02713", "\U274C")) #\U02713 is checkmark; \U274C is X
    
    txt <- paste(
      c(paste0("|", format(lvlLbl, justify = "left", width=c1)),
        paste0("|", format(nr, justify = "left", width=c2)),
        paste0("|", format(nc, justify = "left", width=c3)),
        paste0("|", format(mt, justify = "left", width=c4)),
        paste0("|", format(mr, justify = "left", width=c5)),
        paste0("|", encodeString(chk, justify = "centre", width=c6)),
        "|"),
      collapse = "", sep = "")
    cat(paste0(txt, "\n"))
  }
  
  cat("\n")
  return(invisible(NULL))
}

#scan the edsurvey.data.frame data levels and gather all the merge variables required for a specific data level
getMergeVars <- function(esdf, levelName){
  vars <- c()
  
  for(dl in esdf$dataList){
    vars <- unique(c(vars, dl$parentMergeVars[dl$parentMergeLevels==levelName]))
  }
  
  return(vars)
}