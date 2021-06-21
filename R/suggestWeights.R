#' @title Weight suggestions for ECLS-K:2011 data
#' 
#' @description Suggest Weights for ECLS-K:2011 data based on inputting variables.
#' 
#' @param varnames character vector indicating variables to be included in the weight suggestion. 
#' @param data an \code{edsurvey.data.frame}, a \code{light.edsurvey.data.frame},
#'             or an \code{edsurvey.data.frame.list}
#' @param showAllWeightSuggestions a logical value. When set to \code{TRUE}, all applicable weights that covers 
#'                                 more components, which typically are more conservative with 
#'                                 smaller sample size, will be returned. By default (i.e., \code{FALSE}), only the most approperate 
#'                                 weight is displayed. 
#' @param verbose a logical value to either print or suppress status message output.
#' 
#' @details \code{suggestWeights} provides one additional way to assist researchers in deciding which weight to use for analyses.
#'          This function find the intersect of possible weights given variables provided, and rank this intersect
#'          based on the number of components a weight can adjust. 
#'          
#'          The best weight would adjust for each and every source used and only those sources. 
#'          However, for many analyses, there will be no weight that adjusts for nonresponse 
#'          to all the sources of data that are included and for only those source. 
#'          When no weight corresponds exactly to the combination of components included in the desired analysis, 
#'          researchers might prefer to use a weight that includes nonresponse adjustments for more components 
#'          than they are using in their analysis if that weight also includes 
#'          nonresponse adjustments for the components they are using.
#'          
#'          Researchers should always consult their research questions for optimal weight choice.
#' 
#' @example man/examples/suggestWeights.R                  
#' 
#' @return A list of weight variables. The first one is the most approperate choice.
#' 
#' @author Huade Huo
#' @references
#' Tourangeau, K., Nord, C., Le, T., Sorongon, A.G., Hagedorn, M.C., Daly, P., and Najarian, M. (2015). \emph{Early Childhood Longitudinal Study, Kindergarten Class of 2010-11 (ECLS-K:2011), User's Manual for the ECLS-K:2011 Kindergarten Data File and Electronic Codebook, Public Version} (NCES 2015-074). U.S. Department of Education. Washington, DC: National Center for Education Statistics. 
#' 
#' Tourangeau, K., Nord, C., Le, T., Wallner-Allen, K., Hagedorn, M.C., Leggitt, J., and Najarian, M. (2015). \emph{Early Childhood Longitudinal Study, Kindergarten Class of 2010-11 (ECLS-K:2011), User's Manual for the ECLS-K:2011 Kindergarten-First Grade Data File and Electronic Codebook, Public Version} (NCES 2015-078). U.S. Department of Education. Washington, DC: National Center for Education Statistics.
#' 
#' Tourangeau, K., Nord, C., Le, T., Wallner-Allen, K., Vaden-Kiernan, N., Blaker, L. and Najarian, M. (2017). \emph{Early Childhood Longitudinal Study, Kindergarten Class of 2010-11 (ECLS-K:2011) User's Manual for the ECLS-K:2011 Kindergarten-Second Grade Data File and Electronic Codebook, Public Version} (NCES 2017-285). U.S. Department of Education. Washington, DC: National Center for Education Statistics.
#' 
#' Tourangeau, K., Nord, C., Le, T., Wallner-Allen, K., Vaden-Kiernan, N., Blaker, L. and Najarian, M. (2018). \emph{Early Childhood Longitudinal Study, Kindergarten Class of 2010-11 ( ECLS -K:2011) User's Manual for the ECLS-K:2011 Kindergarten-Third G rade Data File and Electronic Codebook, Public Version} (NCES 2018-034). U.S. Department of Education. Washington, DC: National Center for Education S tatistics
#' 
#' Tourangeau, K., Nord, C., Le, T., Wallner-Allen, K., Vaden-Kiernan, N., Blaker, L. and Najarian, M. (2018). \emph{Early Childhood Longitudinal Study, Kindergarten Class of 2010-11 (ECLS-K:2011) User's Manual for the ECLS-K:2011 Kindergarten-Fourth Grade Data File and Electronic Codebook, Public Version} (NCES 2018-032). U.S. Department of Education. Washington, DC: National Center for Education Statistics.
#' 
#' @export

suggestWeights <- function(varnames=NULL, 
                           data,
                           showAllWeightSuggestions=FALSE,
                           verbose=FALSE) {
  # Check incoming varnames and data
  varnames <- tolower(varnames)
  checkDataClass(data, "edsurvey.data.frame")
  checkVars <- lapply(varnames, searchSDF, data = data)
  varnames <- varnames[!unlist(lapply(checkVars, is.null))]
  
  if (data$survey == "ECLS_K2011") {
    # Load weights information in "inst" folder
    eclsk2011variables <- readRDS(system.file("suggestWeights", 
                                              "eclsk2011variables.rds", 
                                              package = "EdSurvey"))
    eclsk2011variables$varnames <- ""
    eclsk2011weights <- readRDS(system.file("suggestWeights", 
                                            "eclsk2011weights.rds", 
                                            package = "EdSurvey"))
    # Code for Weight suggestion
    
    # Merge varname into variable/weight spec
    possibleWeightsDF <- NULL #Data file for internal weight suggestion
    variableMatchedOutputDF <- NULL #Data file for printed output
    ignoredVariables <- NULL
    for (var in varnames) {
      positionInEclsk2011variables <- match(c(TRUE), 
                                            unlist(lapply(eclsk2011variables$regex, 
                                                          grepl, var, ignore.case = TRUE)))
      if (is.na(positionInEclsk2011variables)) {
        ignoredVariables <- c(ignoredVariables, var)
        next
      }
      if (eclsk2011variables[positionInEclsk2011variables, "varnames"] == "") {
        eclsk2011variables[positionInEclsk2011variables, "varnames"] <- var
      } else {
        eclsk2011variables[positionInEclsk2011variables, "varnames"] <- paste(
          eclsk2011variables$varnames[positionInEclsk2011variables],
          var, sep = ", ", collapse = ", ")
      }
      # Composite variables
      if (grepl("^x\\d+", var, ignore.case = TRUE)) {
        compositeInfo <- eclsk11CompositeHelper(varname = var, data = data, varSpec = eclsk2011variables)
        compositeType <- compositeInfo$compositeType
        compositeRound <- compositeInfo$compositeRound
        compositeMap <- subset(eclsk2011variables, (component %in% compositeType) & (round %in% compositeRound))
        if (compositeType != "ignore" & nrow(compositeMap) > 0) {
          possibleWeightsDF <- rbind(possibleWeightsDF, compositeMap)
        } else {
          if (! var %in% ignoredVariables) ignoredVariables <- c(ignoredVariables, var)
        }
      } else {
        possibleWeightsDF <- rbind(possibleWeightsDF, 
                                   eclsk2011variables[positionInEclsk2011variables, ])
      }
    }
    variableMatchedOutputDF <- eclsk2011variables[which(eclsk2011variables$varnames != ""), c("varnames", "source")]
    
    possibleWeights <- NA
    if (is.null(possibleWeightsDF)) {
      stop(paste0(sQuote("suggestWeights"), 
                  " cannot determine weights for variable(s) ", 
                  sQuote(paste(varnames, collapse = ", "))))
    }
    
    # Intersect possible weights
    row.names(possibleWeightsDF) <- NULL
    for (i in 1:nrow(possibleWeightsDF)) {
      # Error if any weight variable is already in the list
      if (possibleWeightsDF$possibleWeights[i] == "error") {
        stop(paste0(sQuote("suggestWeights"), 
                    " cannot determine weights for variable ", 
                    sQuote(possibleWeightsDF$varnames[i])))
      } else if (possibleWeightsDF$possibleWeights[i] == "ignore") {
        if (! possibleWeightsDF$varnames[i] %in% ignoredVariables) {
          ignoredVariables <- c(ignoredVariables, possibleWeightsDF$varnames[i])
        }
        next
      } else {
        possibleWeightsI <- tolower(gsub("\\s+", "", unlist(strsplit(possibleWeightsDF$possibleWeights[i], ","))))
        if (all(is.na(possibleWeights))) {
          possibleWeights <- possibleWeightsI
        } else {
          possibleWeights <- intersect(possibleWeights, possibleWeightsI)
          if (length(possibleWeights) == 0) {
            stop(paste0("No weight suggestions for the following variables: ", 
                        sQuote(paste0(varnames, collapse = ", "))))
          }
        }
      }
    }
  } else {
    stop(paste0(sQuote("suggestWeights"), 
                " does not currently support ", 
                sQuote(data$survey), " data."))
  }
  if (showAllWeightSuggestions) {
    orderedPossibleWeightsWithDescription <- eclsk2011weights[which(eclsk2011weights$weight %in% possibleWeights), ][c('weight', 'cases', 'description')]
  }else {
    orderedPossibleWeightsWithDescription <- head(eclsk2011weights[which(eclsk2011weights$weight %in% possibleWeights), ],
                                                  n = 1)[c('weight', 'cases', 'description')]
  }
  
  orderedPossibleWeights <- orderedPossibleWeightsWithDescription$weight
  row.names(orderedPossibleWeights) <- NULL
  
  # Print results
  warning("Weight suggestion is experimental. Please consult the data file documentation or data file user's manual for complete descriptions of the analytic weights and their adjustments.")
  
  if (length(orderedPossibleWeights) == 0) {
    stop(paste0(sQuote("suggestWeights"), 
                " cannot determine weights for variables provided."))
  } 
  if (verbose) {
    if (length(orderedPossibleWeights) == 1) {
      cat("The following weight is suggested: \n")
    } else {
      cat("The following weights are suggested: \n")
    }
    
    for (i in 1:nrow(orderedPossibleWeightsWithDescription)) {
      cat(paste0("Weight: ", orderedPossibleWeightsWithDescription[i, "weight"],"\n"))
      cat(paste0("Number of cases: ", 
                 orderedPossibleWeightsWithDescription[i, "cases"],"\n"))
      weightDescriptionWrap <- gsub('(.{1,80})(\\s|$)', '\\1\n', 
                                    orderedPossibleWeightsWithDescription[i, "description"])
      cat(paste0("Description: ", weightDescriptionWrap,"\n"))
      cat("\n")
    }
    cat("Weight selection based on the inclusion of the following variables:\n")
    variablesAdjusted <- variableMatchedOutputDF[which(!variableMatchedOutputDF$varnames %in% ignoredVariables), ]
    row.names(variablesAdjusted) <- NULL
    print(variablesAdjusted)
    if (length(ignoredVariables) > 0) {
      cat("The following weights were not accounted for in the weight selection process because they do not correspond to a specific data collection component or round:\n")
      variablesNotAdjusted <- variableMatchedOutputDF[which(variableMatchedOutputDF$varnames %in% ignoredVariables), ]
      row.names(variablesNotAdjusted) <- NULL
      print(variablesNotAdjusted)
    }
  }
  return(orderedPossibleWeights)
}

eclsk11CompositeHelper <- function(varname, data, varSpec) {
  # Get label of composite variable
  compositeVars <- searchSDF(varname, data)
  compositeVarLabel <- compositeVars[which(compositeVars$variableName == varname),]$Labels
  
  # Get type from label
  if (grepl("teacher report", compositeVarLabel, ignore.case = TRUE)) {
    compositeType <- "Teacher"
  } else if (grepl("parent report|prnt rpt", compositeVarLabel, ignore.case = TRUE)) {
    compositeType <- "Parent"
  } else if (grepl("household|parent|food security|language|income|poverty", compositeVarLabel, ignore.case = TRUE)) {
    compositeType <- "Parent"
  } else if (grepl("reading|math|science|assess", compositeVarLabel, ignore.case = TRUE)) {
    compositeType <- "Child"
  } else if (grepl("irt |theta|score|accuracy|flanker", compositeVarLabel, ignore.case = TRUE)) {
    compositeType <- "Child"
  } else if (grepl(" ses ", compositeVarLabel, ignore.case = TRUE)) {
    compositeType <- "Parent"
  } else if (grepl("school", compositeVarLabel, ignore.case = TRUE)) {
    compositeType <- "Administrator"
  } else {
    # TODO: We need ECLSK team confirms this.
    compositeType <- "ignore"
  }
  
  # Get round from composite variable
  compositeRound <- as.numeric(regmatches(varname, regexpr("[0-9]+", varname)))
  if (compositeRound > 10) {
    compositeRound <- c(compositeRound %/% 10, compositeRound %% 10)
  }
  
  return(list(compositeType = compositeType, 
         compositeRound = compositeRound))
}