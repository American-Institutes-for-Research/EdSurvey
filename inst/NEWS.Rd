\name{NEWS}
\title{NEWS for EdSurvey}
\section{Changes in version 2.7.1}{
  \subsection{DATA UPDATES}{
    \itemize{
      \item Added support for the National Household Education Survey (NHES) set of data.  See \code{downloadNHES} for instructions to locate and obtain the NHES data files, and see \code{readNHES} for reading the NHES data for analysis.
      \item Added support for the School Survey on Crime and Safety (SSOCS) set of data.  See \code{downloadSSOCS} for instructions to locate and obtain the SSOCS data files, and see \code{readSSOCS} for reading the SSOCS data for analysis.
    }
  }
  \subsection{UPDATES}{
    \itemize{
      \item \code{append.edsurvey.data.frame.list} now uses \code{labA} and \code{labB} in preference to existing labels. It also now accepts label names other than \code{label}. When the resulting labels are not unique, it generates new ones.  Also, if a single \code{edsurvey.data.frame} is added to an \code{edsurvey.data.frame.list}, and the labels for the \code{edsurvey.data.frame.list} appear to be automatically generated, then the labels are automatically generated for the output of the call to \code{append.edsurvey.data.frame.list} as well. These changes should make the output of \code{append.edsurvey.data.frame.list} seem more logical.
      \item \code{showCutPoints} now returns one line per set of cut points and does not show the cut point labels.
      \item updated warnings and errors when an \code{edsurvey.data.frame} is passed to a vareity of functions that requires use of PSU or stratum variables but they are not on the data.
      \item \code{cor.sdf} now allows dynamic variables for the variables being correlated (\code{x} and \code{y})
      \item \code{searchSDF} now generates a tabled output when passed an \code{edsurvey.data.frame.list}.
    }
  }
  \subsection{STATISTICS}{
    \itemize{
      \item \code{SD} used to use the population variance estimator and now uses the sample variance estimator. This also affects standard deviations calculated with the \code{gap} function.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{SD} used to set \code{jkSumMultiplier} to 1 by default and now uses the value from the \code{edusrvey.data.frame}. While this was the documented behavior, it was counterintuitive since all other \code{EdSurvey} functions use the value directly from the \code{edusrvey.data.frame}.
      \item when calling \code{achievementLevels} with both \code{returnDiscrete = FALSE} and \code{returnCumulative = TRUE} it would return an error. It now properly returns.
    }
  }
}
\section{Changes in version 2.7.0}{
  \subsection{DATA UPDATES}{
    \itemize{
      \item Added support for the PISA Young Adult Follow-up Study (YAFS). See \code{readPISA_YAFS} for more information.
      \item \code{readICCS} now woks for 2016 ICCS data.
    }
  }
  \subsection{STATISTICS}{
    \itemize{
      \item supports linking error in NAEP assessments that have combined PBA and DBA formats, including NAEP 2018 Social Studies (grades 8); 2019 Science (grades 4, 8, and 12); 2019 Mathematics and Reading (grades 4, 8, and 12). See the vignette for details on the implementation at \url{https://www.air.org/sites/default/files/2021-07/EdSurvey-linkingerror.pdf}.
      \item \code{mml.sdf} a new function that estimates student scale scores using the marginal maximum likelihood regression estimation method (as the program AM does). This is an alternative method to the plausible values approach.
      \item \code{mixed.sdf} now gives clearer information about groups and weights, similar to the updated \code{WeMix::mix} function.
      \item \code{achievementLevels} now accepts multiple sets of plausible values at once, allowing joint achievement levels to be calculated. Because of this the \code{discrete} and \code{cumulative} results column names now include the name of the variable. So for a composite score it will now be labeled \code{composite_Level} where it used to say \code{Level}. Also the \code{cutpoints} object now specifies the scores the cutpoints regard.
      \item \code{percentile} now has a new method \dQuote{symmetric} that is similar to \dQuote{simple} but returns the negative result if all of the values are multiplied by negative one. This method is faster than \dQuote{unbiased} which has a small bias correction.
      \item \code{percentile} used to report the number of people who were more extreme than any percentile in the \code{nsmall} column. This was not used and so was removed.
      \item \code{percentile} now accepts a \code{dofMethod} argument to allow either of the methods described in \code{DoFCorrection}: \code{WS} for the Welch-Satterthwaite formula or \code{JR} for the Johnson-Rust formula. This defaults to \code{JR} but was previously \code{WS}.
      \item \code{edsurveyTable} now warns and removes rows with non-positive full sample weights or no outcome. The also warnings now include the number of rows removed by the filter. Non-positive and NA weights are removed only if the \code{returnMeans} or \code{returnSepct} are \code{TRUE}. Missing outcome scores are removed only if \code{returnMeans} is \code{TRUE}.
      \item we simplified all the code in the statistical methods, including \code{rq.sdf}, \code{lm.sdf}, and \code{percentile}.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item after subsetting an edsurvey.data.frame, the cache (user defined variables) used to throw errors. This now works.
      \item \code{mixed.sdf} used to not correctly identify covariances when outputting the variance tables. Thanks to Endraswara Wiryawan for identifying this bug.
      \item \code{readTIMSS} previously attempted to merge on 2019 teachers using the student's grade level. This lead to many teacher variables not being merged on in AAD (Abu Dhabi, UAE), ARE (United Arab Emirates), and NZL (New Zealand). Thanks to Damian Campbell for identifying this bug.
      \item fixed \code{readTIMSSAdv} bug for 1995 data relating to storing column dimension information in cache.
    }
  }
  \subsection{OTHER}{
    \itemize{
      \item when \code{EdSurvey} is attached, it nolonger sets the options value of \code{timeout}. Instead it only sets it to the larger of the current value and an hour when downloading files.
    }
  }
}

\section{Changes in version 2.6.9}{
  \subsection{DATA UPDATES}{
    \itemize{
      \item \code{downloadTIMSS} and \code{readTIMSS} were updated to include 2019.
      \item \code{downloadPIAAC} was updated so data/codebook downloads perform more robustly on different OS.
      \item \code{readPIAAC} was updated so the number of replicate weights in the resulting \code{edsurvey.data.frame} is accurate to the OECD source data. Before this number was hard coded, resulting in duplicate replicate weights.
      \item \code{readPISA} added 2018 financial literacy.
      \item \code{downloadICILS} now references instructions for downloading the 2018 ICILS data set, as the \code{readICILS} function supported ICILS 2018 as of EdSurvey v2.4.0
      \item when calling \code{$} on an edsurvey.data.frame with a weight variable, only the vector of full sample weights are returned now. This used to also return a matrix (when relevant) that also included the replicate weights.
      \item \code{downloadPISA} and \code{readPISA} now support the PISA 2018 Financial Literacy (FIN) database.
    }
  }
  \subsection{STATISTICS}{
    \itemize{
      \item \code{lm.sdf}, \code{glm.sdf}, and \code{edsurveyTable} and  now supports surveys where the PSU and stratum variables varried by weight variable (usually longitudinal surveys) for Taylor series variance estimation.
      \item \code{mixed.sdf} now has a \code{coef} and \code{vcov} methods.
      \item \code{rq.sdf} now returns the rho statistic
      \item \code{waldTest} now accommodates JK1 denominator degrees of freedom explicitly and allows for cases where the F-test is not valid to return, but only returns the chi-squared test.
      \item \code{waldTest} now covers \code{rq.sdf} and \code{mixed.sdf} models.
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item the vignette was expanded to include more links.
      \item The \code{==} (equal) and \code{!=} (not equal) comparison operators now work on \code{edsurvey.data.frame} and/or \code{edsurvey.data.frame.list} objects to compare objects by their attributes (e.g., survey, year, country).
    }
  }
  \subsection{TECHNICAL}{
    \itemize{
      \item \code{readNAEP} now allows fr2 paths that are seperate from the usual location and is more robust to variation in capitalization, but requires that a file name be unique, regardless of capitalization.
      \item \code{getPSUvar} and \code{getStratumVar} are now more robust and return more informative warnings.
      \item Added \code{match.arg} parameter matching for the following functions/arguments for usability: \code{downloadPISA (database)}, \code{readCivEDICCS (dataSet, gradeLvl)}, \code{readICILS (dataSet)}, \code{readPISA (database, cognitive)}, \code{readTALIS (isced, dataLevel)}, \code{readTIMSS (gradeLvl)}, \code{readTIMSSAdv (subject)}.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{readPIAAC} was fixed so it can now find cached files for \code{usa17} and \code{usa12_14}. Additionally, \code{match.arg} checks were added to better check user inputted data to the \code{countries} and \code{usaOption} arguments.
      \item The \code{readNAEP} function has updates for the \code{frPath} argument to correctly handle relative paths, path expansion (~), and additional file validation checks.
      \item The \code{gap} function now correctly parses expressions that use a variable in the global environment.
      \item The \code{summary2} function now gives a clearer error when something that is not the name of a weight column is passed to the \code{weightVar} argument.
    }
  }
}
\section{Changes in version 2.6.0}{
  \subsection{DATA UPDATES}{
    \itemize{
      \item ECLS_B is now a supported dataset with the \code{readECLS_B} function.
      \item \code{downloadECLS_K} and \code{readECLS_K2011} now download and read in, respectively, the latest ECLS-K:2011 K-5 data.
      \item \code{readTALIS} was updated to work with TALIS 2018.
      \item \code{downloadPISA} added support for 2018 and now downloads 2018 data. OECD compressed the 2018 PISA with an algorithm not implemented in R (DEFLATE64), and so users are instructed to uncompress it outside of R and EdSurvey before using the data.
      \item \code{readPIAAC} was updated to work with files OECD released in early 2020. This includes adding a \code{usaOption} argument that allows the user to select between the  2012-2014 file and the 2017 file for U.S. data.
      \item \code{readICILS} was updated to work with files IEA released early in 2020.
      \item EdSurvey now supports NAEP assessments with no plausible value variables, such as some National Indian Education Study (NIES) data.
      \item EdSurvey now supports TIMSS 2019 pre-release files, including bridge files.
      \item For \code{readHSLS} and \code{readELS} we bug an issue parsing \code{.sav} variable format definitions relating to integer definitions with a display of more than 10 digits. We believe this does not change how EdSurvey reads in any public use files. The issue can arise if the user converts restricted use files with Stat/Transfer.
      \item Some IEA datasets have convenience variables that EdSurvey incorrectly identified as plausible value variables that are discretized plausible values. EdSurvey already internally generates similar convenience variables based on the existing plausible values and does not use the IEA variables. As of 2.6.0, these columns are not linked as plausible variables and are ignored. Affected datasets include TIMSS, TIMSS Advanced, PIRLS, and ePIRLS.
    }
  }
  \subsection{STATISTICS}{
    \itemize{
      \item \code{cor.sdf} now reports standard errors linearized back to the correlation space in the result value \code{se}. It also reports the confidence interval in the Fisher-Z space as \code{ZconfidenceInterval} and a confidence interval mapped back to the correlation space (without linearization) as \code{confidenceInterval}.
      \item \code{cor.sdf} now allows users to select the Fisher-Z transformation or no transformation with the \code{transformation} argument. The default of using the Fisher Z for Pearson and no transformation for other correlation types remains the same.
      \item \code{cor.sdf} now allows the user to set a value of \code{jrrIMax} to allow faster computation.
      \item Updated \code{oddsRatio} to use t-statistic instead of Z, and to identify the confidence interval in the linear space and then convert.
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item The \code{+} function now works on \code{edsurvey.data.frame} and/or \code{edsurvey.data.frame.list} objects to build an \code{edsurvey.data.frame.list}. See \code{?edsurvey.data.frame.list} for examples.
      \item The \code{\%in\%} function now works on \code{edsurvey.data.frame.list} objects, identifying if a survey is in an \code{edsurvey.data.frame.list}. See \code{?edsurvey.data.frame.list} for examples.
      \item \code{percentile}, \code{achievementLevels}, and \code{edsurveyTable} now warns when an \code{edsurvey.data.frame.list} is passed to it with an \code{edsurvey.data.frame} on it that cannot return valid results for the requested statistics. The output is \code{NA} for these surveys.
      \item \code{gap} now supports linking error in PISA for math, reading, and science when a user sets \code{includeLinkingError} to \code{TRUE}.
      \item \code{contourPlot} deprecated the argument \code{colors} which only set density colors. Instead, the function now uses \code{densityColors} and \code{pointColors}, which set the density and point colors separately. This will cause calls to \code{contourPlot} that use \code{colors} to not function with 2.6.0.
      \item When a \code{summary2} call is made with a \code{light.edsurvey.data.frame}, unfilled levels will not be returned in the table anymore.
      \item Users can now assign a variable with $, including subsets. Variables on the original data cannot be removed but can be overwritten. All changes are for the session only and do not change the data on the disk.
    }
  }
  \subsection{TECHNICAL}{
    \itemize{
      \item \code{getData} now closes LaF connections regardless of how it exits (e.g., with an error). This should improve the stability of the package.
      \item An \code{edsurvey.data.frame} now can specify a \code{fr2path}. This is used only for NAEP surveys and no functionality depends on it, but it may be used in the future. The value of \code{fr2path} may always be set to \code{NA}.
      \item \code{achievementLevels} now warns when an \code{edsurvey.data.frame.list} is passed to it with an \code{edsurvey.data.frame} on it that cannot return valid results for the requested statistics. The output is \code{NA} for these surveys.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{cor.sdf} and \code{percentile} incorrectly calculated standard errors when the pre-multiplier for the replicate weights was not one. Users can see that for a survey using \code{getAttributes(data, "jkSumMultiplier")} for their data.
      \item \code{cor.sdf}  previously failed to calculate some correlations when some records had non-zero full sample weights and zero replicate weights. Now results are still calculated.
      \item With TIMSS data, when a user updated a teacher-level variable, it used to prevent the user from using the \code{edsurvey.data.frame} for non-teacher-level analysis. We fixed this issue. Thanks to Anders Astrup Christensen for identifying this issue and sharing reproducible code that is now incorporated into the package as a test.
    }
  }
}

\section{Changes in version 2.5.0}{
  \subsection{NEW DATA}{
    \itemize{
      \item Added support for the 2007-2008 Beginning Teacher Longitudinal Study (see \code{readBTLS}) for Waves 1 to 5.
    }
  }
  \subsection{DATA UPDATES}{
    \itemize{
      \item \code{readPISA} uses less memory when reading in PISA 2015 data. This is because it now uses the \code{haven} package's \code{read_sav} new \code{skip} and \code{n_max} arguments to read in portions of files. This should save time on read in.
      \item Other PISA files were changed to use base R read functions, this results in a different ordering of columns in 2012 and 2015.
      \item Some PISA 2003 variables were read in as an integer and are now in as a real values, mostly hours variables.
      \item After the OECD files were updated, \code{downloadPIAAC} and \code{readPIAAC} now properly use the 2012 USA file for Cycle 1.
    }
  }
  \subsection{TECHNICAL}{
    \itemize{
      \item To allow both 4.0 and 3.6 compatibility, an \code{edsurvey.data.frame.list} now stores \code{covs} as characters in preference to factors.
      \item \code{EdSurvey} is no longer imports \code{readr}, \code{rvest}, \code{stringi}, \code{stringr}, or \code{xml2}. In addition, \code{RColorBrewer} was imported but is now suggested.
      \item \code{ContourPlot} now uses \code{heat.colors} by default.
      \item Several data files were stored in text format in functions and are now stored in \code{inst/extdata} as \code{rds} files.
      \item The news for 2.4.0 was edited to note that the \code{gap} linking error in NAEP is only for mathematics and reading.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{lm.sdf} now rounds the printed coefficients matrix in the standard way.
      \item Fixed a bug in \code{edsurveyTable} that caused it to return incorrectly labeled output if multiple covariates were tabbed with weights.
      \item A broken link in the vignette was fixed.
    }
  }
}

\section{Changes in version 2.4.0}{
  \subsection{NEW DATA}{
    \itemize{
      \item Added support for the 2009 High School Longitudinal Study (see \code{downloadHSLS} and \code{readHSLS}). These do not support Restricted Use Data (RUD).
      \item Added support for the 2002 Education Longitudinal Study (see \code{downloadELS} and \code{readELS}).
      \item Added the ECLS Kindergarten Class of 1998-1999 Study. Thes datasets can be downloaded with \code{downloadECLS_K} and read in with \code{readECLS_K1998}. This was added in 2.3.0 but first added to the NEWS for 2.4.0.
      \item Added support for ePIRLS (see \code{read_ePIRLS} and \code{download_ePIRLS}). This was added in 2.3.0 but first added to the NEWS for 2.4.0.
   }
  }
  \subsection{DATA UPDATES}{
    \itemize{
      \item Added support for 2018 for the existing \code{readICILS} function. Thanks to Jeppe Bundsgaard of Aarhus University, Danish School of Education, for contributing the code for this.
      \item OECD moved the PIAAC data to a new location, and the \code{downloadPIAAC} function now uses the new URL.
      \item The PISA 2015 data cache could have been formed incorrectly; that is fixed. When PISA 2015 datasets are first used with 2.4 it will take time to re-cache the data. The process now also uses far less memory.
      \item The PISA data had incorrect PSU and stratum variables for most years. They are all fixed except for 2000, which we do not believe has a PSU variable on the file. Several strata have only one PSU, so the design still needs to be edited by the user to get correct Taylor series sampling variance estimates.
    }
  }
  \subsection{UPDATES}{
    \itemize{
      \item The \code{mixed.sdf} function now correctly aggregates results and has its methodology documented. It no longer supports binomial models and has several arguments deprecated as a result.
      \item The \code{gap} function argument \code{varMethod} is deprecated. The function uses only jackknife variance estimation.
      \item The \code{gap} function now accounts for linking error between NAEP paper and digitally based assessments for mathematics and reading.
      \item The \code{subset} function used to fail when a global variable shared a name with a column on the data; it now works.
      \item The \code{percentile} function has been updated to a formula that generates survey percentiles that are robust to transformation. For example, if the values are multiplied by a constant, then the percentiles move by that same constant.
      \item The \code{lm.sdf} function header now prints the number of plausible values used (when they are used) as well as the number of plausible values used in the sampling variance (\code{jrrIMax}).
      \item The \code{levelsSDF} function now makes more informative warnings when passed a \code{light.edsurvey.data.frame}.
      \item The \code{print} function for \code{cor.sdf} now prints a final new line.
      \item When \code{searchSDF} was called with more than one search string and the \code{levels} argument was set to \code{TRUE}, a useless warning was issued. The warning was removed.
      \item More \code{EdSurvey} functions wrap to the width of the console.
    }
  }
}

\section{Changes in version 2.3.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The \code{rq.sdf} function adds quantile regression to the package. See the \code{rq.sdf} documentation for more information.
      \item The functions \code{getStratumVar} and \code{getPSUVar} were added to give users stratum and PSU variables for surveys and Taylor series analyses.
      \item The \code{summary2} function now accepts a vector of variables.
      \item The \code{searchSDF} function now accepts a vector \code{string} input so that results can be filtered.
      \item The formula for degrees of freedom when Taylor series variance estimation is used has been updated. The new formula is derived in the statistics vignette. See \url{https://www.air.org/sites/default/files/EdSurvey-Statistics.pdf}.
    }
  }
  \subsection{NEW DATA}{
    \itemize{
      \item PISA 2015 is now supported.
    }
  }
}

\section{Changes in version 2.2.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The \code{waldTest} function allows the user to test composite hypotheses--hypotheses with multiple coefficients involved--even when the data include plausible values.  Because there is no likelihood test for plausible values nor residuals, the Wald test fills the role of the likelihood ratio test, ANOVA, and F-test.
      \item The \code{mvrlm} function adds multivariate regression (a regression with multiple outcomes) to the package. See the \code{mvrlm} documentation for more information.
      \item Survey weighted mixed models can now be fit with the \code{mixed.sdf} function. Both linear and logistic models can be fit. These models are limited to 2 levels (one level with random effects).
      \item Regressions can now output standardized regression coefficients using \code{summary(myLm, src=TRUE)}. When the \code{lm.sdf} call includes \code{standardizeWithSamplingVar=TRUE} the standardized error of the standardized regression coefficient account for the sampling error and measurement error (when applicable). Otherwise, the standard deviations are assumed to be measured without error.
      \item Added \code{summary2} function to produce unweighted and weighted descriptive statistics of a variable in \code{edsurvey.data.frame} or \code{light.edsurvey.data.frame}
      \item Added $ variable access to edsurvey.data.frame. e.g. sdf$dsex
      \item Updated covariance matrix estimation in the \code{vcov} function for \code{lm.sdf} and \code{glm.sdf} to work when \code{varEstInputs} was not returned
      \item Added covariance matrix estimation for \code{lm.sdf} when Taylor series variance estimation was used.
      \item Added \code{rebindAttributes} function to make dplyr interaction more smooth. See the \code{rebindAttributes} documentation for an example.
      \item When printing an \code{edsurvey.data.frame} it now says the survey, year, subject, and country at the top. The dimensions are moved down.
      \item In \code{gap}, achievement levels can be specified with partial matches. This helps out when achievement levels have long names.
    }
  }
  \subsection{NEW DATA}{
    \itemize{
      \item Added the ECLS Kindergarten Class of 2010-2011 Study. Longitudinal datasets can be downloaded with \code{downloadECLS_K} and read in with \code{readECLS_K2011}.
      \item PIRLS 2016 is now supported.
    }
  }
  \subsection{NEW STATISTICS}{
    \itemize{
      \item Added \code{returnNumberOfPSU} in \code{achievenemtLevels}, \code{percentile}, \code{lm.sdf}, and \code{gap} to report the number of primary sampling units (PSUs) used to calculate a statistic.
      \item Added \code{oddsRatio} helper function for logit results to show odds ratios.
      \item When running a Pearson correlation on a discrete variable, \code{cor.sdf} function by default condenses the occupied response codes to be consecutive integers. This can now be turned off by setting the \code{condenseLevels} argument to \code{FALSE} so that the code book levels are used instead.
    }
  }
  \subsection{UPDATES}{
    \itemize{
      \item The \code{glm.sdf} function now uses the \code{glm2} package to fit models. This package converges on a broader class of models.
      \item The \code{EdSurvey} package nolonger sets the number of threads used by the required \code{data.table} package to one when EdSurvey is being attached. The issue is now fixed in \code{data.table}.
      \item Across the download functions, when a file is missing the warning text was homogenized. Additionally, all download functions now support a \code{verbose} argument that can be used to make downloads silent.
      \item The \code{lm.sdf} and \code{glm.sdf} functions now accept formulas that use the \code{I()} function or other unevaluated expressions to the left of the tilda. Previously only a single variable could be named.
      \item \code{recode.sdf} now checks that each recode has only a \code{to} and \code{from} in it.
      \item The \code{edsurveyTable} function now works without RHS variable, allowing the formula \code{y ~ 1} to return overall means.
      \item The \code{percentile} function used to produce output even if asked to produce a percentile outside of the valid range (0 to 100). Now it prints out message if users input at least percentile outside of the valid range, and stops if all percentiles are invalid.
      \item The \code{getData} function now removes rows with omitted levels after being recoded.
      \item All SPSS (.sav) file reads using the \code{haven} package set the \code{user_na = TRUE} flag to ensure no defined missing/omitted values are automatically converted to \code{NA} values prematurely.
      \item For consistency with other download functions, \code{downloadPISA} now uses a \code{years} argument instead of a \code{year} argument.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Running an \code{edsurveyTable} on an \code{edsurvey.data.frame.list} used to have the potential to create invalid (unprintable) output if the factor levels did not agree on every element of the \code{edsurvey.data.frame.list}. They now return printable output.
      \item An \code{edsurveyTable} could produce a standard error when there was data from only one stratum. It now produces an \code{NA} standard error.
    }
  }
}

\section{Changes in version 2.0.0}{
  \subsection{NEW DATA}{
    \itemize{
      \item Works with the Trends in International Mathematics and Science Study (TIMSS), TIMSS Advanced, the Progress in International Reading Literacy Study (PIRLS),
      and the International Computer and Information Literacy Study (ICILS), International Civic and Citizenship Education Study (ICCS), the Civic Education Study (CivEd),
      the Program for International Student Assessment (PISA), the Program for the International Assessment of Adult Competencies (PIAAC), and the Teaching and Learning International Survey (TALIS).
      \item International datasets can be downloaded with \code{downloadTIMSS}, \code{downloadTIMSSAdv},
            \code{downloadPIRLS}, \code{downloadICILS}, \code{downloadICCS},
      \code{downloadCivEDICCS}, \code{downloadPISA}, \code{downloadPIAAC}, \code{downloadTALIS}.
      \item International datasets can be loaded with \code{readTIMSS}, \code{readTIMSSAdv},
            \code{readPIRLS}, \code{readICILS}, \code{readICCS},
      \code{readCivEDICCS}, \code{readPISA}, \code{readPIAAC}, \code{readTALIS}.
    }
  }
  \subsection{NEW STATISTICS}{
    \itemize{
      \item Added \code{logit.sdf} and \code{probit.sdf} functions with support
            for survey item responses.
      \item Added \code{gap} code{gap} that compares the average, percentile, achievement level, or percentage of survey responses between two groups that potentially share members.
      \item Added \code{percentile} that calculates the percentiles of a numeric variable.
      \item Added \code{showCodebook} that retrieves variable names, variable labels, and value labels for an \code{edsurvey.data.frame}, \code{light.edsurvey.data.frame}, or \code{edsurvey.data.frame.list}.
    }
  }
  \subsection{UPDATES}{
    \itemize{
      \item Redesigned \code{achievementLevels},  \code{edsurveyTable} function for
      significant faster computation with much smaller memory footprint. We also
      made error messages and outputs more informative.
      \item More informative error message and output for \code{cor.sdf},
      \code{levelsSDF}, \code{getPlausibleValue}, \code{print.edsurvey.data.frame},
      \code{searchSDF}, \code{showPlausibleValues}, \code{showWeights}, and \code{getData}.
      \item \code{lm.sdf} and \code{glm.sdf} are now S3 method extended from
      \code{stats::lm} so users can call the function using \code{lm} and \code{glm}.
      \item Added the \code{contourPlot} for regressions diagnostic.
      \item Added the \code{recode.sdf} function for recoding levels within variables.
      \item Added the \code{rename.sdf} function for modifying variable names.
      \item Added the \code{append.edsurvey.data.frame.list} function to return
      a list of sdfs from either an edsurvey.data.frame.list or a single edsurvey.data.frame.
    }
  }
  \subsection{DOCUMENTATION}{
    \itemize{
      \item Manual documentation was refreshed.
      \item Moved vignettes to AIR webiste at https://www.air.org/project/nces-data-r-project-edsurvey or see links in the vignette inlcuded in this package.
      \item Added a new vignette on ``Exploratory Data Analysis on NCES Data'' provides examples of conducting exploratory data analysis on NAEP data.
      \item Added new vignette on ``Calculating Adjusted p-Values From EdSurvey Results'' to the AIR website describing the basics of adjusting p-Values to account for multiple comparisons.
      \item Added new vignette on ``Using EdSurvey to Analyze TIMSS Data'' to the AIR website describing; an introduction to the methods used in analysis of large-scale educational assessment programs such as Trends in International Mathematics and Science Study (TIMSS) using the EdSurvey package. The vignette covers topics such as preparing the R environment for processing, creating summary tables, running linear regression models, and correlating variables.
      \item Added new vignette on ``Using EdSurvey for Trend Analysis'' to the AIR website describing the methods used in the EdSurvey package to conduct analyses of statistics that change over time in large-scale educational studies.
      \item Added new vignette on ``Producing LaTeX Tables From edsurveyTable Results With edsurveyTable2pdf'' to the AIR website detailing the creation of pdf summary tables from summary results using the edsurveyTable2pdf function.
      \item Added new methodology documentation on ``Methods Used for Gap Analysis in EdSurvey'' to the AIR website convering the methods comparing the gap analysis results of the EdSurvey package to the NAEP Data Explorer.
      \item Added new methodology documentation on ``Methods Used for Estimating Percentiles in EdSurvey'' to the AIR website describing the methods used to estimate percentiles.
      \item Added new methodology documentation on ``Weighted and Unweighted Correlation Methods for Large-Scale Educational Assessment: wCorr Formulas'' to the AIR website detailing the methodology used by the wCorr R package for computing the Pearson, Spearman, polyserial, and polychoric correlations, with and without weights applied. See \url{https://www.air.org/resource/report/weighted-and-unweighted-correlation-methods-large-scale-educational-assessment}.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed connection issue associated with closing connections to an \code{LaF}.
    }
  }
}

\section{Changes in version 1.0.6}{
  \itemize{
    \item readNAEP function now works on a case sensitive file system.
    \item Vignettes now should appear in numerical order on CRAN.
    \item Examples now name most arguments.
    \item Vignettes now name most argument.
    \item some print functions had number of plausible values added.
  }
}
