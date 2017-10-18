#' carpsReport function
#'
#' This is a helper function for building standardised final outcomes for CARPS reproducibility reports.
#' @param Report_Type Enter 'pilot' or 'final'
#' @param Article_ID Enter the article's unique ID code
#' @param Insufficient_Information_Errors Enter the number of Insufficient Information Errors
#' @param Decision_Errors Enter the number of decision errors
#' @param Major_Numerical_Errors Enter the number of major numerical errors
#' @param Time_to_Complete Enter the estimated time to complete the report in minutes
#' @param Substantive_Conclusion_Affected Enter whether you think the substantive conclusion of the article is affected by any errors you encountered (T/F)
#' @param Author_Assistance Enter whether author assistance was required (T/F)
#' @return Returns a formatted table (via kable) reporting error tallys. Also writes out a csv object containing the error tallys.
#' @export
#' @examples
#' carpsReport(Report_Type = "pilot", Article_ID = "ABhgyo", Insufficient_Information_Errors = 0, Decision_Errors = 1, Major_Numerical_Errors = 4, Time_to_Complete = 120, Substantive_Conclusion_Affected = F, Author_Assistance = T)

carpsReport <- function(Report_Type, Article_ID, Insufficient_Information_Errors, Decision_Errors, Major_Numerical_Errors, Time_to_Complete, Substantive_Conclusion_Affected, Author_Assistance){

  # input check
  if(!(Report_Type %in% c('pilot', 'final'))) stop("Error! Report_Type must be either 'pilot' or 'final'.")

  if(Decision_Errors > 0 | Major_Numerical_Errors > 0 | Insufficient_Information_Errors > 0){
    finalOutcome <- "Failure"
    if(Author_Assistance == T){
      finalOutcome <- "Failure despite author assistance"
    }
  }else{
    finalOutcome <- "Success"
    if(Author_Assistance == T){
      finalOutcome <- "Success with author assistance"
    }
  }

  reportObject <- data.frame("Insufficient_Information_Errors" = Insufficient_Information_Errors,
                             "Decision_Errors" = Decision_Errors,
                             "Major_Numerical_Errors" = Major_Numerical_Errors,
                             "Substantive_Conclusion_Affected" = Substantive_Conclusion_Affected,
                             "Time_to_Complete" = Time_to_Complete,
                             "Final_Outcome" = finalOutcome)

  filename <- paste0("reportObject_",Report_Type,"_",Article_ID,".csv")
  write.csv(reportObject, filename, row.names = F)
  return(kable(reportObject))
}
