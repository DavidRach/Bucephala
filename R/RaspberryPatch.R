#' Buils the .R and .sh scripts needed to automate
#' 
#' @param FolderName Default is ScheduleMonitor, can switch 
#' out as desired
#' @param AlternateDirectory Default is your Documents folder,
#'  can switch out if desired
#' 
#' @importFrom utils write.csv
#' 
#' @return .R and .sh scripts to the FolderName folder
#' 
#' @export
#' 
#' @examples
#' 
#' A <- 2+2
#' 
RaspberryPatch <- function(FolderName="ScheduleMonitor", AlternateDirectory=NULL){

    # Checking for the ScheduleMonitor folder
    FolderPattern <- paste0("^", FolderName, "$")
    if(!is.null(AlternateDirectory)){(DocumentsPath <- AlternateDirectory)
    } else {DocumentsPath <- OperatingSystemCheck()}
    MonitorFolder <- list.files(DocumentsPath, pattern=FolderPattern,
    full.names=TRUE)
    if (length( MonitorFolder) == 0){
      stop(FolderName, " folder found, run FolderSetup")
    }
  
    




}