#' Creates a "ScheduleMonitor" folder under Documents. 
#' 
#' @param FolderName Default is ScheduleMonitor, can switch 
#' out as desired
#' @param AlternateDirectory Default is your Documents folder,
#'  can switch out if desired
#' 
#' @return A ScheduleMonitor folder with its subfolders.
#' 
#' @export
#' 
#' @examples
#' 
#' A <- 2 + 2
#' 
FolderSetup <- function(FolderName="ScheduleMonitor", AlternateDirectory=NULL){

    FolderPattern <- paste0("^", FolderName, "$")

    if(!is.null(AlternateDirectory)){(DocumentsPath <- AlternateDirectory)
    } else {DocumentsPath <- OperatingSystemCheck()}

    MonitorFolder <- list.files(DocumentsPath, pattern=FolderPattern,
    full.names=TRUE)
    
    # Checking for  MonitorFolder
    if (length( MonitorFolder) > 0){
      message(FolderName, " folder found, skipping")
    } else {
      # Creating  MonitorFolder and subfolders
      message(FolderName, " folder not found, creating")
      dir.create(file.path(DocumentsPath, FolderName), showWarnings = FALSE)
      MonitorFolderPath <- file.path(DocumentsPath, FolderName)
      dir.create(file.path(MonitorFolderPath, "data"), showWarnings = FALSE)
      dir.create(file.path(MonitorFolderPath, "images"), showWarnings = FALSE)
      dir.create(file.path(MonitorFolderPath, "cookies"), showWarnings = FALSE)
      dir.create(file.path(MonitorFolderPath, "screenshots"), showWarnings = FALSE)
    }

}