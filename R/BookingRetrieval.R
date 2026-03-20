#' Orchestrates retrieving the bookings from iLab
#' 
#' @param data data.frame with "TheInstrument" column, containing the
#' instrument names, followed by a "Code" column with the instrument url hash
#' @param chromote_session The returned chromote session from BrowserRun()
#' @param theurl Default is "https://cibr.umaryland.edu/schedules", swap 
#' in your own institutions iLab page url
#' @param FolderName Default ScheduleMonitor, can switch 
#' out as desired
#' @param AlternateDirectory Default is your Documents folder,
#'  can switch out if desired
#' 
#' @importFrom dplyr pull bind_rows
#' @importFrom purrr map imap set_names compact
#' @importFrom utils write.csv
#' 
#' @return CSV files to the data folder, and a data.frame to R. 
#' 
#' @export 
#' 
#' @examples 
#' 
#' A <- 2 + 2
#' 
BookingRetrieval <- function(data, chromote_session,
 theurl="https://cibr.umaryland.edu/schedules",
 FolderName="ScheduleMonitor", AlternateDirectory=NULL){

  # Checking for data folder within ScheduleMonitor folder
  FolderPattern <- paste0("^", FolderName, "$")
  if(!is.null(AlternateDirectory)){(DocumentsPath <- AlternateDirectory)
    } else {DocumentsPath <- OperatingSystemCheck()}
  MonitorFolder <- list.files(DocumentsPath, pattern=FolderPattern,
    full.names=TRUE)
  if (length(MonitorFolder) == 1){
    DataFolder <- list.files(MonitorFolder, pattern="data",full.names=TRUE)
    if (length(DataFolder) == 1){
  # Successfully found the data folder
    } else {stop(FolderName, " data folder not Found")}
  } else {stop(FolderName, " not Found")}

  Instrument <- data |> pull(Code)

  CompleteDataset <- purrr::map(.f=iLabScraper, .x=Instrument,
   chromote_session=chromote_session, theurl=theurl,
   FolderName=FolderName, AlternateDirectory=AlternateDirectory,
   InstrumentSet=data)

  Named <- purrr::set_names(CompleteDataset, TheInstrument)

  ReturnedCSVs <- imap(Named, \(df, name) write.csv(df,
   file = file.path(DataFolder, paste0(name, ".csv")), row.names = FALSE))
  Combined <- purrr::compact(Named) |> dplyr::bind_rows()

  StorageLocation <- file.path(DataFolder, "Combined.csv")
  write.csv(Combined, StorageLocation, row.names=FALSE)
  return(Combined)
}