#' 
#' 
#' @param x The iterated in instrument url hash
#' @param chromote_session The returned chromote session from BrowserRun()
#' @param theurl Default is "https://cibr.umaryland.edu/schedules", swap 
#' in your own institutions iLab page url
#' @param FolderName Default ScheduleMonitor, can switch 
#' out as desired
#' @param AlternateDirectory Default is your Documents folder,
#'  can switch out if desired
#' @param InstrumentSet data.frame with "TheInstrument" column, containing the
#' instrument names, followed by a "Code" column with the instrument url hash
#' 
#' @importFrom lubridate wday
#' @importFrom dplyr mutate bind_rows filter pull
#' @importFrom purrr map compact
#' @importFrom rvest read_html html_node html_nodes
#' 
#' @noRd
#' 
#' 
iLabScraper <- function(x, chromote_session, theurl="https://cibr.umaryland.edu/schedules",
 FolderName="ScheduleMonitor", AlternateDirectory=NULL, InstrumentSet){

  # Checking for Existing Screenshots in ScheduleMonitor folder
  FolderPattern <- paste0("^", FolderName, "$")
  if(!is.null(AlternateDirectory)){(DocumentsPath <- AlternateDirectory)
    } else {DocumentsPath <- OperatingSystemCheck()}
  MonitorFolder <- list.files(DocumentsPath, pattern=FolderPattern,
    full.names=TRUE)
  if (length(MonitorFolder) == 1){
    ScreenshotFolder <- list.files(MonitorFolder, pattern="screenshots",full.names=TRUE)
    if (length(ScreenshotFolder) == 1){
      # Successfully found the screenshots folder
    } else {stop(FolderName, " screenshot folder not Found")}
  } else {stop(FolderName, " not Found")}
  
  # Navigating to the correct instrument page
  TodaysDate <- Sys.Date()
  WeekSunday <- TodaysDate - wday(TodaysDate, week_start = 7) + 1 
  InstrumentURL <- file.path(theurl, x)
  URLPath <- file.path(InstrumentURL, "schedule", "week7", TodaysDate)
  InstrumentName <- InstrumentSet |> filter(Code %in% x) |> pull(TheInstrument)
  chromote_session$Page$navigate(URLPath)
  Sys.sleep(3)
  img <- chromote_session$screenshot(wait_ = TRUE)
  ScreenshotName <- paste0(InstrumentName, ".png") 
  file.rename("screenshot.png", file.path(ScreenshotFolder, ScreenshotName))

  # Scraping the instrument page html

  doc <- chromote_session$DOM$getDocument()
  page_html <- chromote_session$DOM$getOuterHTML(nodeId = doc$root$nodeId)[["outerHTML"]]
  page <- read_html(page_html)
  scheduler <- page |> html_node("#scheduler_here")
  cal_data <- scheduler |> html_node(".dhx_cal_data")
  day_columns <- cal_data |> html_nodes(xpath = ".//div[contains(@class, 'dhx_scale_holder')]")
  WorthIt <- cal_data |> html_nodes(".dhx_cal_event")
  CancelledIndex  <- grep("cancelled", WorthIt)

  # Removed Cancelled Appointments # May need to rethink approach 
  # if my goal is to enumerate schedule changes
  if (length(CancelledIndex) > 0){
    WorthIt <- WorthIt[-CancelledIndex]
    day_columns <- day_columns[-CancelledIndex]
  }

  if (length(WorthIt) > 0){
  MyDataset <- map(.f=CalendarParse, .x=day_columns, InstrumentName=InstrumentName)
  MyDataset <- compact(MyDataset) |> bind_rows()
  MyDataset <- MyDataset |> mutate(Date = WeekSunday + DayOffset[Day])
  } else {MyDataset <- NULL}

  return(MyDataset)
}