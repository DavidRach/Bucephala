#' 
#' 
#' @param x TBD
#' @param chromote_session TBD
#' 
#' @importFrom lubridate wday
#' @importFrom dplyr mutate bind_rows filter pull
#' @importFrom purrr map compact
#' 
#' @noRd
#' 
#' 
iLabScraper <- function(x, chromote_session, ParentURL="https://cibr.umaryland.edu/schedules",
 InstrumentSet){
  
  TodaysDate <- Sys.Date()

  WeekSunday <- TodaysDate - wday(TodaysDate, WeekStart = 7) + 1 
  InstrumentURL <- file.path(ParentURL, x)
  URLPath <- file.path(InstrumentURL, "schedule", "week7", TodaysDate)
  InstrumentName <- InstrumentSet |> filter(Code %in% x) |> pull(TheInstrument)

  chromote_session$Page$navigate(URLPath)
  Sys.sleep(3)
  # img <- Browser$screenshot(wait_ = TRUE)
  doc <- chromote_session$DOM$getDocument()
  page_html <- chromote_session$DOM$getOuterHTML(nodeId = doc$root$nodeId)[["outerHTML"]]
  page <- read_html(page_html)
  scheduler <- page %>% html_node("#scheduler_here")
  cal_data <- scheduler %>% html_node(".dhx_cal_data")
  day_columns <- cal_data %>% html_nodes(xpath = ".//div[contains(@class, 'dhx_scale_holder')]")
  WorthIt <- cal_data %>% html_nodes(".dhx_cal_event")
  CancelledIndex  <- grep("cancelled", WorthIt)

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