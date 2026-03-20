
#' Internal for iLab Scraper, processes the individual html returns
#' for relavent booking information
#' 
#' @param x The individual html blob I think
#' @param InstrumentName The iterated in Instrument Name
#' 
#' @importFrom rvest html_nodes
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' 
#' @noRd
#' 
#' 
CalendarParse <- function(x, InstrumentName){

  DayWeek <- get_day(x) # Our Internal

  Reservations <- x |> html_nodes(".dhx_cal_event")

  CancelledIndex  <- grep("cancelled", Reservations)

  if (length(CancelledIndex) > 0){
    Reservations  <- Reservations [-CancelledIndex]
  }

  if (length(Reservations) == 0){MyDataset <- NULL
  } else {

  MyDataset <- map(.x = Reservations, .f = DataParse,
   InstrumentName=InstrumentName, DayWeek=DayWeek) |> bind_rows()

  return(MyDataset)
  }
}