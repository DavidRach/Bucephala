
#'
#' @param x TBD
#' @param InstrumentName TBD
#' 
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' 
#' @noRd
#' 
#' 
CalendarParse <- function(x, InstrumentName){

  DayWeek <- get_day(x)

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