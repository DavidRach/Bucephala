#'
#' 
#' @param x TBD
#' @param InstrumentName TBD
#' @param DayWeek TBD
#' 
#' @importFrom lubridate days parse_date_time
#' @importFrom stringr str_split
#' 
#' @noRd
#' 
#' 
DataParse <- function(x, InstrumentName, DayWeek){

User <- x %>% html_node(".owner a") %>% html_text(trim = TRUE)
Email <- x %>% html_node(".owner a") %>% html_attr("href") %>% stringr::str_remove("^mailto:")
Type <- x %>% html_node(".type") %>% html_text(trim = TRUE)
Type <- gsub(" Usage", "", Type)
Timeslot <- x %>% html_node(".dhx_title") %>% html_text(trim = TRUE)

times <- str_split(Timeslot, " - ", simplify = TRUE)
Start <- times[1]
Stop <- times[2]
StartTime <- parse_date_time(Start, orders = "I:M p")
StopTime <- parse_date_time(Stop, orders = "I:M p")

if (StopTime < StartTime) {
    StopTime <- StopTime + days(1)
  }

Duration <- as.numeric(difftime(StopTime, StartTime, units = "hours")) 

Data <- data.frame(
    User = User,
    Email = Email,
    Instrument=InstrumentName,
    Type = Type,
    Day=DayWeek,
    Start = Start,
    Stop = Stop,
    Duration = Duration)

return(Data)
}