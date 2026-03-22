#' Internal for Calendar Parse
#' 
#' @param node The entry being passed in
#' 
#' @importFrom stringr str_match
#' @importFrom rvest html_attr
#' 
#' @noRd
#' 
get_day <- function(node) {
  style <- html_attr(node, "style")
  m <- str_match(style, "left:\\s*(\\d+\\.?\\d*)px")
  left_px <- as.numeric(m[,2])
  
  day <- names(DayMap)[
    sapply(DayMap, function(r) left_px >= r[1] & left_px <= r[2])
  ]
  
  if (length(day) == 0) return(NA)
  return(day)
}