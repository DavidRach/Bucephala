get_day <- function(node) {
  style <- html_attr(node, "style")
  m <- stringr::str_match(style, "left:\\s*(\\d+\\.?\\d*)px")
  left_px <- as.numeric(m[,2])
  
  day <- names(DayMap)[
    sapply(DayMap, function(r) left_px >= r[1] & left_px <= r[2])
  ]
  
  if (length(day) == 0) return(NA)
  return(day)
}