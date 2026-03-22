#' Internal for iLabScraper
#' 
#' @param node The iLab entry being passed in. 
#' 
#' @importFrom stringr str_match
#' @importFrom rvest html_attr
#' 
#' @noRd
#' 
get_left <- function(node) {
  style <- html_attr(node, "style")
  m <- stringr::str_match(style, "left:\\s*(\\d+)px")
  as.numeric(m[,2])
}