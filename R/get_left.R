get_left <- function(node) {
  style <- html_attr(node, "style")
  m <- stringr::str_match(style, "left:\\s*(\\d+)px")
  as.numeric(m[,2])
}