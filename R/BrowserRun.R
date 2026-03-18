#' This Function Returns a Running Chromote Session
#' 
#' @param theurl URL to website
#' @param theCookies Path to the stored cookies
#' 
#' @noRd
#' 
BrowserRun  <- function(theURL, theCookies="umb_cookies.rds"){

if (!file.exists(Cookies)){
  PreviousCookies <- FALSE #Edit
  ActiveCookies <- FALSE
  Browser <- ChromoteSession$new()
  Browser$Page$navigate(URLPath)
  Sys.sleep(3)
  img <- Browser$screenshot(wait_ = TRUE)
  #Browser$view()
  } else 
  {
    PreviousCookies <- TRUE
    Cookies <- readRDS(Cookies)
    Browser <- ChromoteSession$new()
    Browser$Page$enable()
    Browser$Network$enable()
    for (c in Cookies$cookies) 
      {
      Browser$Network$setCookie(
        name = c$name,
        value = c$value,
        domain = c$domain,
        path = c$path,
        secure = c$secure,
        httpOnly = c$httpOnly
            )
      }
  }
  
}