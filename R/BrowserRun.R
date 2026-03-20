#' This Function Returns a Running Chromote Session
#' 
#' @param theurl Default is "https://cibr.umaryland.edu/schedules", swap 
#' in your own institutions iLab page url
#' @param instrumenthash  Default is "512453#", swap 
#' in your own instruments url hash
#' @param FolderName Default ScheduleMonitor, can switch 
#' out as desired
#' @param AlternateDirectory Default is your Documents folder,
#'  can switch out if desired
#' @param pattern For # usethis::edit_r_environ(), use THE_USER and THE_PASS
#' @param PauseInterval Seconds between headless browser steps to allow page to load.
#' 
#' @importFrom chromote ChromoteSession
#' 
#' @return An active chromote session post login
#' 
#' @export
#' 
#' @examples
#' 
#' A <- 2 + 2
#' 
#' 
BrowserRun  <- function(theurl="https://cibr.umaryland.edu/schedules",
  instrumenthash="512453#", FolderName="ScheduleMonitor", AlternateDirectory=NULL,
  pattern=NULL, PauseInterval=5){
  
  if(!is.null(pattern)){print("Hello!")}

  # Checking for Existing Cookies in ScheduleMonitor folder
  FolderPattern <- paste0("^", FolderName, "$")
  if(!is.null(AlternateDirectory)){(DocumentsPath <- AlternateDirectory)
    } else {DocumentsPath <- OperatingSystemCheck()}
  MonitorFolder <- list.files(DocumentsPath, pattern=FolderPattern,
    full.names=TRUE)
  if (length(MonitorFolder) == 1){
    CookieFolder <- list.files(MonitorFolder, pattern="cookies",full.names=TRUE)
    if (length(MonitorFolder) == 1){
      ExistingCookies <- list.files(MonitorFolder, pattern=".rds",full.names=TRUE)
      ScreenshotPath <- list.files(MonitorFolder, pattern="screenshots",full.names=TRUE)
      if (length(ExistingCookies) == 0 ){
        Cookies <- FALSE
      } else {Cookies <- TRUE}
    } else {stop(FolderName, " not Found")}
  } else {stop(FolderName, " not Found")}

  # Set Up iLab URL
  TodaysDate <- Sys.Date()
  InstrumentURL <- file.path(theurl, instrumenthash)
  URLPath <- file.path(InstrumentURL, "schedule", "week7", TodaysDate) #InternetConflict?

  # Start a Chromote Headless Browser
  Browser <- ChromoteSession$new()

  # Load Cookies if Existant
  if (Cookies == TRUE){ 
      Cookies <- readRDS(ExistingCookies)
      Browser$Page$enable()
      Browser$Network$enable()
      for (c in Cookies$cookies){
        Browser$Network$setCookie(name = c$name, value = c$value,
          domain = c$domain, path = c$path, secure = c$secure,
          httpOnly = c$httpOnly)
        }
    }

  # Start Chromote Browser
  Browser$Page$navigate(URLPath)
  Sys.sleep(PauseInterval)
  img <- Browser$screenshot(wait_ = TRUE)
  file.rename("screenshot.png", file.path(ScreenshotPath, "00_OpeningScreenshot.png"))
  #Browser$view()

  # Is LogIn Button Present (ie. not logged in)
  LoginButton <- Browser$Runtime$evaluate("
    !!document.querySelector('a.ui.positive.button.login.login_link')
  ")$result$value

  if (LoginButton == TRUE){
    # Renavigate to Page
    Browser$Page$navigate("https://cibr.umaryland.edu/landing/1754")
    Sys.sleep(PauseInterval)
    img <- Browser$screenshot(wait_ = TRUE)
    Sys.sleep(PauseInterval)
    file.rename("screenshot.png", file.path(ScreenshotPath, "01_AttemptingLogin.png"))

    # Press Login
    Browser$Runtime$evaluate("
      const loginButton = document.querySelector('a.ui.positive.button.login.login_link');
      if (loginButton) loginButton.click();
    ")
    Sys.sleep(PauseInterval)
    img <- Browser$screenshot(wait_ = TRUE)
    file.rename("screenshot.png", file.path(ScreenshotPath, "02_PressLogin.png"))

    # UMB Specific Landing Page
    Browser$Runtime$evaluate("
      const umbLink = document.querySelector('a[href=\"/account/saml/umb\"]');
      if (umbLink) umbLink.click();
    ") # May Need To Generalize UMB In The Future
    Sys.sleep(PauseInterval)
    img <- Browser$screenshot(wait_ = TRUE)
    file.rename("screenshot.png", file.path(ScreenshotPath, "03_UMD_Duo.png"))

    # Loging In
    Browser$Runtime$evaluate(sprintf(
      "document.querySelector('#username').value = '%s';",
      Sys.getenv('THE_USER') # Need to set generalized keyword later
    ))
    Sys.sleep(PauseInterval)
    img <- Browser$screenshot(wait_ = TRUE)
    file.rename("screenshot.png", file.path(ScreenshotPath, "04_User.png"))

    Browser$Runtime$evaluate(sprintf(
      "document.querySelector('#password').value = '%s';",
      Sys.getenv('THE_PASS')  # Need to set generalized keyword later
    ))
    Sys.sleep(PauseInterval)
    img <- Browser$screenshot(wait_ = TRUE)
    file.rename("screenshot.png", file.path(ScreenshotPath, "05_LoginReady.png"))

    Sys.sleep(PauseInterval)
    Browser$Runtime$evaluate("
      const btn = document.querySelector('button[type=submit], input[type=submit]');
      if (btn) {
        btn.click();
      } else {
        const form = document.querySelector('form');
        if (form) form.submit();
      }
    ")
    Sys.sleep(PauseInterval)
    img <- Browser$screenshot(wait_ = TRUE)
    file.rename("screenshot.png", file.path(ScreenshotPath, "06_PostCredentials.png"))

    # Evaluate Whether 2FA has been triggered

    LoginButton <- Browser$Runtime$evaluate("
      !!document.querySelector('a.ui.positive.button.login.login_link')
    ")$result$value

     Sys.sleep(PauseInterval) 

    if (LoginButton == FALSE){
      Browser$Runtime$evaluate("
      const otherOptions = document.querySelector('div.other-options-link a.button--link');
      if (otherOptions) {
        console.log('Clicking Other Options link...');
        otherOptions.click();
      } else {
        console.log('Other Options link not found.');
      }
    ") # Other Options
    Sys.sleep(PauseInterval) 
    img <- Browser$screenshot(wait_ = TRUE)
    file.rename("screenshot.png", file.path(ScreenshotPath, "07_OtherOptions.png"))
      
    Browser$Runtime$evaluate("
      const phoneCall = document.querySelector('li[data-testid=\"test-id-phone\"] a.auth-method-link');
      if (phoneCall) {
        console.log('Selecting Phone call for MFA...');
        phoneCall.click();
      } else {
        console.log('Phone call option not found.');
      }
    ") # Select Phone Call
    Sys.sleep(PauseInterval+10) # Shoot, where is my phone?!?!?!
    img <- Browser$screenshot(wait_ = TRUE)
    file.rename("screenshot.png", file.path(ScreenshotPath, "08_PhoneUte.png"))
      
    Browser$Runtime$evaluate("
      const trustButton = document.querySelector('#trust-browser-button');
      if (trustButton) {
        console.log('Clicking Yes, this is my device...');
        trustButton.click();
      } else {
        console.log('Trust browser button not found.');
      }
    ")
    Sys.sleep(PauseInterval)
    img <- Browser$screenshot(wait_ = TRUE)
    file.rename("screenshot.png", file.path(ScreenshotPath, "09_YesTrust.png"))
    } # End of 2FA
  } # End of Initial Login

  Browser$Page$navigate(URLPath)
  Sys.sleep(PauseInterval)
  img <- Browser$screenshot(wait_ = TRUE)
  file.rename("screenshot.png", file.path(ScreenshotPath, "10_PostLoginScreenshot.png"))
  #Browser$view()

  TheCookies <- Browser$Network$getAllCookies()
  StorageLocation <- file.path(CookieFolder, "cookies.rds")
  if (!is.null(TheCookies$cookies)) saveRDS(TheCookies, StorageLocation)

  return(Browser)
}