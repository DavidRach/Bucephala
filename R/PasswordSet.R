#' Uses the keyring package to set your username credentials
#' 
#' @param KeyringIgnore Default FALSE, set to TRUE to proceed with
#'  storing credentials in default env when secure keychain setup
#'  is still incomplete (can occur on some Linux OS)
#' 
#' @importFrom keyring key_set
#' 
#' @return Stores password for future use. 
#' 
#' @export
#' 
#' @examples
#' 
#' A <- 2 + 2
#' 
UsernameSet <- function(KeyringIgnore = FALSE){
  check <- KeyringCheck(KeyringIgnore = KeyringIgnore)
  if(check == FALSE){return(invisible(NULL))}
  key_set("THE_USER")
}

#' Uses the keyring package to set your password credentials
#' 
#' @param KeyringIgnore Default FALSE, set to TRUE to proceed with
#'  storing credentials in default env when secure keychain setup
#'  is still incomplete (can occur on some Linux OS)
#' 
#' @importFrom keyring key_set
#' 
#' @return Stores password for future use. 
#' 
#' @export
#' 
#' @examples
#' 
#' A <- 2 + 2
#' 
PasswordSet <- function(KeyringIgnore = FALSE){
  check <- KeyringCheck(KeyringIgnore = KeyringIgnore)
  if(check == FALSE){return(invisible(NULL))}
  key_set("THE_PASS")
}

#' Check whether keyring is correctly set up. 
#' 
#' Should work for Windows/MacOS, Linux can be configured correctly, 
#' but may require manual intervention. Provides heads-up before
#' proceeding to default env option. 
#' 
#' @param KeyringIgnore Ignores the keyring check, proceeds with the
#' use of the env variable
#' 
#' @importFrom keyring default_backend
#' 
#' @return TRUE if keyring is setup or bypassed, otherwise false
#' 
#' @keywords internal
#' 
KeyringCheck <- function(KeyringIgnore = FALSE) {
  
  OperatingSystem <- Sys.info()["sysname"]
  
  if (OperatingSystem == "Linux"){
    options(keyring_backend = "env")
  }

  backend <- tryCatch(
    default_backend(),
    error = function(e) NULL
  )
  
  if (is.null(backend) || inherits(backend, "backend_env")) {

  if (KeyringIgnore == FALSE){
    warning(
        "No secure keychain configured for this system. \n",
        "Defaulting to keyring_backend = `env` \n",
        "Attempt to fix, or proceed with env option \n",
        "by setting argument KeyringIgnore = TRUE",
        call. = FALSE
      )
  }
    
    if (KeyringIgnore == TRUE){return(TRUE)
    } else {return(FALSE)}
  } else {return(TRUE)}
}