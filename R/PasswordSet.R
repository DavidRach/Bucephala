#' Uses the keyring package to set your username credentials
#' 
#' @param KeyringIgnore If TRUE, proceeds with storing credentials
#'   in environment variables when no secure keychain is available. 
#'   Default is FALSE.
#' 
#' @importFrom keyring key_set
#' 
#' @return Securely stores username on your local system
#' 
#' @export
#' 
#' @examples
#' 
#' A <- 2 + 2
#' 
UsernameSet <- function(KeyringIgnore = FALSE){
  check <- KeyringCheck(KeyringIgnore = KeyringIgnore)
  if(!isTRUE(check)) return(invisible(NULL))
  key_set(service = "Bucephala", username = "THE_USER")
}

#' Uses the keyring package to set your password credentials
#' 
#' @param KeyringIgnore If TRUE, proceeds with storing credentials
#'   in environment variables when no secure keychain is available. 
#'   Default is FALSE.
#' 
#' @importFrom keyring key_set
#' 
#' @return Securely stores password on your local system
#' 
#' @export
#' 
#' @examples
#' 
#' A <- 2 + 2
#' 
PasswordSet <- function(KeyringIgnore = FALSE){
  check <- KeyringCheck(KeyringIgnore = KeyringIgnore)
  if(!isTRUE(check)) return(invisible(NULL))
  key_set(service = "Bucephala", username = "THE_PASS")
}

#' Check keyring backend security
#' 
#' Internal helper that checks whether a secure keychain is available
#' and warns the user if falling back to the insecure env backend.
#' 
#' @param KeyringIgnore If TRUE, proceeds with storing credentials
#'   in environment variables when no secure keychain is available. 
#'   Default is FALSE.
#' 
#' @importFrom keyring default_backend
#' 
#' @return TRUE if it is safe to proceed, FALSE otherwise
#' 
#' @keywords internal
#' 
KeyringCheck <- function(KeyringIgnore = FALSE) {
  
  if (.Platform$OS.type == "unix" && !grepl("darwin", R.version$os)) {
    options(keyring_backend = "file")
  }
  
  backend <- tryCatch(
    keyring::default_backend(),
    error = function(e) NULL
  )
  
  if (is.null(backend) || inherits(backend, "backend_env")) {
    if (!isTRUE(KeyringIgnore)) {
      warning(
        "No secure keychain found on this system. When this is the case ",
        "secrets will be stored in environment variables only. Consider ",
        "installing a keychain manager:\n",
        "For macOS/Windows: should work automatically, let us know otherwise! \n",
        "For Linux: install gnome-keyring or libsecret, otherwise ",
        "set the KeyringIgnore argument to TRUE to proceed using ",
        "environment variables only",
        call. = FALSE
      )
      return(FALSE)
    }
  }
  
  return(TRUE)
}