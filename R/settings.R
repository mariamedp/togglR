
#' @export
api_setup <- function(token) {

  Sys.setenv("TOGGLR_API_AUTH" = paste("Basic", base64enc::base64encode(charToRaw(paste0(token, ":api_token")))))

}

get_api_auth <- function() {

  auth <- Sys.getenv("TOGGLR_API_AUTH")

  if (auth == "") {
    stop("Call api_setup() to set the API key first.")
  }

  return(auth)
}

