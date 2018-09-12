
#' Create a client
#'
#' @param workspace_id workspace ID, where the client will be used (integer, required)
#' @param name The name of the client (string, required, unique in workspace)
#' @param notes Notes for the client (string, not required)
#'
#' @export
#'
createClient <- function(workspace_id, name, notes=NULL) {

  ENDPOINT <- "https://www.toggl.com/api/v8/clients"

  resp <- httr::POST(url = ENDPOINT,
                     body = list("client" = list("wid" = workspace_id,
                                                 "name" = name,
                                                 "notes" = notes)),
                     encode = "json",
                     httr::add_headers(Authorization = get_api_auth()))

  invisible(resp)

}



#' Get client details
#'
#' @param workspace_id
#' @param name
#' @param notes
#'
#' @return
#' @export
#'
getClient <- function(client_id, name, notes=NULL) {

  ENDPOINT <- paste0("https://www.toggl.com/api/v8/clients/", client_id)

  resp <- httr::GET(url = ENDPOINT,
                    httr::add_headers(Authorization = get_api_auth()))

  json <- rawToChar(resp$content)
  client <- jsonlite::fromJSON(json)$data

  return(client)

}
