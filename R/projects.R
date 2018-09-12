


#' Create project
#'
#' @param workspace_id workspace ID, where the project will be saved (integer, required)
#' @param name The name of the project (string, required, unique for client and workspace)
#' @param client_id client ID (integer, not required)
#' @param is_private whether project is accessible for only project users or for all workspace users (boolean, default true)
#'
#' @export
createProject <- function(workspace_id, name, client_id=NULL, is_private=TRUE) {

  ENDPOINT <- "https://www.toggl.com/api/v8/projects"

  resp <- httr::POST(url = ENDPOINT,
                     body = list("project" = list("wid" = workspace_id,
                                                  "name" = name,
                                                  "client_id" = client_id,
                                                  "is_private" = is_private)),
                     encode = "json",
                     httr::add_headers(Authorization = get_api_auth()))

  invisible(resp)

}

