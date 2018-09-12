

## Get workspaces


## Get single workspace


## Update workspace


## Get workspace users



#' Get workspace clients
#'
#' @param workspace_id
#'
#' @return data.table with columns: client_id, client_name
#' @export
#'
getClients <- function(workspace_id) {

  ENDPOINT <- paste0("https://www.toggl.com/api/v8/workspaces/", workspace_id, "/clients")

  resp <- httr::GET(url = ENDPOINT,
                    httr::add_headers(Authorization = get_api_auth()))

  json <- rawToChar(resp$content)
  clients <- as.data.table(jsonlite::fromJSON(json))

  return(clients[, .(client_id = id, client_name = name)])
}




## Get workspace groups


#' Get workspace projects
#'
#' @param workspace_id
#'
#' @return data.table with columns: project_id, project_name, client_id
#' @export
#'
getProjects <- function(workspace_id) {

  ENDPOINT <- paste0("https://www.toggl.com/api/v8/workspaces/", workspace_id, "/projects")

  resp <- httr::GET(url = ENDPOINT,
                    httr::add_headers(Authorization = get_api_auth()))

  json <- rawToChar(resp$content)
  projects <- as.data.table(jsonlite::fromJSON(json))

  return(projects[, .(project_id = id, project_name = name, client_id = cid)])
}

## Get workspace tasks


## Get workspace tags







