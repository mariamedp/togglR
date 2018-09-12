
#' @export
getWorkspaces <- function() {

  ENDPOINT <- "https://www.toggl.com/api/v8/workspaces"

  resp <- httr::GET(url = ENDPOINT,
                    httr::add_headers(Authorization = get_api_auth()))

  json <- rawToChar(resp$content)

}

#' @export
getTimeEntries <- function(workspace_id, start_date, end_date) {

  ENDPOINT <- "https://toggl.com/reports/api/v2/details"

  resp <- httr::GET(url = ENDPOINT,
                    query = list("workspace_id" = workspace_id, "since" = start_date, "until" = end_date, "user_agent" = "control_horas"),
                    httr::add_headers(Authorization = get_api_auth()))

  json <- rawToChar(resp$content)
  json <- jsonlite::fromJSON(json)

  entries <- formatTimeEntries(json)
  page <- 1
  while (nrow(entries) < json$total_count) {
    page <- page + 1

    resp <- httr::GET(url = ENDPOINT,
                         query = list("workspace_id" = workspace_id, "since" = start_date, "until" = end_date, "user_agent" = "control_horas",
                                      "page" = page),
                         httr::add_headers(Authorization = get_api_auth()))

    json <- rawToChar(resp$content)
    json <- jsonlite::fromJSON(json)

    entries <- rbind(entries, formatTimeEntries(json))
  }

  return(entries)
}


formatTimeEntries <- function(json) {

  entries <- data.table(json$data)

  if (nrow(entries) > 0) {
    entries[, c("id", "tid", "pid", "uid", "user", "updated", "dur", "use_stop", "project_color", "project_hex_color", "task", "billable", "is_billable", "cur") := NULL]

    entries[, start := gsub(":([0-9]{2})$", "\\1", start)]
    entries[, start := as.POSIXct(start, format="%Y-%m-%dT%T%z")]
    entries[, end := gsub(":([0-9]{2})$", "\\1", end)]
    entries[, end := as.POSIXct(end, format="%Y-%m-%dT%T%z")]

    setcolorder(entries, c("client", "project", "description", "start", "end", "tags"))

  } else {
    entries <- data.table(client=character(0), project=character(0), description=character(0),
                          start=as.POSIXct(logical(0)), end=as.POSIXct(logical(0)), tags=character(0))
  }

  return(entries)
}

#' @export
uploadTimeEntry <- function(workspace_id, project_id, description, start, duration, tags=NULL) {

  ENDPOINT <- "https://www.toggl.com/api/v8/time_entries"

  start_str <- format(start, format="%Y-%m-%dT%T%z")
  start_str <- gsub("([0-9]{2})([0-9]{2})$", "\\1:\\2", start_str)

  resp <- httr::POST(url = ENDPOINT,
                     body = list("time_entry" = list("workspace_id" = workspace_id,
                                                     "pid" = project_id,
                                                     "description" = description,
                                                     "tags" = tags,
                                                     "start" = start_str,
                                                     "duration" = duration,
                                                     "created_with" = "httr")),
                     encode = "json",
                     httr::add_headers(Authorization = get_api_auth()))

  invisible(resp)
}

# deleteTimeEntry <- function(workspace_id, project_id, description, tags, start, duration) {
#
#   ENDPOINT <- "https://www.toggl.com/api/v8/time_entries"
#
#   start_str <- format(start, format="%Y-%m-%dT%T%z")
#   start_str <- gsub("([0-9]{2})([0-9]{2})$", "\\1:\\2", start_str)
#
#   resp <- httr::POST(url = ENDPOINT,
#                      body = list("time_entry" = list("workspace_id" = workspace_id,
#                                                      "pid" = project_id,
#                                                      "description" = description,
#                                                      "tags" = tags,
#                                                      "start" = start_str,
#                                                      "duration" = duration,
#                                                      "created_with" = "httr")),
#                      encode = "json",
#                      httr::add_headers(Authorization = get_api_auth()))
#
#   # return(resp)
# }



