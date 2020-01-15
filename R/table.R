#' Get a table on Synapse
#'
#' Downloads a table on Synapse. Either as a dataframe, a query object, or returns
#' the query string by itself.
#'
#' @param synapse_id The Synapse ID of the table.
#' @param columns A character vector of columns to download. Defaults to all columns.
#' @param conditionals A character vector of SQL conditionals.
#' @param as_query_str If TRUE, returns the query string without executing the query.
#' Defaults to FALSE.
#' @param as_data_frame If TRUE, returns the table as a dataframe. Defaults to TRUE.
#'
#' @export
synGetTable <- function(synapse_id, columns = list(), conditionals = list(),
                        as_query_str = FALSE, as_data_frame = TRUE) {
  query_str <-  paste0("SELECT ", ifelse(length(columns), paste(columns, collapse = ", "), "*"),
                       " FROM ", synapse_id, ifelse(length(conditionals), " WHERE ", ""),
                       paste(conditionals, collapse = " AND "))
  if (as_query_str) {
    return(query_str)
  }
  table <- synapser::synTableQuery(query_str)
  if (as_data_frame) {
    return(tibble::as_tibble(table$asDataFrame()))
  }
  return(table)
}
