#' Get a table on Synapse
#'
#' Downloads a table on Synapse. Either as a dataframe, a query object, or returns
#' the query string by itself.
#'
#' @param synapse_id The Synapse ID of the table.
#' @param columns A character vector of columns to download. Defaults to all columns.
#' @param conditionals A character vector of SQL conditionals.
#' @param limit Upper limit on the number of rows returned.
#' @param offset Number of rows to offset in the SQL query.
#' @param as_query_str If TRUE, returns the query string without executing the query.
#' Defaults to FALSE.
#' @param as_data_frame If TRUE, returns the table as a dataframe. Defaults to TRUE.
#'
#' @export
synGetTable <- function(synapse_id, columns = list(), conditionals = list(),
                        limit = NULL, offset = NULL, as_query_str = FALSE, as_data_frame = TRUE) {
  if (is.null(limit) && !is.null(offset)) {
    stop("When setting an OFFSET in a SQL query, LIMIT must be set too.")
  }
  query_str <-  paste0("SELECT ", ifelse(length(columns), paste(columns, collapse = ", "), "*"),
                       " FROM ", synapse_id, ifelse(length(conditionals), " WHERE ", ""),
                       paste(conditionals, collapse = " AND "),
                       ifelse(is.null(limit), "",  paste0(" LIMIT ", limit)),
                       ifelse(is.null(offset), "",  paste0(" OFFSET ", offset)))
  if (as_query_str) {
    return(query_str)
  }
  table <- synapser::synTableQuery(query_str)
  if (as_data_frame) {
    return(tibble::as_tibble(table$asDataFrame()))
  }
  return(table)
}

#' Get a table and its files
#'
#' Downloads a table on Synapse as a dataframe and adds columns containing
#' local filepaths for the specified columns.
#'
#' @inheritParams synGetTable
#' @param file_columns A character vector of columns which contain file
#' handles that need downloading.
#'
#' @export
synGetTableFiles <- function(synapse_id, file_columns, limit = NULL,
                             offset = NULL, conditionals = list()) {
  q <- synGetTable(synapse_id = synapse_id,
                   columns = list(),
                   limit = limit,
                   offset = offset,
                   conditionals = conditionals,
                   as_data_frame = FALSE)
  df <- tibble::as_tibble(q$asDataFrame())
  file_handles <- synDownloadTableColumns(q, file_columns)
  file_handles_df <- tibble::enframe(file_handles, "filehandle", "path")
  merged_df <- df %>%
    tidyr::pivot_longer(cols = unlist(file_columns), values_to="filehandle") %>%
    dplyr::inner_join(file_handles_df, by = "filehandle") %>%
    tidyr::pivot_wider(names_from=name, values_from = c("filehandle", "path"))
  return(merged_df)
}
