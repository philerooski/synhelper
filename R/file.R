#' Defines some commonly used parameters
#'
#' @param synapse_id The Synapse ID of the file.
.parameter_definitions <- function() {}

#' Gets a file on Synapse.
#'
#' @inheritParams .parameter_definitions
#' @param reader A function which accepts a file path as its first argument.
#' Used to read the file into a data.frame, tibble, etc. Default is to assume
#' the file is a CSV file and to return it as a tibble.
#' @param ... Additional arguments to be passed to synapser::synGet
#'
#' @export
synGetFile <- function(synapse_id, reader = readr::read_csv, ...) {
  f <- synapser::synGet(synapse_id, ...)
  df <- reader(f$path)
  return(df)
}
