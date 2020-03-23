#' Query an Evaluation object
#'
#' Fetches all key/value pairs associated with the submissions objects in an
#' evaluation. Includes both annotations and default Evaluation values such as
#' name, submitterId, objectId, etc.
#'
#' @param evaluation_id The ID of the evaluation object.
#' @param as_data_frame Return results as a dataframe. If FALSE, returns the
#' object returned by the API service `GET /evaluation/submission/query`.
#' @return See the parameter description for `as_data_frame`.
#' @export
evaluationQuery <- function(evaluation_id, as_data_frame = TRUE) {
  query_str <- glue::glue("SELECT * FROM evaluation_{evaluation_id}")
  q <- synRestGET("/evaluation/submission/query", params=list("query" = query_str))
  if (as_data_frame) {
    column_names <- unlist(q$headers)
    row_values <- purrr::map(q$rows, ~ .$values)
    transposed_values <- purrr::transpose(row_values)
    transposed_values_no_null <- purrr::map(transposed_values, function(l) {
      purrr::map(l, function(i) {
        ifelse(is.null(i), NA, i)
      })
    })
    unlisted_values <- purrr::map(transposed_values_no_null, unlist)
    names(unlisted_values) <- column_names
    df <- tibble::as_tibble(unlisted_values)
    return(df)
  } else {
    return(q)
  }
}
