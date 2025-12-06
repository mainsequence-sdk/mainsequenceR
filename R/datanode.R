#' Get a DataNodeStorage Python object by storage_hash
#'
#' @param storage_hash character
#' @return A reticulate Python object (DataNodeStorage instance).
#' @export
ms_data_node_storage <- function(storage_hash) {
  models <- ms_py_tdag(convert = FALSE)
  DataNodeStorage <- models$DataNodeStorage

  node <- DataNodeStorage$get(storage_hash = storage_hash)

  ms_log_debug(
    "Loaded DataNodeStorage(storage_hash=%s, id=%s)",
    storage_hash,
    reticulate::py_to_r(node$id),
    logger = ms_logger("r-datanode")
  )

  node
}

#' Read data for a DataNodeStorage
#'
#' Thin wrapper around the Python method.
#'
#' @param storage_hash character, or a Python DataNodeStorage object.
#' @param start,end optional POSIXct or character dates.
#' @param ids optional character vector of unique_identifier values.
#' @param columns optional character vector of columns.
#' @param as return type: "data.frame" or "pandas".
#' @export
ms_data_node_read <- function(
  storage_hash,
  start   = NULL,
  end     = NULL,
  ids     = NULL,
  columns = NULL,
  as = c("data.frame", "pandas")
) {
  as <- match.arg(as)
  log <- ms_logger("r-datanode")

  node <- if (inherits(storage_hash, "python.builtin.object")) {
    storage_hash
  } else {
    ms_data_node_storage(storage_hash)
  }

  if (!is.null(start)) start <- as.POSIXct(start, tz = "UTC")
  if (!is.null(end))   end   <- as.POSIXct(end,   tz = "UTC")

  ms_log_info(
    "Reading data node id=%s (storage_hash=%s)",
    reticulate::py_to_r(node$id),
    reticulate::py_to_r(node$storage_hash),
    logger = log
  )

  py_df <- node$get_data_between_dates_from_api(
    start_date = start,
    end_date   = end,
    unique_identifier_list = ids,
    columns = columns,
    great_or_equal = TRUE,
    less_or_equal  = TRUE
  )

  if (as == "pandas") {
    return(py_df)
  }

  reticulate::py_to_r(py_df)
}

#' Get last observation for one or more identifiers
#'
#' @param storage_hash DataNodeStorage object or storage_hash.
#' @param ids Character vector of unique_identifier values.
#' @param as "data.frame" or "pandas".
#' @export
ms_data_node_last_observation <- function(
  storage_hash,
  ids,
  as = c("data.frame", "pandas")
) {
  as <- match.arg(as)
  node <- if (inherits(storage_hash, "python.builtin.object")) {
    storage_hash
  } else {
    ms_data_node_storage(storage_hash)
  }

  py_df <- node$get_last_observation(unique_identifier_list = as.list(ids))

  if (as == "pandas") {
    return(py_df)
  }

  reticulate::py_to_r(py_df)
}
