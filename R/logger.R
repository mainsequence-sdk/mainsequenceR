.ms_py_logger <- NULL

# Internal: fetch and cache the Python logger
ms_py_logger <- function() {
  if (!is.null(.ms_py_logger)) {
    return(.ms_py_logger)
  }

  out <- tryCatch({
    logconf <- ms_py_logconf(convert = FALSE)
    .ms_py_logger <<- logconf$logger
    .ms_py_logger
  }, error = function(e) NULL )
    stop("Failed to initialize MainSequence logger: ", e$message)
  
  .ms_py_logger <<- out
  out
}

#' Create a MainSequence logger bound to a sub-application
#'
#' This is a lightweight R wrapper over the Python logger from
#' `mainsequence.logconf`. It calls `logger.bind(sub_application=...)`
#' in Python and stores that bound logger in the R object.
#'
#' @param sub_application label, e.g. "r-client", "r-datanode", ...
#' @export
ms_logger <- function(sub_application = "r-client") {
  base_log <- ms_py_logger()
  bound    <- base_log$bind(sub_application = sub_application)

  structure(
    list(
      py  = bound,
      sub = sub_application
    ),
    class = "ms_logger"
  )
}

# Internal helper to normalize logger argument
.ms_get_logger <- function(logger = NULL) {
  if (inherits(logger, "ms_logger")) {
    logger$py
  } else {
    ms_logger()$py
  }
}

#' Log DEBUG via the Python logger
#' @export
ms_log_debug <- function(msg, ..., logger = NULL, .also_r = FALSE) {
  text <- sprintf(msg, ...)
  py_log <- .ms_get_logger(logger)
  py_log$debug(text)
  if (.also_r) cli::cli_inform(text)
  invisible(NULL)
}

#' Log INFO via the Python logger
#' @export
ms_log_info <- function(msg, ..., logger = NULL, .also_r = TRUE) {
  text <- sprintf(msg, ...)
  py_log <- .ms_get_logger(logger)
  py_log$info(text)
  if (.also_r) cli::cli_inform(text)
  invisible(NULL)
}

#' Log WARNING via the Python logger
#' @export
ms_log_warn <- function(msg, ..., logger = NULL, .also_r = TRUE) {
  text <- sprintf(msg, ...)
  py_log <- .ms_get_logger(logger)
  py_log$warning(text)
  if (.also_r) cli::cli_warn(text)
  invisible(NULL)
}

#' Log ERROR via the Python logger
#' @export
ms_log_error <- function(msg, ..., logger = NULL, .also_r = TRUE) {
  text <- sprintf(msg, ...)
  py_log <- .ms_get_logger(logger)
  py_log$error(text)
  if (.also_r) cli::cli_abort(text)
  invisible(NULL)
}
