#' Validate MainSequence environment variables
#' @export
ms_check_env <- function() {
  vars <- c("TDAG_ENDPOINT", "MAINSEQUENCE_TOKEN", "VFB_PROJECT_PATH")
  missing <- vars[!nzchar(Sys.getenv(vars))]
  if (length(missing)) {
    stop("Missing env vars: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}
