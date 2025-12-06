.onLoad <- function(libname, pkgname) {
  python <- Sys.getenv("RETICULATE_PYTHON", unset = "")
  if (nzchar(python)) {
    reticulate::use_python(python, required = FALSE)
  }
}

`%||%` <- function(x, y) if (is.null(x)) y else x
