ms_py_tdag <- function(convert = FALSE) {
  reticulate::import("mainsequence.client.models_tdag", convert = convert, delay_load = TRUE)
}

ms_py_vam <- function(convert = FALSE) {
  reticulate::import("mainsequence.client.models_vam", convert = convert, delay_load = TRUE)
}

ms_py_logconf <- function(convert = FALSE) {
  reticulate::import("mainsequence.logconf", convert = convert, delay_load = TRUE)
}
