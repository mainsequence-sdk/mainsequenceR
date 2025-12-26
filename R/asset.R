# Internal: convert a Python Asset/AssetMixin to a simple R list
ms_asset_to_list <- function(a) {
  list(
    id                    = reticulate::py_to_r(a$id),
    unique_identifier     = reticulate::py_to_r(a$unique_identifier),
    figi                  = reticulate::py_to_r(a$figi),
    composite             = reticulate::py_to_r(a$composite),
    share_class           = reticulate::py_to_r(a$share_class),
    isin                  = reticulate::py_to_r(a$isin),
    security_type         = reticulate::py_to_r(a$security_type),
    security_type_2       = reticulate::py_to_r(a$security_type_2),
    security_market_sector= reticulate::py_to_r(a$security_market_sector),
    is_custom_by_org      = reticulate::py_to_r(a$is_custom_by_organization),
    ticker                = reticulate::py_to_r(a$ticker %||% NULL),
    name                  = reticulate::py_to_r(a$name %||% NULL),
    exchange_code         = reticulate::py_to_r(a$exchange_code %||% NULL),
    asset_ticker_group_id = reticulate::py_to_r(a$asset_ticker_group_id %||% NULL)
  )
}

#' Get a single asset
#'
#' Thin wrapper over Asset.get().
#'
#' @param id Optional integer asset id.
#' @param unique_identifier Optional unique_identifier.
#' @param figi Optional FIGI.
#' @param isin Optional ISIN (can be combined with exchange_code).
#' @param ticker Optional ticker (translated via current_snapshot).
#' @param exchange_code Optional exchange code (MIC or composite).
#' @return A Python Asset (or subclass) instance.
#' @export
ms_asset_get <- function(
  id = NULL,
  unique_identifier = NULL,
  figi = NULL,
  isin = NULL,
  ticker = NULL,
  exchange_code = NULL
) {
  vam <- ms_py_vam(convert = FALSE)
  Asset <- vam$Asset

  kwargs <- list()
  if (!is.null(id))                kwargs$id <- as.integer(id)
  if (!is.null(unique_identifier)) kwargs$unique_identifier <- unique_identifier
  if (!is.null(figi))              kwargs$figi <- figi
  if (!is.null(isin))              kwargs$isin <- isin
  if (!is.null(ticker))            kwargs$ticker <- ticker
  if (!is.null(exchange_code))     kwargs$exchange_code <- exchange_code

  if (!length(kwargs)) {
    stop("Provide at least one filter (id, unique_identifier, figi, isin, ticker/exchange_code).",
         call. = FALSE)
  }

  asset <- do.call(Asset$get, kwargs)

  ms_log_info(
    "Fetched asset id=%s uid=%s",
    reticulate::py_to_r(asset$id),
    reticulate::py_to_r(asset$unique_identifier),
    logger = ms_logger("r-asset")
  )

  asset
}

#' Search assets
#'
#' Uses Asset.filter_with_asset_class() so subclasses (e.g. futures, FX)
#' come back as the right Python classes.
#'
#' @param ticker,name,exchange_code,asset_ticker_group_id,security_type,
#'        security_market_sector Optional filter fields.
#' @param limit Max number of results to fetch per call.
#' @param as "data.frame" (default) or "python".
#'
#' @export
ms_assets_search <- function(
  ticker = NULL,
  name   = NULL,
  exchange_code = NULL,
  asset_ticker_group_id = NULL,
  security_type = NULL,
  security_market_sector = NULL,
  limit = 500L,
  as = c("data.frame", "python")
) {
  as <- match.arg(as)

  vam <- ms_py_vam(convert = FALSE)
  Asset <- vam$Asset

  kwargs <- list()
  if (!is.null(ticker))                kwargs$ticker <- ticker
  if (!is.null(name))                  kwargs$name <- name
  if (!is.null(exchange_code))         kwargs$exchange_code <- exchange_code
  if (!is.null(asset_ticker_group_id)) kwargs$asset_ticker_group_id <- asset_ticker_group_id
  if (!is.null(security_type))         kwargs$security_type <- security_type
  if (!is.null(security_market_sector))kwargs$security_market_sector <- security_market_sector

  py_list <- do.call(
    Asset$filter_with_asset_class,
    c(kwargs, list(limit = as.integer(limit), timeout = NULL))
  )

  if (as == "python") {
    return(py_list)
  }

  if (!length(py_list)) {
    return(data.frame())
  }

  rows <- lapply(py_list, ms_asset_to_list)
  df <- as.data.frame(
    do.call(rbind, lapply(rows, function(x) {
      vapply(
        x,
        function(v) if (is.null(v)) NA_character_ else as.character(v),
        FUN.VALUE = character(1),
        USE.NAMES = TRUE
      )
    })),
    stringsAsFactors = FALSE
  )
  df
}

#' Register or fetch an asset by FIGI
#'
#' @export
ms_asset_register_figi <- function(figi, timeout = NULL) {
  vam <- ms_py_vam(convert = FALSE)
  Asset <- vam$Asset
  Asset$register_asset_from_figi(figi = figi, timeout = timeout)
}

#' Register or fetch an asset by ISIN + exchange_code
#'
#' @export
ms_asset_register_isin <- function(isin, exchange_code, timeout = NULL) {
  vam <- ms_py_vam(convert = FALSE)
  Asset <- vam$Asset
  Asset$get_or_register_from_isin(
    isin         = isin,
    exchange_code = exchange_code,
    timeout      = timeout
  )
}

#' Register or fetch a custom asset
#'
#' @param ... Named fields accepted by Asset.get_or_register_custom_asset().
#'
#' @export
ms_asset_register_custom <- function(..., timeout = NULL) {
  vam <- ms_py_vam(convert = FALSE)
  Asset <- vam$Asset
  kwargs <- list(...)
  kwargss$timeout <- timeout
  do.call(Asset$get_or_register_custom_asset, kwargs)
}
