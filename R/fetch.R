# Data fetching -------------------------------------------------------------

#' Fetch a single page from an API
#'
#' Performs one authenticated request against an [af_api()] profile, applying its
#' pagination strategy, and returns the parsed body as a tibble. HTTP errors and
#' connection failures are translated into friendly `cli` messages.
#'
#' @param api An `apifetch_api` object (see [af_api()]).
#' @param name The token name to authenticate with (looked up via the API's
#'   `service`).
#' @param limit Maximum number of records to request. Default `Inf` (no limit).
#'   Non-positive or infinite values omit the parameter.
#' @param offset Starting record. Default `0` (omitted).
#' @param query A named list of additional query-string filters. Default empty.
#' @param verbosity `0` (silent, default), `1` (progress messages), or `2`
#'   (progress plus full HTTP request/response details).
#' @return A tibble with the parsed response.
#' @examples
#' \dontrun{
#' api <- af_api("https://www.bigdata.pe.gov.br/api/buscar",
#'               service = "BigDataPE", auth = af_auth_raw(),
#'               pagination = af_paginate_offset("header"))
#' af_store_token("dengue", "token", service = "BigDataPE")
#' af_fetch(api, "dengue", limit = 50)
#' }
#' @export
af_fetch <- function(api, name, limit = Inf, offset = 0L, query = list(),
                     verbosity = 0L) {
  if (!inherits(api, "apifetch_api")) {
    cli::cli_abort("{.arg api} must be an {.cls apifetch_api} object (see {.fn af_api}).")
  }
  if (!is.list(query)) {
    cli::cli_abort("{.arg query} must be a {.cls list}.")
  }

  token <- af_get_token(name, service = api$service)
  if (is.null(token)) {
    cli::cli_abort("No token available for {.val {name}}; store one with {.fn af_store_token}.")
  }

  offset    <- .safe_as_integer(offset, "offset")
  limit     <- .safe_as_integer(limit, "limit")
  verbosity <- .safe_as_integer(verbosity, "verbosity")

  req <- api$endpoint |>
    parse_queries(query_list = query) |>
    httr2::request() |>
    httr2::req_error(is_error = function(resp) FALSE)

  req <- api$auth$apply(req, token)
  req <- api$pagination$apply(req, limit, offset)

  httr2_verbosity <- if (verbosity >= 2L) 1L else 0L

  resp <- tryCatch(
    httr2::req_perform(req, verbosity = httr2_verbosity),
    error = function(e) {
      msg <- c(
        "Unable to connect to the API at {.url {api$endpoint}}.",
        "i" = "Check your network connection."
      )
      if (!is.null(api$connect_hint)) msg <- c(msg, "i" = api$connect_hint)
      msg <- c(msg, "x" = conditionMessage(e))
      cli::cli_abort(msg)
    }
  )

  status <- httr2::resp_status(resp)
  if (status >= 400L) {
    reason <- httr2::resp_status_desc(resp)
    cli::cli_abort(c(
      "The API returned an error (HTTP {status} - {reason}).",
      "i" = "Try again later, and check that the endpoint is correct and your token is valid."
    ))
  }

  resp |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    tibble::as_tibble()
}

#' Fetch all data from an API in chunks
#'
#' Iteratively calls [af_fetch()] with an advancing `offset`, stopping when a
#' chunk comes back empty or `total_limit` is reached, then row-binds the chunks
#' into one tibble. Columns listed in the API profile's `drop_cols` are removed.
#'
#' @inheritParams af_fetch
#' @param total_limit Maximum number of records to retrieve in total. Default
#'   `Inf` (all available).
#' @param chunk_size Records to request per chunk. Default `50000`.
#' @return A tibble with all retrieved records.
#' @examples
#' \dontrun{
#' api <- af_api("https://www.bigdata.pe.gov.br/api/buscar",
#'               service = "BigDataPE", auth = af_auth_raw(),
#'               pagination = af_paginate_offset("header"),
#'               drop_cols = "Mensagem")
#' af_fetch_all(api, "dengue", total_limit = 500, chunk_size = 100)
#' }
#' @export
af_fetch_all <- function(api, name, total_limit = Inf, chunk_size = 50000L,
                         query = list(), verbosity = 0L) {
  if (!inherits(api, "apifetch_api")) {
    cli::cli_abort("{.arg api} must be an {.cls apifetch_api} object (see {.fn af_api}).")
  }
  if (!is.list(query)) {
    cli::cli_abort("{.arg query} must be a {.cls list}.")
  }

  total_limit <- .safe_as_integer(total_limit, "total_limit")
  chunk_size  <- .safe_as_integer(chunk_size, "chunk_size")
  verbosity   <- .safe_as_integer(verbosity, "verbosity")
  if (!is.infinite(chunk_size) && chunk_size <= 0) {
    cli::cli_abort("{.arg chunk_size} must be a positive whole number.")
  }

  offset <- 0L
  total_fetched <- 0L
  all_data <- list()

  repeat {
    current_limit <- as.integer(min(chunk_size, total_limit - total_fetched))
    if (current_limit <= 0) break

    chunk <- af_fetch(
      api = api,
      name = name,
      limit = current_limit,
      offset = offset,
      query = query,
      verbosity = verbosity
    )

    drop <- intersect(api$drop_cols, names(chunk))
    if (length(drop)) chunk <- dplyr::select(chunk, -dplyr::all_of(drop))

    if (nrow(chunk) == 0L) break

    all_data <- append(all_data, list(chunk))
    total_fetched <- as.integer(total_fetched) + nrow(chunk)

    if (verbosity > 0L) {
      cli::cli_alert_info("Fetched {nrow(chunk)} records (total: {total_fetched}).")
    }

    offset <- as.integer(offset) + nrow(chunk)
    if (total_fetched >= total_limit) break
  }

  combined <- dplyr::bind_rows(all_data)

  if (verbosity > 0L) {
    cli::cli_alert_success("Fetching complete: {nrow(combined)} records retrieved.")
  }

  combined
}
