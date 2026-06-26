# API profiles: authentication + pagination strategies --------------------
# An `apifetch_api` object describes *where* to call, *how* to authenticate,
# and *how* to paginate. Auth and pagination are pluggable strategy objects so
# the same fetch verbs work against APIs with different conventions.

# ---- Authentication strategies -------------------------------------------

#' Authentication strategies
#'
#' Constructors describing how a token is attached to a request. Pass the result
#' to the `auth` argument of [af_api()].
#'
#' - `af_auth_raw()`: send the token verbatim in a header (default
#'   `Authorization`). This is what the Big Data PE API expects.
#' - `af_auth_bearer()`: send `"Bearer <token>"` in the `Authorization` header.
#' - `af_auth_header()`: send the token in an arbitrary header (e.g.
#'   `X-API-Key`).
#' - `af_auth_query()`: send the token as a URL query parameter.
#'
#' @param header Header name to use.
#' @param prefix String prepended to the token (bearer scheme).
#' @param param Query-parameter name (query scheme).
#' @return An `apifetch_auth` object.
#' @name af_auth
#' @examples
#' af_auth_raw()
#' af_auth_bearer()
#' af_auth_header("X-API-Key")
#' af_auth_query("api_key")
NULL

.auth <- function(apply) structure(list(apply = apply), class = "apifetch_auth")

#' @rdname af_auth
#' @export
af_auth_raw <- function(header = "Authorization") {
  .auth(function(req, token) {
    h <- stats::setNames(list(token), header)
    do.call(httr2::req_headers, c(list(req), h))
  })
}

#' @rdname af_auth
#' @export
af_auth_bearer <- function(header = "Authorization", prefix = "Bearer ") {
  .auth(function(req, token) {
    h <- stats::setNames(list(paste0(prefix, token)), header)
    do.call(httr2::req_headers, c(list(req), h))
  })
}

#' @rdname af_auth
#' @export
af_auth_header <- function(header = "X-API-Key") {
  .auth(function(req, token) {
    h <- stats::setNames(list(token), header)
    do.call(httr2::req_headers, c(list(req), h))
  })
}

#' @rdname af_auth
#' @export
af_auth_query <- function(param = "api_key") {
  .auth(function(req, token) {
    q <- stats::setNames(list(token), param)
    do.call(httr2::req_url_query, c(list(req), q))
  })
}

# ---- Pagination strategies -----------------------------------------------

#' Pagination strategies
#'
#' Constructors describing how `limit`/`offset` are sent with a request. Pass
#' the result to the `pagination` argument of [af_api()].
#'
#' - `af_paginate_offset()`: send `limit`/`offset` either as HTTP headers
#'   (default, as the Big Data PE API expects) or as URL query parameters.
#' - `af_paginate_none()`: send no pagination parameters.
#'
#' Non-positive or infinite values are omitted from the request.
#'
#' @param where Either `"header"` or `"query"`.
#' @param limit_param,offset_param Parameter names to use.
#' @return An `apifetch_pagination` object.
#' @name af_paginate
#' @examples
#' af_paginate_offset("header")
#' af_paginate_offset("query", limit_param = "per_page")
#' af_paginate_none()
NULL

.pagination <- function(apply) {
  structure(list(apply = apply), class = "apifetch_pagination")
}

#' @rdname af_paginate
#' @export
af_paginate_offset <- function(where = c("header", "query"),
                               limit_param = "limit",
                               offset_param = "offset") {
  where <- match.arg(where)
  .pagination(function(req, limit, offset) {
    vals <- list()
    if (!(is.infinite(limit) || limit <= 0)) vals[[limit_param]] <- limit
    if (!(is.infinite(offset) || offset <= 0)) vals[[offset_param]] <- offset
    if (length(vals) == 0) return(req)
    fn <- if (where == "header") httr2::req_headers else httr2::req_url_query
    do.call(fn, c(list(req), vals))
  })
}

#' @rdname af_paginate
#' @export
af_paginate_none <- function() {
  .pagination(function(req, limit, offset) req)
}

# ---- API profile ----------------------------------------------------------

#' Describe an API endpoint
#'
#' Bundles an endpoint URL with its authentication and pagination strategies and
#' a namespace `service` (used to look up tokens). The resulting object is passed
#' to [af_fetch()] and [af_fetch_all()].
#'
#' @param endpoint The base API URL.
#' @param service Namespace used to look up the token (see [af_get_token()]).
#' @param auth An `apifetch_auth` strategy (see [af_auth]). Default
#'   [af_auth_bearer()].
#' @param pagination An `apifetch_pagination` strategy (see [af_paginate]).
#'   Default [af_paginate_offset()].
#' @param drop_cols Character vector of response columns to drop after parsing
#'   (e.g. a status column). Default none.
#' @param connect_hint Optional extra line shown when a connection error occurs
#'   (e.g. a VPN requirement).
#' @return An `apifetch_api` object.
#' @examples
#' af_api(
#'   endpoint = "https://www.bigdata.pe.gov.br/api/buscar",
#'   service = "BigDataPE",
#'   auth = af_auth_raw(),
#'   pagination = af_paginate_offset("header"),
#'   drop_cols = "Mensagem",
#'   connect_hint = "Ensure you are on the PE Conectado network or VPN."
#' )
#' @export
af_api <- function(endpoint,
                   service = "apifetch",
                   auth = af_auth_bearer(),
                   pagination = af_paginate_offset(),
                   drop_cols = character(0),
                   connect_hint = NULL) {
  if (!is.character(endpoint) || !nzchar(endpoint)) {
    cli::cli_abort("{.arg endpoint} must be a non-empty string.")
  }
  if (!inherits(auth, "apifetch_auth")) {
    cli::cli_abort("{.arg auth} must be an {.cls apifetch_auth} object (see {.fn af_auth_bearer}).")
  }
  if (!inherits(pagination, "apifetch_pagination")) {
    cli::cli_abort("{.arg pagination} must be an {.cls apifetch_pagination} object (see {.fn af_paginate_offset}).")
  }

  structure(
    list(
      endpoint = endpoint,
      service = service,
      auth = auth,
      pagination = pagination,
      drop_cols = drop_cols,
      connect_hint = connect_hint
    ),
    class = "apifetch_api"
  )
}

#' @export
print.apifetch_api <- function(x, ...) {
  cli::cli_h3("<apifetch_api>")
  cli::cli_li("endpoint: {.url {x$endpoint}}")
  cli::cli_li("service: {.val {x$service}}")
  if (length(x$drop_cols)) cli::cli_li("drop_cols: {.val {x$drop_cols}}")
  invisible(x)
}
