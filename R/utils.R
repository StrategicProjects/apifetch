# Internal utilities -------------------------------------------------------

# Safe coercion to integer.
# Tries to convert a scalar value to integer. If conversion is not possible
# (e.g. character, logical, NA, or a numeric with fractional part) an
# informative error is raised via cli.
# A positive `Inf` passes through unchanged; whole numbers outside the integer
# range are rejected rather than silently becoming NA.
.safe_as_integer <- function(x, arg_name) {
  if (is.numeric(x) && length(x) == 1L && !is.na(x)) {
    if (identical(x, Inf)) return(x)
    if (is.finite(x) && x == trunc(x) && abs(x) <= .Machine$integer.max) {
      return(as.integer(x))
    }
  }
  cli::cli_abort(
    "{.arg {arg_name}} must be a single whole number or {.val Inf}, not {.cls {class(x)}} ({.val {x}})."
  )
}

# Sanitize a name into the suffix used for environment-variable token storage.
# Transliterates to ASCII (dropping accents) and turns spaces into underscores,
# matching the contract shared by all token functions. Some iconv
# implementations (e.g. macOS libiconv) render accents as separate marks
# ("é" -> "'e"), so those marks are stripped for a platform-independent result.
.sanitize_name <- function(x) {
  x_ascii <- iconv(enc2utf8(x), from = "UTF-8", to = "ASCII//TRANSLIT")
  x_ascii <- gsub("['`^~\"]", "", x_ascii)
  gsub(" ", "_", x_ascii)
}

# Build the environment-variable name for a token: "<service>_<name>".
.token_var <- function(name, service) {
  paste0(.sanitize_name(service), "_", .sanitize_name(name))
}

#' Build a URL with query parameters
#'
#' Appends a named list of query parameters to a base URL, URL-encoding both
#' names and values. Parameters whose value is `NULL`, `NA` or the empty string
#' are dropped; a parameter with several values is repeated (`a=1&a=2`). If
#' `url` already has a query string, the new parameters are appended to it.
#'
#' @param url The base URL.
#' @param query_list A named list of query parameters.
#' @return The URL with the query string appended (or the base URL unchanged
#'   when there are no parameters to add).
#' @examples
#' parse_queries("https://example.com", list(a = "1", b = "2"))
#' @export
parse_queries <- function(url, query_list) {
  if (length(query_list) == 0) {
    return(url)
  }

  nms <- names(query_list)
  if (is.null(nms) || anyNA(nms) || !all(nzchar(nms))) {
    cli::cli_abort("{.arg query_list} must be a named list.")
  }

  pairs <- unlist(lapply(nms, function(name) {
    vals <- as.character(unlist(query_list[[name]], use.names = FALSE))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) == 0) return(character(0))
    paste0(
      utils::URLencode(name, reserved = TRUE), "=",
      vapply(vals, utils::URLencode, character(1), reserved = TRUE,
             USE.NAMES = FALSE)
    )
  }))

  if (length(pairs) == 0) {
    return(url)
  }

  sep <- if (grepl("[?&]$", url)) "" else if (grepl("?", url, fixed = TRUE)) "&" else "?"
  paste0(url, sep, paste(pairs, collapse = "&"))
}
