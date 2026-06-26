# Internal utilities -------------------------------------------------------

# Safe coercion to integer.
# Tries to convert a scalar value to integer. If conversion is not possible
# (e.g. character, logical, NA, or a numeric with fractional part) an
# informative error is raised via cli.
# Values that are already integer or Inf pass through unchanged.
.safe_as_integer <- function(x, arg_name) {
  if (is.infinite(x)) return(x)
  if (is.integer(x)) return(x)
  if (is.numeric(x) && length(x) == 1L && !is.na(x) && x == trunc(x)) {
    return(as.integer(x))
  }
  cli::cli_abort(
    "{.arg {arg_name}} must be a whole number or {.val Inf}, not {.cls {class(x)}} ({.val {x}})."
  )
}

# Sanitize a name into the suffix used for environment-variable token storage.
# Transliterates to ASCII (dropping accents) and turns spaces into underscores,
# matching the contract shared by all token functions.
.sanitize_name <- function(x) {
  x_ascii <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  gsub(" ", "_", x_ascii)
}

# Build the environment-variable name for a token: "<service>_<name>".
.token_var <- function(name, service) {
  paste0(.sanitize_name(service), "_", .sanitize_name(name))
}

#' Build a URL with query parameters
#'
#' Appends a named list of query parameters to a base URL, URL-encoding both
#' names and values and dropping parameters whose value is the empty string.
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

  query_list <- query_list[query_list != ""]

  if (length(query_list) == 0) {
    return(url)
  }

  query_string <- paste(
    vapply(names(query_list), function(name) {
      paste0(
        utils::URLencode(name, reserved = TRUE), "=",
        utils::URLencode(as.character(query_list[[name]]), reserved = TRUE)
      )
    }, character(1)),
    collapse = "&"
  )

  paste0(url, "?", query_string)
}
