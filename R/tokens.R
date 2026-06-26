# Token management via environment variables -------------------------------
# Tokens are never written to disk; they live only in process environment
# variables named "<service>_<name>". The `service` acts as a namespace so a
# single R session can hold tokens for several different APIs without clashing.

#' Store an API token in an environment variable
#'
#' Stores an authentication token in a process environment variable named
#' `"<service>_<name>"`. The token is never written to disk. If a non-empty
#' variable with that name already exists, the function refuses to overwrite it.
#'
#' @param name The identifier for this token (e.g. a dataset or resource name).
#' @param token The authentication token (character).
#' @param service A namespace prefix grouping tokens for one API. Default
#'   `"apifetch"`.
#' @return Invisibly `NULL`; called for its side effect.
#' @examples
#' bdpe <- af_store_token("dengue", "your-token-here", service = "BigDataPE")
#' @export
af_store_token <- function(name, token, service = "apifetch") {
  if (!is.character(name) || !nzchar(name)) {
    cli::cli_abort("{.arg name} must be a non-empty string.")
  }
  if (!is.character(token) || !nzchar(token)) {
    cli::cli_abort("{.arg token} must be a non-empty string.")
  }

  env_var_name <- .token_var(name, service)

  if (nzchar(Sys.getenv(env_var_name, unset = ""))) {
    cli::cli_alert_warning(
      "The environment variable {.envvar {env_var_name}} is already defined. Not overwriting to avoid data loss."
    )
    return(invisible())
  }

  env_list <- list(token)
  names(env_list) <- env_var_name
  do.call(Sys.setenv, env_list)

  cli::cli_alert_success("Token stored in environment variable: {.envvar {env_var_name}}")
  invisible()
}

#' Retrieve a stored API token
#'
#' Retrieves the token stored for `name` under `service`. Returns `NULL`
#' (with a warning) rather than erroring when no token is found.
#'
#' @inheritParams af_store_token
#' @return The token string, or `NULL` if not found.
#' @examples
#' token <- af_get_token("dengue", service = "BigDataPE")
#' @export
af_get_token <- function(name, service = "apifetch") {
  if (!is.character(name) || !nzchar(name)) {
    cli::cli_abort("{.arg name} must be a non-empty string.")
  }

  env_var_name <- .token_var(name, service)
  token <- Sys.getenv(env_var_name, unset = "")

  if (token == "") {
    cli::cli_alert_warning("No token found for {.val {name}} (service {.val {service}}).")
    return(NULL)
  }

  token
}

#' Remove a stored API token
#'
#' Removes the token stored for `name` under `service`. Does nothing (beyond a
#' warning) when no token is found.
#'
#' @inheritParams af_store_token
#' @return Invisibly `NULL`; called for its side effect.
#' @examples
#' af_remove_token("dengue", service = "BigDataPE")
#' @export
af_remove_token <- function(name, service = "apifetch") {
  if (!is.character(name) || !nzchar(name)) {
    cli::cli_abort("{.arg name} must be a non-empty string.")
  }

  env_var_name <- .token_var(name, service)

  if (nzchar(Sys.getenv(env_var_name, unset = ""))) {
    Sys.unsetenv(env_var_name)
    cli::cli_alert_success("Token removed for {.val {name}} (service {.val {service}}).")
  } else {
    cli::cli_alert_warning("No token found for {.val {name}} (service {.val {service}}).")
  }
  invisible()
}

#' List stored API tokens
#'
#' Returns the names (without the `service` prefix) of all tokens stored for a
#' given `service` in environment variables.
#'
#' @param service A namespace prefix grouping tokens for one API. Default
#'   `"apifetch"`.
#' @return A character vector of token names, empty if none are found.
#' @examples
#' af_list_tokens(service = "BigDataPE")
#' @export
af_list_tokens <- function(service = "apifetch") {
  prefix <- paste0("^", .sanitize_name(service), "_")
  all_envs <- Sys.getenv()
  stored <- grep(prefix, names(all_envs), value = TRUE)

  if (length(stored) == 0) {
    cli::cli_alert_info("No tokens found for service {.val {service}}.")
    return(character(0))
  }

  sub(prefix, "", stored)
}
