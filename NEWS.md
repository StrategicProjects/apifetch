# apifetch 0.2.0

## Bug fixes

* `af_fetch_all()` no longer loops forever with `af_paginate_none()`: an API
  that cannot be paged is now fetched with a single request.
* `af_fetch_all()` now accepts `chunk_size = Inf` (previously failed with a
  "missing value" error), and truncates to `total_limit` when an API returns
  more rows than requested.
* `parse_queries()` (and therefore the `query` argument of `af_fetch()`)
  appends to an endpoint that already has a query string instead of producing
  a second `?`; drops `NULL` and `NA` values instead of sending `a=` or
  `a=NA`; repeats a key for multi-valued parameters; and errors clearly on an
  unnamed list.
* Integer-like arguments (`limit`, `offset`, `chunk_size`, ...) now reject
  vectors, `NULL`, `NA`, `-Inf` and values beyond the integer range with a
  clear message, instead of silently becoming `NA` or failing cryptically.
* Token names with accents are sanitized to plain ASCII on every platform
  (macOS previously produced names such as `Sa'ude`).
* `af_list_tokens()` matches the `service` prefix literally rather than as a
  regular expression, and skips empty variables.

## Improvements

* `af_store_token()` gains `overwrite = FALSE`, to rotate an expired token.
* `af_fetch()` returns an empty tibble for responses without a body (e.g.
  HTTP 204), gives a token-specific hint on HTTP 401/403, and a clear error
  when the response is not JSON (e.g. an HTML login or captive-portal page).
* Added mocked-HTTP tests for `af_fetch()` and `af_fetch_all()`.
* Requires 'httr2' >= 1.0.0.

# apifetch 0.1.0

* Initial CRAN release.
* A generic, dependency-light toolkit for token-authenticated REST APIs,
  generalising the engine first developed in the 'BigDataPE' package.
* Token management in process environment variables, namespaced per service:
  `af_store_token()`, `af_get_token()`, `af_remove_token()`, `af_list_tokens()`.
* API profiles via `af_api()`, with pluggable authentication strategies
  (`af_auth_raw()`, `af_auth_bearer()`, `af_auth_header()`, `af_auth_query()`)
  and pagination strategies (`af_paginate_offset()`, `af_paginate_none()`).
* Data retrieval with `af_fetch()` (single page) and `af_fetch_all()`
  (chunked, combined into one tibble), built on `httr2`.
* All user-facing output goes through the `cli` package.
* Includes a vignette showing the Big Data PE platform as a worked use case.
