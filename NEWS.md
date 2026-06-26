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
