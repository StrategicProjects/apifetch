test_that("parse_queries encodes and drops empties", {
  expect_equal(parse_queries("https://x.test", list()), "https://x.test")
  expect_equal(
    parse_queries("https://x.test", list(a = "1", b = "")),
    "https://x.test?a=1"
  )
  expect_match(
    parse_queries("https://x.test", list(`a b` = "c d")),
    "a%20b=c%20d", fixed = TRUE
  )
})

test_that(".safe_as_integer accepts whole doubles and Inf, rejects fractions", {
  expect_identical(apifetch:::.safe_as_integer(50, "x"), 50L)
  expect_identical(apifetch:::.safe_as_integer(Inf, "x"), Inf)
  expect_error(apifetch:::.safe_as_integer(1.5, "x"))
  expect_error(apifetch:::.safe_as_integer("a", "x"))
})

test_that("af_api validates its strategy arguments", {
  expect_error(af_api("https://x.test", auth = "nope"))
  expect_error(af_api("https://x.test", pagination = "nope"))
  expect_error(af_api(""))
})

test_that("af_api stores the configured pieces", {
  api <- af_api(
    "https://x.test",
    service = "S",
    auth = af_auth_raw(),
    pagination = af_paginate_offset("header"),
    drop_cols = "Mensagem"
  )
  expect_s3_class(api, "apifetch_api")
  expect_equal(api$service, "S")
  expect_equal(api$drop_cols, "Mensagem")
})

test_that("pagination strategies attach params correctly", {
  req <- httr2::request("https://x.test")

  hdr <- af_paginate_offset("header")$apply(req, 10L, 5L)
  expect_equal(hdr$headers$limit, 10L)
  expect_equal(hdr$headers$offset, 5L)

  qry <- af_paginate_offset("query")$apply(req, 10L, 0L)
  expect_match(qry$url, "limit=10", fixed = TRUE)

  # Inf / non-positive values are omitted
  none <- af_paginate_offset("header")$apply(req, Inf, 0L)
  expect_null(none$headers$limit)
})

test_that("auth strategies attach the token", {
  # httr2 redacts the default `Authorization` header, so verify the raw/bearer
  # behaviour through non-redacted custom headers.
  req <- httr2::request("https://x.test")
  expect_equal(af_auth_raw("X-Token")$apply(req, "tok")$headers$`X-Token`, "tok")
  expect_equal(af_auth_bearer("X-Token")$apply(req, "tok")$headers$`X-Token`, "Bearer tok")
  expect_equal(af_auth_header("X-API-Key")$apply(req, "tok")$headers$`X-API-Key`, "tok")
  expect_equal(af_auth_query("api_key")$apply(req, "tok")$url, "https://x.test/?api_key=tok")
})

test_that(".safe_as_integer rejects vectors, NULL, NA, -Inf and out-of-range", {
  expect_error(apifetch:::.safe_as_integer(c(1, 2), "x"), "single whole number")
  expect_error(apifetch:::.safe_as_integer(NULL, "x"), "single whole number")
  expect_error(apifetch:::.safe_as_integer(NA_integer_, "x"), "single whole number")
  expect_error(apifetch:::.safe_as_integer(-Inf, "x"), "single whole number")
  expect_error(apifetch:::.safe_as_integer(1e10, "x"), "single whole number")
})

test_that("parse_queries appends to an existing query string", {
  expect_equal(
    parse_queries("https://x.test/api?fmt=json", list(a = "1")),
    "https://x.test/api?fmt=json&a=1"
  )
  expect_equal(parse_queries("https://x.test?", list(a = "1")), "https://x.test?a=1")
})

test_that("parse_queries drops NULL/NA, repeats multi-values, needs names", {
  expect_equal(
    parse_queries("https://x.test", list(a = NA, b = NULL, c = "2")),
    "https://x.test?c=2"
  )
  expect_equal(
    parse_queries("https://x.test", list(a = c("1", "2"))),
    "https://x.test?a=1&a=2"
  )
  expect_error(parse_queries("https://x.test", list("1")), "named list")
})

test_that("af_paginate_none() is flagged as not paged", {
  expect_false(af_paginate_none()$paged)
  expect_true(af_paginate_offset()$paged)
})

# ---- fetching (mocked HTTP, no network) -----------------------------------

json_resp <- function(x, status = 200L) {
  httr2::response(
    status_code = status,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(x)
  )
}

test_that("af_fetch parses JSON and handles empty bodies", {
  svc <- "apifetchFetchTest"
  Sys.setenv(apifetchFetchTest_t = "tok")
  on.exit(Sys.unsetenv("apifetchFetchTest_t"), add = TRUE)
  api <- af_api("https://x.test", service = svc)

  httr2::local_mocked_responses(function(req) json_resp('[{"a":1},{"a":2}]'))
  expect_equal(af_fetch(api, "t")$a, c(1L, 2L))

  httr2::local_mocked_responses(function(req) json_resp("[]"))
  expect_equal(nrow(af_fetch(api, "t")), 0L)

  httr2::local_mocked_responses(function(req) httr2::response(status_code = 204L))
  expect_equal(nrow(af_fetch(api, "t")), 0L)
})

test_that("af_fetch gives friendly errors", {
  svc <- "apifetchFetchTest"
  Sys.setenv(apifetchFetchTest_t = "tok")
  on.exit(Sys.unsetenv("apifetchFetchTest_t"), add = TRUE)
  api <- af_api("https://x.test", service = svc)

  httr2::local_mocked_responses(function(req) json_resp("{}", status = 401L))
  expect_error(af_fetch(api, "t"), "token")

  httr2::local_mocked_responses(function(req) {
    httr2::response(headers = list(`Content-Type` = "text/html"),
                    body = charToRaw("<html>login</html>"))
  })
  expect_error(af_fetch(api, "t"), "parsed as JSON")
})

test_that("af_fetch_all pages until an empty chunk and drops columns", {
  svc <- "apifetchFetchTest"
  Sys.setenv(apifetchFetchTest_t = "tok")
  on.exit(Sys.unsetenv("apifetchFetchTest_t"), add = TRUE)
  api <- af_api("https://x.test", service = svc,
                pagination = af_paginate_offset("query"), drop_cols = "Mensagem")
  data <- seq_len(5)

  httr2::local_mocked_responses(function(req) {
    q <- httr2::url_parse(req$url)$query
    off <- if (is.null(q$offset)) 0L else as.integer(q$offset)
    lim <- as.integer(q$limit)
    ids <- data[data > off][seq_len(max(0, min(lim, length(data) - off)))]
    rows <- vapply(ids, function(i) sprintf('{"id":%d,"Mensagem":"ok"}', i), "")
    json_resp(paste0("[", paste(rows, collapse = ","), "]"))
  })

  res <- af_fetch_all(api, "t", chunk_size = 2)
  expect_equal(res$id, 1:5)
  expect_false("Mensagem" %in% names(res))

  expect_equal(af_fetch_all(api, "t", total_limit = 3, chunk_size = 2)$id, 1:3)
})

test_that("af_fetch_all makes one request with af_paginate_none()", {
  svc <- "apifetchFetchTest"
  Sys.setenv(apifetchFetchTest_t = "tok")
  on.exit(Sys.unsetenv("apifetchFetchTest_t"), add = TRUE)
  api <- af_api("https://x.test", service = svc, pagination = af_paginate_none())
  calls <- 0L
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1L
    json_resp('[{"a":1},{"a":2},{"a":3}]')
  })

  expect_equal(nrow(af_fetch_all(api, "t")), 3L)
  expect_equal(calls, 1L)
  expect_equal(nrow(af_fetch_all(api, "t", total_limit = 2)), 2L)
})

test_that("af_fetch_all accepts chunk_size = Inf", {
  svc <- "apifetchFetchTest"
  Sys.setenv(apifetchFetchTest_t = "tok")
  on.exit(Sys.unsetenv("apifetchFetchTest_t"), add = TRUE)
  api <- af_api("https://x.test", service = svc, pagination = af_paginate_offset("query"))
  httr2::local_mocked_responses(function(req) {
    off <- httr2::url_parse(req$url)$query$offset
    json_resp(if (is.null(off)) '[{"a":1},{"a":2}]' else "[]")
  })
  expect_equal(nrow(af_fetch_all(api, "t", chunk_size = Inf)), 2L)
})
