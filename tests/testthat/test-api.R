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
