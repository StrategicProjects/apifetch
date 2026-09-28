test_that("store / get / list / remove round-trip works", {
  svc <- "apifetchTest"
  on.exit(suppressMessages(af_remove_token("alpha", service = svc)), add = TRUE)

  suppressMessages(af_store_token("alpha", "tok-123", service = svc))
  expect_equal(af_get_token("alpha", service = svc), "tok-123")
  expect_true("alpha" %in% af_list_tokens(service = svc))

  suppressMessages(af_remove_token("alpha", service = svc))
  expect_null(suppressMessages(af_get_token("alpha", service = svc)))
})

test_that("store refuses to overwrite an existing token", {
  svc <- "apifetchTest"
  on.exit(suppressMessages(af_remove_token("beta", service = svc)), add = TRUE)

  suppressMessages(af_store_token("beta", "first", service = svc))
  suppressMessages(af_store_token("beta", "second", service = svc)) # no overwrite
  expect_equal(af_get_token("beta", service = svc), "first")
})

test_that("names with accents and spaces are sanitized consistently", {
  svc <- "apifetchTest"
  on.exit(suppressMessages(af_remove_token("São Paulo", service = svc)), add = TRUE)

  suppressMessages(af_store_token("São Paulo", "tok", service = svc))
  expect_equal(af_get_token("São Paulo", service = svc), "tok")
})

test_that("get / remove inform (not error) when token is missing", {
  expect_message(res <- af_get_token("missing", service = "nope"))
  expect_null(res)
  expect_message(af_remove_token("missing", service = "nope"))
})

test_that("invalid inputs error", {
  expect_error(af_store_token("", "tok"))
  expect_error(af_store_token("x", ""))
  expect_error(af_get_token(""))
})

test_that("store can overwrite when asked", {
  svc <- "apifetchTest"
  on.exit(suppressMessages(af_remove_token("gamma", service = svc)), add = TRUE)

  suppressMessages(af_store_token("gamma", "first", service = svc))
  suppressMessages(af_store_token("gamma", "second", service = svc, overwrite = TRUE))
  expect_equal(af_get_token("gamma", service = svc), "second")
})

test_that("sanitized names are plain ASCII on every platform", {
  expect_equal(apifetch:::.sanitize_name("Saúde pública"), "Saude_publica")
})

test_that("af_list_tokens treats the service literally, not as a regex", {
  on.exit(Sys.unsetenv("aXb_tok"), add = TRUE)
  Sys.setenv(aXb_tok = "x")
  expect_false("tok" %in% suppressMessages(af_list_tokens(service = "a.b")))
})
