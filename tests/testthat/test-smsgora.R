body <- '{"event":"sms.received"}'
good <- "sha256=313fe9b54923f49c8b94d50c5b96fa44f3f5a7ca90ef52fd55afb2bfd3ae8f60"

test_that("webhook signatures are checked", {
  expect_true(smsgora_verify_webhook(body, good, "whsec_test"))
  expect_true(smsgora_verify_webhook(charToRaw(body), paste0(" ", good, "\n"), "whsec_test"))
  expect_false(smsgora_verify_webhook(body, good, "other_secret"))
  expect_false(smsgora_verify_webhook(paste0(body, " "), good, "whsec_test"))
  expect_false(smsgora_verify_webhook(body, "", "whsec_test"))
  expect_false(smsgora_verify_webhook(body, NULL, "whsec_test"))
  expect_false(smsgora_verify_webhook(body, NA_character_, "whsec_test"))
})

test_that("clients need a key", {
  expect_error(smsgora_client(""), "API key")
  expect_error(smsgora_client(NA_character_), "API key")
  cl <- smsgora_client("sgk_x", base_url = "https://example.com/api/")
  expect_s3_class(cl, "smsgora_client")
  expect_equal(cl$base_url, "https://example.com/api")
  expect_error(smsgora_get_account(list()), "smsgora_client")
})

test_that("API errors become smsgora_error conditions", {
  resp <- httr2::response(status_code = 401L,
                          headers = list(`Content-Type` = "application/json"),
                          body = charToRaw('{"success":false,"message":"Invalid API key"}'))
  err <- tryCatch(smsgora:::handle_response(resp), smsgora_error = function(e) e)
  expect_s3_class(err, "smsgora_error")
  expect_equal(err$status, 401L)
  expect_equal(err$api_message, "Invalid API key")
  ok <- httr2::response(status_code = 200L, body = charToRaw('{"success":true,"data":{"id":7}}'))
  expect_equal(smsgora:::handle_response(ok), list(id = 7L))
})

test_that("the live API rejects an invalid key with 401", {
  skip_if_not(identical(Sys.getenv("SMSGORA_LIVE_TEST"), "true"), "live test not enabled")
  skip_if_offline("tekservix.com")
  cl <- smsgora_client("sgk_invalid_r_test")
  err <- tryCatch(smsgora_get_account(cl), smsgora_error = function(e) e)
  expect_s3_class(err, "smsgora_error")
  expect_equal(err$status, 401L)
})
