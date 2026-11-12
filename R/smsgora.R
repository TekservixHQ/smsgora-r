#' smsgora: Client for the 'SMS Gora' SMS Gateway
#'
#' SMS Gora SMS Gateway by Tekservix: send and receive SMS through your own
#' Android phones and SIM cards. You need a Tekservix account with SMS Gora,
#' at least one phone linked with the SMS Gora app, and an API key (it starts
#' with `sgk_`). Create one at <https://tekservix.com/smsgora>. API reference:
#' <https://tekservix.com/smsgora/docs/>.
#'
#' Start with [smsgora_client()], then call the other `smsgora_*` functions.
#' Every call returns the `data` part of the API answer as a list. API errors
#' are raised as conditions of class `smsgora_error`, which carry the HTTP
#' `status`, the API `message` and the raw `body`.
#'
#' @keywords internal
"_PACKAGE"

#' Create an SMS Gora client
#'
#' @param key Your SMS Gora API key (starts with `sgk_`). Defaults to the
#'   `SMSGORA_API_KEY` environment variable.
#' @param base_url API base URL. Change it only for tests or a proxy.
#' @return An object of class `smsgora_client`, used by every other function.
#' @export
#' @examples
#' client <- smsgora_client("sgk_your_key")
smsgora_client <- function(key = Sys.getenv("SMSGORA_API_KEY"),
                           base_url = "https://tekservix.com/smsgora/api/v1") {
  if (!is.character(key) || length(key) != 1L || !nzchar(key)) {
    stop("An SMS Gora API key is required (SMS Gora -> API keys in your Tekservix account).",
         call. = FALSE)
  }
  structure(list(key = key, base_url = sub("/+$", "", base_url)),
            class = "smsgora_client")
}

#' @export
print.smsgora_client <- function(x, ...) {
  cat("<smsgora_client> ", x$base_url, "\n", sep = "")
  invisible(x)
}

#' Call any SMS Gora endpoint
#'
#' Low-level access for endpoints that have no helper of their own.
#'
#' @param client A client from [smsgora_client()].
#' @param method HTTP method, e.g. `"GET"` or `"POST"`.
#' @param path Path after the base URL, e.g. `"/batches/311"`.
#' @param body Optional list sent as JSON.
#' @param query Optional named list of query parameters.
#' @return The `data` part of the API answer, as a list.
#' @export
#' @examples
#' \dontrun{
#' client <- smsgora_client("sgk_your_key")
#' smsgora_request(client, "GET", "/account")
#' }
smsgora_request <- function(client, method, path, body = NULL, query = NULL) {
  check_client(client)
  req <- httr2::request(paste0(client$base_url, path))
  req <- httr2::req_method(req, method)
  req <- httr2::req_headers(req, `x-api-key` = client$key, Accept = "application/json",
                            .redact = "x-api-key")
  req <- httr2::req_user_agent(req, "smsgora-r/1.0.0")
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  query <- Filter(Negate(is.null), as.list(query))
  if (length(query)) req <- do.call(httr2::req_url_query, c(list(req), query))
  if (!is.null(body)) req <- httr2::req_body_json(req, body, auto_unbox = TRUE, null = "null")
  resp <- httr2::req_perform(req)
  handle_response(resp)
}

#' Send an SMS
#'
#' Sends one text to one or more numbers (up to 1,000), from your best online
#' phone or from the phone you choose.
#'
#' @inheritParams smsgora_request
#' @param to Character vector of phone numbers in international format.
#' @param message The text to send.
#' @param device_id Optional ID of the phone to send from.
#' @param sim_slot Optional SIM: 0 = SIM 1, 1 = SIM 2.
#' @param schedule_at Optional ISO 8601 time to send later.
#' @return A list with the queued message IDs.
#' @export
#' @examples
#' \dontrun{
#' client <- smsgora_client("sgk_your_key")
#' smsgora_send_sms(client, "+250788123456", "Your order #1042 has shipped.")
#' smsgora_send_sms(client, c("+250788123456", "+250722000000"),
#'                  "Reminder: appointment tomorrow",
#'                  device_id = 12, sim_slot = 1, schedule_at = "2026-12-01T08:00:00Z")
#' }
smsgora_send_sms <- function(client, to, message, device_id = NULL,
                             sim_slot = NULL, schedule_at = NULL) {
  body <- compact(list(recipients = as.list(as.character(to)), message = message,
                       sim_slot = sim_slot, schedule_at = schedule_at))
  path <- if (is.null(device_id)) "/send-sms" else paste0("/devices/", id(device_id), "/send-sms")
  smsgora_request(client, "POST", path, body = body)
}

#' Send different texts to different people
#'
#' Bulk sending from one phone, up to 5,000 recipients per call.
#'
#' @inheritParams smsgora_request
#' @param device_id ID of the phone to send from.
#' @param messages A list of messages; each is a list with `recipients`
#'   (character vector) and an optional `message`.
#' @param template Optional text used for messages that have no `message`.
#' @return A list describing the batch, including its ID.
#' @export
#' @examples
#' \dontrun{
#' client <- smsgora_client("sgk_your_key")
#' smsgora_send_bulk_sms(client, 12, list(
#'   list(recipients = "+250788123456", message = "Hi Alice, your table is ready"),
#'   list(recipients = c("+250722000000", "+250733000000"))
#' ), template = "Thank you for visiting us")
#' }
smsgora_send_bulk_sms <- function(client, device_id, messages, template = NULL) {
  messages <- lapply(messages, function(m) {
    m$recipients <- as.list(as.character(m$recipients))
    m
  })
  body <- compact(list(messages = messages, messageTemplate = template))
  smsgora_request(client, "POST", paste0("/devices/", id(device_id), "/send-bulk-sms"), body = body)
}

#' Get one message
#'
#' @inheritParams smsgora_request
#' @param message_id Message ID.
#' @return The message, with its status (queued, dispatched, sent, delivered,
#'   failed or cancelled).
#' @export
#' @examples
#' \dontrun{
#' smsgora_get_message(smsgora_client("sgk_your_key"), 1873)
#' }
smsgora_get_message <- function(client, message_id) {
  smsgora_request(client, "GET", paste0("/messages/", id(message_id)))
}

#' Cancel a queued or scheduled message
#'
#' @inheritParams smsgora_get_message
#' @return The cancelled message.
#' @export
#' @examples
#' \dontrun{
#' smsgora_cancel_message(smsgora_client("sgk_your_key"), 1873)
#' }
smsgora_cancel_message <- function(client, message_id) {
  smsgora_request(client, "POST", paste0("/messages/", id(message_id), "/cancel"))
}

#' List messages
#'
#' @inheritParams smsgora_request
#' @param ... Filters passed as query parameters: `direction`, `status`,
#'   `device_id`, `phone`, `since`, `page`, `limit`.
#' @return A list with the matching messages and paging details.
#' @export
#' @examples
#' \dontrun{
#' smsgora_list_messages(smsgora_client("sgk_your_key"), status = "failed", limit = 20)
#' }
smsgora_list_messages <- function(client, ...) {
  smsgora_request(client, "GET", "/messages", query = list(...))
}

#' List received SMS
#'
#' @inheritParams smsgora_request
#' @param device_id Optional phone ID; all phones when `NULL`.
#' @param ... Query parameters such as `page` and `limit`.
#' @return A list with the received messages and paging details.
#' @export
#' @examples
#' \dontrun{
#' smsgora_list_received_sms(smsgora_client("sgk_your_key"), device_id = 12, limit = 10)
#' }
smsgora_list_received_sms <- function(client, device_id = NULL, ...) {
  path <- if (is.null(device_id)) "/received-sms" else paste0("/devices/", id(device_id), "/get-received-sms")
  smsgora_request(client, "GET", path, query = list(...))
}

#' Get a bulk batch
#'
#' @inheritParams smsgora_request
#' @param batch_id Batch ID returned by [smsgora_send_bulk_sms()].
#' @return The batch and its progress.
#' @export
#' @examples
#' \dontrun{
#' smsgora_get_batch(smsgora_client("sgk_your_key"), 311)
#' }
smsgora_get_batch <- function(client, batch_id) {
  smsgora_request(client, "GET", paste0("/batches/", id(batch_id)))
}

#' List your linked phones
#'
#' @inheritParams smsgora_request
#' @return A list of phones with their status.
#' @export
#' @examples
#' \dontrun{
#' smsgora_list_devices(smsgora_client("sgk_your_key"))
#' }
smsgora_list_devices <- function(client) {
  smsgora_request(client, "GET", "/devices")
}

#' Get one phone
#'
#' @inheritParams smsgora_request
#' @param device_id Phone ID.
#' @return The phone and its status.
#' @export
#' @examples
#' \dontrun{
#' smsgora_get_device(smsgora_client("sgk_your_key"), 12)
#' }
smsgora_get_device <- function(client, device_id) {
  smsgora_request(client, "GET", paste0("/devices/", id(device_id)))
}

#' Get your account
#'
#' @inheritParams smsgora_request
#' @return Your plan, limits and usage.
#' @export
#' @examples
#' \dontrun{
#' smsgora_get_account(smsgora_client("sgk_your_key"))
#' }
smsgora_get_account <- function(client) {
  smsgora_request(client, "GET", "/account")
}

#' List your webhooks
#'
#' @inheritParams smsgora_request
#' @return A list of webhooks.
#' @export
#' @examples
#' \dontrun{
#' smsgora_list_webhooks(smsgora_client("sgk_your_key"))
#' }
smsgora_list_webhooks <- function(client) {
  smsgora_request(client, "GET", "/webhooks")
}

#' Add a webhook
#'
#' @inheritParams smsgora_request
#' @param url HTTPS address that receives the events.
#' @param events Character vector of events, e.g. `"sms.received"`,
#'   `"sms.failed"`.
#' @param device_id Optional phone ID to limit the webhook to one phone.
#' @return The new webhook, including its signing secret.
#' @export
#' @examples
#' \dontrun{
#' smsgora_create_webhook(smsgora_client("sgk_your_key"),
#'                        "https://example.com/sms-hook", c("sms.received", "sms.failed"))
#' }
smsgora_create_webhook <- function(client, url, events, device_id = NULL) {
  body <- compact(list(url = url, events = as.list(as.character(events)), device_id = device_id))
  smsgora_request(client, "POST", "/webhooks", body = body)
}

#' Remove a webhook
#'
#' @inheritParams smsgora_request
#' @param webhook_id Webhook ID.
#' @return The API confirmation.
#' @export
#' @examples
#' \dontrun{
#' smsgora_delete_webhook(smsgora_client("sgk_your_key"), 4)
#' }
smsgora_delete_webhook <- function(client, webhook_id) {
  smsgora_request(client, "DELETE", paste0("/webhooks/", id(webhook_id)))
}

#' Verify a webhook signature
#'
#' Checks the `X-SmsGora-Signature` header (`sha256=` followed by the hex
#' HMAC-SHA256 of the raw request body, keyed with your webhook secret). The
#' comparison takes the same time whether or not the signature matches.
#'
#' @param raw_body The raw request body, as a string or raw vector, exactly as
#'   received.
#' @param signature Value of the `X-SmsGora-Signature` header.
#' @param secret Your webhook secret.
#' @return `TRUE` when the signature is valid, otherwise `FALSE`.
#' @export
#' @examples
#' body <- '{"event":"sms.received"}'
#' sig <- "sha256=313fe9b54923f49c8b94d50c5b96fa44f3f5a7ca90ef52fd55afb2bfd3ae8f60"
#' smsgora_verify_webhook(body, sig, "whsec_test")
smsgora_verify_webhook <- function(raw_body, signature, secret) {
  if (is.null(signature) || length(signature) != 1L || is.na(signature)) return(FALSE)
  if (is.character(raw_body)) raw_body <- charToRaw(enc2utf8(paste(raw_body, collapse = "")))
  expected <- paste0("sha256=", as.character(openssl::sha256(raw_body, key = enc2utf8(secret))))
  same_bytes(charToRaw(expected), charToRaw(trimws(signature)))
}

# ---- internal ----------------------------------------------------------------

check_client <- function(client) {
  if (!inherits(client, "smsgora_client")) {
    stop("`client` must come from smsgora_client().", call. = FALSE)
  }
}

compact <- function(x) Filter(Negate(is.null), x)

id <- function(x) format(as.numeric(x), scientific = FALSE, trim = TRUE)

same_bytes <- function(a, b) {
  if (length(a) != length(b) || length(a) == 0L) return(FALSE)
  sum(as.integer(xor(a, b))) == 0L
}

handle_response <- function(resp) {
  status <- httr2::resp_status(resp)
  text <- if (httr2::resp_has_body(resp)) httr2::resp_body_string(resp) else ""
  parsed <- if (nzchar(text)) {
    tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
  }
  if (status >= 400) {
    msg <- if (is.list(parsed) && is.character(parsed$message)) parsed$message else paste("HTTP", status)
    cond <- structure(
      class = c("smsgora_error", "error", "condition"),
      list(message = sprintf("SMS Gora API error %d: %s", status, msg), call = NULL,
           status = status, api_message = msg, body = text)
    )
    stop(cond)
  }
  if (is.list(parsed) && !is.null(names(parsed)) && "data" %in% names(parsed)) parsed$data else parsed
}
