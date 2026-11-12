# smsgora: SMS Gora SMS Gateway for R

**SMS Gora SMS Gateway**: the official R client by Tekservix. Send and receive SMS through your own Android phones and SIM cards, with delivery reports and signed webhooks.

**You need a Tekservix account with SMS Gora and at least one Android phone linked with the SMS Gora app.** Create an API key under SMS Gora → API keys (it starts with `sgk_`). [Create an account](https://tekservix.com/smsgora) · [API docs](https://tekservix.com/smsgora/docs/)

## Install

```r
install.packages("smsgora")
# development version
# install.packages("remotes"); remotes::install_github("TekservixHQ/smsgora-r")
```

## Use

```r
library(smsgora)

client <- smsgora_client("sgk_your_key")   # or set SMSGORA_API_KEY

smsgora_send_sms(client, "+250788123456", "Your order #1042 has shipped.")

# From a chosen phone and SIM, later
smsgora_send_sms(client, c("+250788123456", "+250722000000"),
                 "Reminder: appointment tomorrow",
                 device_id = 12, sim_slot = 1, schedule_at = "2026-12-01T08:00:00Z")

smsgora_get_message(client, 1873)                         # delivery status
smsgora_list_messages(client, status = "failed", limit = 20)
smsgora_list_received_sms(client, device_id = 12, limit = 10)
smsgora_list_devices(client)
smsgora_get_account(client)
```

Every function returns the API's `data` as a list. API errors are raised as `smsgora_error` conditions with `status`, `api_message` and `body`:

```r
tryCatch(smsgora_get_account(client),
         smsgora_error = function(e) message("Failed with HTTP ", e$status, ": ", e$api_message))
```

Webhooks: `smsgora_verify_webhook(raw_body, signature_header, secret)` checks `X-SmsGora-Signature`.

## More SMS Gora integrations

[npm](https://www.npmjs.com/package/smsgora) · [PyPI](https://pypi.org/project/smsgora/) · [Java/Kotlin](https://central.sonatype.com/artifact/com.tekservix/smsgora) · [All docs](https://tekservix.com/smsgora/docs/)

support@tekservix.com · MIT licensed.
