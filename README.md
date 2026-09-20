# JevR

An R client for [TypeSafe's Jev](https://docs.typesafe.ai), a *System One*
model that answers typed questions about text or structured data. Instead of
generating prose, Jev returns probabilities: whether a condition holds (noul),
which of several options applies (choice), or where the input sits on an ordered
scale (score). That makes the answers easy to use directly in code and to
aggregate across many records.

## Installation

```r
# install.packages("remotes")
remotes::install_local("~/R/JevR")   # or remotes::install_github("mountainMath/JevR")
```

Set your API key in the `TYPESAFE_API_KEY` environment variable, for example in
`~/.Renviron`:

```
TYPESAFE_API_KEY='apikey_...'
```

## Usage

Build questions, ask them about a state, and tidy the answers:

```r
library(JevR)

questions <- list(
  urgent = jev_noul("Does this convey urgency?"),
  topic  = jev_choice("What is this message about?",
                      c(payments = "A payment or payout problem",
                        account  = "Logging in or account access",
                        other    = "Something else")),
  mood   = jev_score("How does the writer feel?",
                     c("Calm", "Frustrated", "Very angry"))
)

res <- jev("Help! My payouts have been failing for 3 days.", questions)
res
#> <jev_response> model: jev-1.13.0  tokens in/out: 350 / 30
#>   urgent (noul): p(yes) = 0.97
#>   topic (choice): payments  [confidence 0.95]
#>   mood (score): 1.10 of 0..2  [confidence 0.88]

jev_tidy(res)                    # one row: urgent, topic, topic_confidence, mood, ...
jev_probabilities(res, "mood")   # the full distribution over levels
```

All questions in one call are evaluated against the same state in parallel, so
put independent questions together. The state can be a string or a named list
(or one-row data frame) of fields, which questions can refer to by name in
backticks:

```r
jev(list(title = "Re: towers on Broadway", letter = letter_text),
    list(housing = jev_noul("Is `letter` mainly about housing?")))
```

### Many records

`jev_map()` runs the same questions over a character vector, a list of states or
the rows of a data frame, and returns one tidy row per record. Failed requests
are kept as `NA` rows with the message in `.error`.

```r
reviews <- data.frame(comment = c("Great dentist, no pain at all.",
                                  "Charged me double what was quoted."))

concerns <- list(
  cost = jev_noul("Does the reviewer complain about cost or billing?"),
  pain = jev_noul("Does the reviewer complain about pain or discomfort?"),
  sentiment = jev_score("How does the reviewer feel overall?",
                        c("Very negative", "Negative", "Mixed", "Positive", "Very positive"))
)

jev_map(reviews, concerns) |> dplyr::bind_cols(reviews)
```

### Other helpers

- `jev_request()` builds the `httr2` request without sending it, for
  `httr2::req_dry_run()` or `httr2::req_perform_parallel()`.
- `jev_answer()` returns one raw answer; `jev_usage()` the token counts.
- `jev_models()` lists the model names and aliases available to your account.

Rate-limit (429) and overloaded (529) responses are retried with exponential
backoff. Set `TYPESAFE_BASE_URL` to point at a different endpoint.

## Writing good questions

The [TypeSafe docs](https://docs.typesafe.ai/) cover this in depth. In
short: ask one narrow judgment per question, describe what yes and no mean for
nouls, give choice questions a no-match option, and make each score level a
concrete situation that stands on its own. Probabilities are calibrated
decisions, so evaluate thresholds on your own data before acting on them.
