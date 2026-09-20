req_body <- function(req) req$body$data

test_that("jev_request builds the documented body", {
  req <- jev_request(
    "Help! My payouts have been failing for 3 days.",
    list(is_urgent = jev_noul("Does this convey urgency?")),
    api_key = "dummy"
  )
  expect_s3_class(req, "httr2_request")
  expect_equal(req$url, "https://api.typesafe.ai/v1/systemone")
  expect_true("Authorization" %in% names(req$headers))
  body <- req_body(req)
  expect_equal(body$model, "jev-latest")
  expect_equal(body$state, "Help! My payouts have been failing for 3 days.")
  expect_equal(
    as.character(jsonlite::toJSON(body, auto_unbox = TRUE)),
    '{"state":"Help! My payouts have been failing for 3 days.","model":"jev-latest","questions":{"is_urgent":{"type":"noul","instructions":"Does this convey urgency?"}}}'
  )
})

test_that("missing API key errors early", {
  withr::local_envvar(TYPESAFE_API_KEY = "")
  expect_error(jev_request("x", list(q = jev_noul("y"))), "TYPESAFE_API_KEY")
})

test_that("state coercion handles lists, data frames and empties", {
  expect_equal(JevR:::as_state("text"), "text")
  expect_equal(JevR:::as_state(list(a = "x", b = NA, c = NULL, d = "")), list(a = "x"))
  df <- data.frame(title = "T", body = NA_character_, n = 3)
  expect_equal(JevR:::as_state(df), list(title = "T", n = 3))
  df$extra <- list(list(k = "v"))
  expect_equal(JevR:::as_state(df)$extra, list(k = "v"))
  expect_error(JevR:::as_state(""), "empty")
  expect_error(JevR:::as_state(list(a = NA)), "no non-empty")
  expect_error(JevR:::as_state(data.frame(a = 1:2)), "one row")
})

test_that("named-vector state serialises as an object", {
  req <- jev_request(c(title = "T", text = "hello"), list(q = jev_noul("y")), api_key = "k")
  expect_equal(req_body(req)$state, list(title = "T", text = "hello"))
})

test_that("jev_map stacks rows and records errors", {
  fake <- function(state, questions, ...) {
    txt <- if (is.list(state)) state$text else state
    if (identical(txt, "boom")) stop("simulated failure")
    JevR:::new_jev_response(list(
      model = "jev-test",
      answers = list(
        short = list(type = "noul", noul = 0.5),
        mood = list(type = "score", score = 1, legend = list("0" = "a", "1" = "b"),
                    probabilities = list("0" = 0, "1" = 1), confidence = 1)
      ),
      usage = list(input_tokens = 10, output_tokens = 1)
    ))
  }
  local_mocked_bindings(jev = fake)
  qs <- list(short = jev_noul("short?"), mood = jev_score("mood?", c("a", "b")))

  expect_warning(
    out <- jev_map(c("one", "boom", "three"), qs, api_key = "k", progress = FALSE),
    "1 of 3 requests failed"
  )
  expect_equal(nrow(out), 3)
  expect_equal(out$short, c(0.5, NA, 0.5))
  expect_equal(is.na(out$.error), c(TRUE, FALSE, TRUE))
  expect_match(out$.error[2], "simulated")

  df <- data.frame(id = 1:2, text = c("x", "boom"))
  expect_error(jev_map(df, qs, api_key = "k", progress = FALSE, on_error = "stop"), "simulated")

  with_resp <- suppressWarnings(jev_map(list("a", "b"), qs, api_key = "k", progress = FALSE,
                                        keep_response = TRUE))
  expect_s3_class(with_resp$.response[[1]], "jev_response")

  expect_warning(all_fail <- jev_map("boom", qs, api_key = "k", progress = FALSE), "All 1")
  expect_equal(nrow(all_fail), 1)
  expect_false(is.na(all_fail$.error))
})

test_that("live API call works when a key is available", {
  skip_on_cran()
  skip_if(!nzchar(Sys.getenv("TYPESAFE_API_KEY")), "TYPESAFE_API_KEY not set")

  res <- jev(
    "Help! My payouts have been failing for 3 days.",
    list(
      urgent = jev_noul("Does this convey urgency?"),
      topic = jev_choice("What is this about?",
                         c(payments = "A payment or payout problem",
                           other = "Something else")),
      mood = jev_score("How does the writer feel?", c("Calm", "Frustrated", "Very angry"))
    )
  )
  expect_s3_class(res, "jev_response")
  expect_gt(res$answers$urgent$noul, 0.5)
  expect_equal(res$answers$topic$choice, "payments")
  expect_gt(res$answers$mood$score, 0.5)
  t <- jev_tidy(res, probabilities = TRUE)
  expect_equal(nrow(t), 1)
  expect_true(t$.input_tokens > 0)
})
