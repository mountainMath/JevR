fake_response <- function() {
  JevR:::new_jev_response(list(
    model = "jev-1.13.0",
    answers = list(
      urgent = list(type = "noul", noul = 0.91),
      topic = list(type = "choice", choice = "billing",
                   probabilities = list(billing = 0.8, other = 0.2), confidence = 0.75),
      mood = list(type = "score", score = 1.05,
                  legend = list("0" = "Calm", "1" = "Frustrated", "2" = "Very angry"),
                  probabilities = list("0" = 0, "1" = 0.95, "2" = 0.05), confidence = 0.92)
    ),
    usage = list(input_tokens = 300, output_tokens = 20)
  ))
}

test_that("jev_answer returns raw answers and errors on unknown ids", {
  res <- fake_response()
  expect_equal(jev_answer(res, "urgent")$noul, 0.91)
  expect_error(jev_answer(res, "nope"), "No answer with id")
})

test_that("jev_probabilities builds the right tables", {
  res <- fake_response()
  n <- jev_probabilities(res, "urgent")
  expect_equal(n$probability, c(0.91, 0.09))

  ch <- jev_probabilities(res, "topic")
  expect_equal(ch$option, c("billing", "other"))
  expect_equal(ch$chosen, c(TRUE, FALSE))

  sc <- jev_probabilities(res, "mood")
  expect_equal(sc$level, 0:2)
  expect_equal(sc$label, c("Calm", "Frustrated", "Very angry"))
  expect_equal(sum(sc$probability), 1)
  expect_equal(unique(sc$score), 1.05)
})

test_that("jev_tidy flattens to one row", {
  res <- fake_response()
  t <- jev_tidy(res)
  expect_equal(nrow(t), 1)
  expect_named(t, c("urgent", "topic", "topic_confidence", "mood", "mood_confidence",
                    ".model", ".input_tokens", ".output_tokens"))
  expect_equal(t$topic, "billing")
  expect_equal(t$mood, 1.05)

  tp <- jev_tidy(res, probabilities = TRUE)
  expect_true(all(c("topic_p_billing", "topic_p_other", "mood_p_0", "mood_p_2") %in% names(tp)))
  expect_equal(tp$mood_p_1, 0.95)
})

test_that("jev_usage reports tokens", {
  u <- jev_usage(fake_response())
  expect_equal(u$input_tokens, 300)
  expect_equal(u$model, "jev-1.13.0")
})

test_that("print methods run", {
  expect_output(print(fake_response()), "jev_response")
  expect_output(print(jev_noul("x")), "jev_question")
})
