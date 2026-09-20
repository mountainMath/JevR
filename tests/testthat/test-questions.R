to_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE))

test_that("noul questions serialise with optional criteria", {
  q <- jev_noul("Is it urgent?")
  expect_s3_class(q, "jev_question")
  expect_equal(to_json(unclass(q)), '{"type":"noul","instructions":"Is it urgent?"}')

  q2 <- jev_noul("Is it urgent?", yes = "Needs action now", no = "Can wait")
  expect_equal(
    to_json(unclass(q2)),
    '{"type":"noul","instructions":"Is it urgent?","criteria":{"true":"Needs action now","false":"Can wait"}}'
  )
})

test_that("choice criteria become a JSON object even from a named vector", {
  q <- jev_choice("Topic?", c(a = "About a", b = "About b"))
  expect_equal(
    to_json(unclass(q)),
    '{"type":"choice","instructions":"Topic?","criteria":{"a":"About a","b":"About b"}}'
  )
  expect_error(jev_choice("Topic?", c("About a", "About b")), "named")
})

test_that("score levels stay an array even with one-element edge cases guarded", {
  q <- jev_score("Mood?", c("Calm", "Angry"))
  expect_equal(
    to_json(unclass(q)),
    '{"type":"score","instructions":"Mood?","criteria":["Calm","Angry"]}'
  )
  expect_error(jev_score("Mood?", "Calm"), "at least two")
})

test_that("structured instructions are preserved", {
  q <- jev_noul(list(question = "Does `text` match `policy`?", policy = "No refunds after 30 days"))
  expect_equal(
    to_json(unclass(q)),
    '{"type":"noul","instructions":{"question":"Does `text` match `policy`?","policy":"No refunds after 30 days"}}'
  )
})

test_that("plain-list questions are normalised like constructor output", {
  raw <- list(
    a = list(type = "noul", instructions = "x", criteria = c("true" = "yes", "false" = "no")),
    b = list(type = "choice", instructions = "y", criteria = c(p = "P", q = "Q")),
    c = list(type = "score", instructions = "z", criteria = c("low", "high"))
  )
  built <- list(
    a = jev_noul("x", yes = "yes", no = "no"),
    b = jev_choice("y", c(p = "P", q = "Q")),
    c = jev_score("z", c("low", "high"))
  )
  expect_equal(to_json(JevR:::normalise_questions(raw)),
               to_json(JevR:::normalise_questions(built)))
})

test_that("question lists must be named and non-empty", {
  expect_error(JevR:::normalise_questions(list(jev_noul("x"))), "named")
  expect_error(JevR:::normalise_questions(list()), "non-empty")
  expect_error(JevR:::normalise_questions(list(a = jev_noul("x"), a = jev_noul("y"))), "duplicated")
  expect_error(JevR:::normalise_questions(jev_noul("x")), "named list")
})
