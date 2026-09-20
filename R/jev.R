#' Ask Jev typed questions about a state
#'
#' Sends one request to TypeSafe's System One endpoint. All questions are
#' evaluated against the same `state` in parallel and cannot see each other's
#' answers, so put independent questions in one call.
#'
#' @param state The content to evaluate: a string, or a named list (or one-row
#'   data frame) of named fields for structured context. Questions can refer to
#'   fields with backticked paths such as `` `review.text` ``.
#' @param questions A named list of questions built with [jev_noul()],
#'   [jev_choice()] or [jev_score()]. The names are the ids under which answers
#'   are returned; they are not sent to the model.
#' @param model Model name or alias. `"jev-latest"` is the current stable
#'   release; see [jev_models()].
#' @param api_key TypeSafe API key. Defaults to the `TYPESAFE_API_KEY`
#'   environment variable.
#' @param max_tries Number of attempts on rate-limit (429) or overloaded (529)
#'   responses, with exponential backoff.
#' @param timeout Request timeout in seconds.
#'
#' @return An object of class `jev_response`: a list with `model` (the
#'   versioned model that answered), `answers` (one per question id) and
#'   `usage` (input and output tokens). Use [jev_tidy()], [jev_probabilities()]
#'   or [jev_answer()] to work with the answers.
#'
#' @examples
#' \dontrun{
#' res <- jev(
#'   "Help! My payouts have been failing for 3 days.",
#'   list(
#'     urgent = jev_noul("Does this convey urgency?"),
#'     mood = jev_score("How does the writer feel?",
#'                      c("Calm", "Frustrated", "Very angry"))
#'   )
#' )
#' jev_tidy(res)
#' jev_probabilities(res, "mood")
#' }
#' @seealso [jev_map()] to run the same questions over many states.
#' @export
jev <- function(state, questions, model = "jev-latest",
                api_key = jev_api_key(), max_tries = 5, timeout = 60) {
  req <- jev_request(state, questions, model = model, api_key = api_key,
                     max_tries = max_tries, timeout = timeout)
  resp <- httr2::req_perform(req)
  new_jev_response(httr2::resp_body_json(resp))
}

#' Build (without sending) a Jev request
#'
#' Returns the `httr2` request that [jev()] would perform. Useful for
#' inspecting the JSON body with [httr2::req_dry_run()] or for performing many
#' requests concurrently with [httr2::req_perform_parallel()].
#'
#' @inheritParams jev
#' @return An `httr2_request`.
#' @examples
#' req <- jev_request("some text", list(q = jev_noul("Is this text short?")),
#'                    api_key = "dummy")
#' httr2::req_dry_run(req)
#' @export
jev_request <- function(state, questions, model = "jev-latest",
                        api_key = jev_api_key(), max_tries = 5, timeout = 60) {
  if (!nzchar(api_key)) {
    rlang::abort("No API key. Set the TYPESAFE_API_KEY environment variable or pass `api_key`.")
  }
  body <- list(
    state = as_state(state),
    model = model,
    questions = normalise_questions(questions)
  )
  httr2::request(jev_base_url()) |>
    httr2::req_url_path_append("v1", "systemone") |>
    httr2::req_user_agent(jev_user_agent()) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_body_json(body, auto_unbox = TRUE, null = "null", na = "null") |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(
      max_tries = max_tries,
      is_transient = function(r) httr2::resp_status(r) %in% c(429, 529),
      backoff = function(i) min(2^i, 30)
    ) |>
    httr2::req_error(body = jev_error_body)
}

#' Read the API key from the environment
#'
#' @return The value of the `TYPESAFE_API_KEY` environment variable, or an
#'   empty string if it is not set.
#' @export
jev_api_key <- function() Sys.getenv("TYPESAFE_API_KEY")

jev_base_url <- function() {
  Sys.getenv("TYPESAFE_BASE_URL", "https://api.typesafe.ai")
}

jev_user_agent <- function() {
  paste0("JevR/", utils::packageVersion("JevR"), " (https://github.com/mountainMath/JevR)")
}

jev_error_body <- function(resp) {
  body <- tryCatch(httr2::resp_body_string(resp), error = function(e) NULL)
  if (is.null(body) || !nzchar(body)) return(NULL)
  parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE), error = function(e) NULL)
  if (is.list(parsed) && !is.null(parsed$error)) {
    err <- parsed$error
    if (is.list(err)) err <- err$message %||% jsonlite::toJSON(err, auto_unbox = TRUE)
    return(as.character(err))
  }
  body
}

# Turn the state into something the API accepts: strings pass through; named
# lists and one-row data frames become objects with NA/NULL/empty fields dropped.
as_state <- function(state) {
  if (is.data.frame(state)) {
    if (nrow(state) != 1) {
      rlang::abort("A data frame `state` must have exactly one row; use jev_map() for many rows.")
    }
    state <- df_row_to_list(state, 1)
  }
  if (is.character(state) && length(state) == 1 && is.null(names(state))) {
    if (is.na(state) || !nzchar(state)) rlang::abort("`state` is empty.")
    return(state)
  }
  if (is.list(state) && !is.null(names(state))) {
    state <- drop_empty(state)
    if (length(state) == 0) rlang::abort("`state` has no non-empty fields.")
    return(as_json(state))
  }
  if (is.atomic(state) && !is.null(names(state))) return(as_json(state))
  if (is.list(state)) return(as_json(state))
  rlang::abort("`state` must be a string, a named list or a one-row data frame.")
}

drop_empty <- function(x) {
  keep <- vapply(x, function(v) {
    !(is.null(v) || (length(v) == 1 && is.atomic(v) && (is.na(v) || identical(v, ""))) ||
        length(v) == 0)
  }, logical(1))
  x[keep]
}

df_row_to_list <- function(df, i) {
  row <- lapply(df, function(col) {
    v <- col[i]
    if (is.list(v)) v[[1]] else v
  })
  drop_empty(row)
}

new_jev_response <- function(x) structure(x, class = c("jev_response", "list"))

#' @export
print.jev_response <- function(x, ...) {
  cat("<jev_response> model:", x$model,
      " tokens in/out:", x$usage$input_tokens, "/", x$usage$output_tokens, "\n")
  for (id in names(x$answers)) {
    a <- x$answers[[id]]
    line <- switch(a$type,
      noul = sprintf("  %s (noul): p(yes) = %.3f", id, a$noul),
      choice = sprintf("  %s (choice): %s  [confidence %.2f]", id, a$choice, a$confidence),
      score = sprintf("  %s (score): %.2f of 0..%d  [confidence %.2f]", id, a$score,
                      length(a$probabilities) - 1, a$confidence),
      sprintf("  %s (%s)", id, a$type)
    )
    cat(line, "\n")
  }
  invisible(x)
}
