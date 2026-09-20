#' Run the same questions over many states
#'
#' Calls [jev()] once per state and stacks the tidied answers into a tibble
#' with one row per state, in input order. Failed calls are recorded rather
#' than aborting the whole run.
#'
#' @param states The records to evaluate: a character vector (one string per
#'   state), a list of states (strings or named lists), or a data frame whose
#'   rows become named-list states (columns with `NA`, `NULL` or empty values
#'   are dropped per row; list columns are unwrapped).
#' @inheritParams jev
#' @param probabilities Passed to [jev_tidy()].
#' @param pause Seconds to wait between requests.
#' @param progress Show a progress bar (see [purrr::map()]).
#' @param on_error What to do when a request fails after retries: `"warn"`
#'   (default) keeps going and leaves that row's answers `NA`, recording the
#'   message in `.error`; `"stop"` aborts.
#' @param keep_response Also keep every raw `jev_response` in a `.response`
#'   list column.
#'
#' @return A tibble with one row per state: the [jev_tidy()] columns, plus
#'   `.error` (`NA` when the call succeeded) and optionally `.response`.
#'   Bind it back onto your data with [dplyr::bind_cols()] or keep an id column
#'   in `states`.
#'
#' @examples
#' \dontrun{
#' qs <- list(
#'   cost = jev_noul("Does the reviewer complain about cost?"),
#'   sentiment = jev_score("How does the reviewer feel overall?",
#'                         c("Very negative", "Negative", "Neutral", "Positive", "Very positive"))
#' )
#' reviews <- data.frame(comment = c("Great dentist, no pain at all.",
#'                                   "Charged me double what was quoted."))
#' jev_map(reviews, qs)
#' }
#' @export
jev_map <- function(states, questions, model = "jev-latest",
                    api_key = jev_api_key(), probabilities = FALSE,
                    pause = 0, progress = TRUE,
                    on_error = c("warn", "stop"), keep_response = FALSE,
                    max_tries = 5, timeout = 60) {
  on_error <- match.arg(on_error)
  states <- as_state_list(states)
  questions <- normalise_questions(questions)

  one <- function(state) {
    if (pause > 0) Sys.sleep(pause)
    res <- tryCatch(
      jev(state, questions, model = model, api_key = api_key,
          max_tries = max_tries, timeout = timeout),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      if (on_error == "stop") rlang::abort(conditionMessage(res), parent = res)
      return(list(error = conditionMessage(res), response = NULL))
    }
    list(error = NA_character_, response = res)
  }

  out <- purrr::map(states, one, .progress = progress)
  responses <- purrr::map(out, "response")
  errors <- purrr::map_chr(out, "error")

  ok <- !vapply(responses, is.null, logical(1))
  if (!any(ok)) {
    rlang::warn(sprintf("All %d requests failed. First error: %s", length(states), errors[1]))
  } else if (any(!ok)) {
    rlang::warn(sprintf("%d of %d requests failed; see the `.error` column.",
                        sum(!ok), length(states)))
  }

  tidy <- purrr::map(responses, function(r) if (is.null(r)) NULL else jev_tidy(r, probabilities))
  if (any(ok)) {
    # an all-NA row shaped like the first successful result keeps failed rows aligned
    na_row <- tidy[[which(ok)[1]]][NA_integer_, ]
    rows <- purrr::map(tidy, function(t) if (is.null(t)) na_row else t)
    result <- purrr::list_rbind(rows)
  } else {
    result <- tibble::tibble(.rows = length(states))
  }
  result$.error <- errors
  if (keep_response) result$.response <- responses
  result
}

as_state_list <- function(states) {
  if (is.data.frame(states)) {
    return(lapply(seq_len(nrow(states)), function(i) df_row_to_list(states, i)))
  }
  if (is.character(states)) return(as.list(states))
  if (is.list(states)) return(states)
  rlang::abort("`states` must be a character vector, a list of states or a data frame.")
}
