#' Extract a single raw answer
#'
#' @param res A `jev_response` from [jev()].
#' @param id The question id.
#' @return The answer as returned by the API: for a noul a list with `noul`
#'   (probability of yes); for a choice `choice`, `probabilities` and
#'   `confidence`; for a score `score`, `legend`, `probabilities` and
#'   `confidence`.
#' @family answers
#' @export
jev_answer <- function(res, id) {
  check_response(res)
  a <- res$answers[[id]]
  if (is.null(a)) {
    rlang::abort(sprintf("No answer with id '%s'. Available: %s", id,
                         paste(names(res$answers), collapse = ", ")))
  }
  a
}

#' Probability table for one answer
#'
#' Returns the full distribution behind a choice or score answer as a tibble,
#' or the yes/no probabilities of a noul.
#'
#' @inheritParams jev_answer
#' @return A tibble with one row per option (choice), level (score) or outcome
#'   (noul). Choice tables have columns `option`, `probability`, `chosen`,
#'   `confidence`; score tables `level`, `label`, `probability`, `score`,
#'   `confidence`; noul tables `outcome` and `probability`.
#' @family answers
#' @export
jev_probabilities <- function(res, id) {
  a <- jev_answer(res, id)
  switch(a$type,
    noul = tibble::tibble(outcome = c("yes", "no"), probability = c(a$noul, 1 - a$noul)),
    choice = tibble::tibble(
      option = names(a$probabilities),
      probability = unlist(a$probabilities, use.names = FALSE),
      chosen = names(a$probabilities) == a$choice,
      confidence = a$confidence
    ),
    score = {
      lv <- names(a$probabilities)
      tibble::tibble(
        level = as.integer(lv),
        label = unlist(a$legend, use.names = FALSE)[match(lv, names(a$legend))],
        probability = unlist(a$probabilities, use.names = FALSE),
        score = a$score,
        confidence = a$confidence
      )
    },
    rlang::abort(sprintf("Unknown answer type '%s'.", a$type))
  )
}

#' Flatten a response into one tidy row
#'
#' Turns every answer into columns of a one-row tibble so results from many
#' calls can be stacked. For each question id:
#' * noul: `<id>` holds the probability of yes;
#' * choice: `<id>` holds the chosen option and `<id>_confidence` the confidence;
#' * score: `<id>` holds the probability-weighted score and `<id>_confidence`
#'   the confidence.
#'
#' With `probabilities = TRUE`, choice and score answers also get one
#' `<id>_p_<option>` (or `<id>_p_<level>`) column per option or level.
#' Model and token usage go into `.model`, `.input_tokens` and
#' `.output_tokens`.
#'
#' @param res A `jev_response` from [jev()].
#' @param probabilities Also include per-option / per-level probability columns.
#' @return A one-row tibble.
#' @family answers
#' @export
jev_tidy <- function(res, probabilities = FALSE) {
  check_response(res)
  cols <- list()
  for (id in names(res$answers)) {
    a <- res$answers[[id]]
    if (a$type == "noul") {
      cols[[id]] <- a$noul
    } else if (a$type == "choice") {
      cols[[id]] <- a$choice
      cols[[paste0(id, "_confidence")]] <- a$confidence
    } else if (a$type == "score") {
      cols[[id]] <- a$score
      cols[[paste0(id, "_confidence")]] <- a$confidence
    }
    if (probabilities && a$type %in% c("choice", "score")) {
      for (k in names(a$probabilities)) {
        cols[[paste0(id, "_p_", k)]] <- a$probabilities[[k]]
      }
    }
  }
  cols$.model <- res$model
  cols$.input_tokens <- res$usage$input_tokens %||% NA_integer_
  cols$.output_tokens <- res$usage$output_tokens %||% NA_integer_
  tibble::as_tibble(cols)
}

#' Token usage of a response
#'
#' @inheritParams jev_tidy
#' @return A one-row tibble with `model`, `input_tokens` and `output_tokens`.
#' @family answers
#' @export
jev_usage <- function(res) {
  check_response(res)
  tibble::tibble(
    model = res$model,
    input_tokens = res$usage$input_tokens %||% NA_integer_,
    output_tokens = res$usage$output_tokens %||% NA_integer_
  )
}

check_response <- function(res) {
  if (!is.list(res) || is.null(res$answers)) {
    rlang::abort("`res` must be a jev_response returned by jev().")
  }
  invisible(res)
}
