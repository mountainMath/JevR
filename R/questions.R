#' Build a noul (yes/no) question
#'
#' A noul question asks whether a condition holds. Jev returns the probability
#' that the answer is yes. Use one noul per label when several labels may apply
#' at once.
#'
#' @param instructions The yes/no question to evaluate. A string, or a named
#'   list holding the question in one field and data it refers to in others.
#' @param yes,no Optional descriptions of what a yes (probability near 1) and a
#'   no (probability near 0) mean. Either may be a string or a named list.
#'
#' @return A list of class `jev_question` ready to be sent by [jev()].
#' @examples
#' jev_noul("Does the review complain about cost?",
#'          yes = "The reviewer describes a negative experience involving cost.",
#'          no = "Cost is not mentioned, or only positively.")
#' @family questions
#' @export
jev_noul <- function(instructions, yes = NULL, no = NULL) {
  q <- list(type = "noul", instructions = as_json(instructions))
  if (!is.null(yes) || !is.null(no)) {
    q$criteria <- list()
    if (!is.null(yes)) q$criteria[["true"]] <- as_json(yes)
    if (!is.null(no)) q$criteria[["false"]] <- as_json(no)
  }
  new_jev_question(q)
}

#' Build a choice question
#'
#' A choice question picks one option from a defined set. Jev returns the chosen
#' option together with a probability for every option and a confidence
#' summarising how concentrated the distribution is.
#'
#' @inheritParams jev_noul
#' @param criteria A named character vector or named list of options. Names are
#'   the option ids returned in the answer; values describe each option.
#'   Include a no-match option (for example `other`) when nothing may fit.
#'
#' @return A list of class `jev_question` ready to be sent by [jev()].
#' @examples
#' jev_choice("What is this review mainly about?",
#'            c(cost = "Mainly about cost or billing",
#'              pain = "Mainly about pain or discomfort",
#'              praise = "General praise without a specific concern",
#'              other = "Something else"))
#' @family questions
#' @export
jev_choice <- function(instructions, criteria) {
  if (is.null(names(criteria)) || any(names(criteria) == "")) {
    rlang::abort("`criteria` of a choice question must be a named vector or list.")
  }
  new_jev_question(list(
    type = "choice",
    instructions = as_json(instructions),
    criteria = lapply(as.list(criteria), as_json)
  ))
}

#' Build a score question
#'
#' A score question rates the state along an ordered dimension. Jev returns a
#' probability-weighted position on the levels (a number between 0 and
#' `length(levels) - 1`, which can land between levels), the probability of
#' each level and a confidence.
#'
#' @inheritParams jev_noul
#' @param levels An unnamed character vector (or list) of level descriptions in
#'   increasing order. Each level should describe a concrete situation and
#'   stand on its own.
#'
#' @return A list of class `jev_question` ready to be sent by [jev()].
#' @examples
#' jev_score("How does the reviewer feel about the experience overall?",
#'           c("Very negative: angry, warns others away",
#'             "Negative: dissatisfied",
#'             "Mixed or neutral",
#'             "Positive: satisfied",
#'             "Very positive: enthusiastic, strongly recommends"))
#' @family questions
#' @export
jev_score <- function(instructions, levels) {
  if (length(levels) < 2) {
    rlang::abort("`levels` of a score question needs at least two levels.")
  }
  new_jev_question(list(
    type = "score",
    instructions = as_json(instructions),
    criteria = lapply(unname(as.list(levels)), as_json)
  ))
}

new_jev_question <- function(q) structure(q, class = c("jev_question", "list"))

#' @export
print.jev_question <- function(x, ...) {
  cat("<jev_question:", x$type, ">\n")
  cat(jsonlite::toJSON(unclass(x), auto_unbox = TRUE, pretty = TRUE), "\n")
  invisible(x)
}

# Coerce R values into the shape jsonlite serialises the way the API expects:
# named atomic vectors become objects (not arrays), unnamed length-1 vectors
# stay scalars, and everything else is left alone.
as_json <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "jev_question")) return(unclass(x))
  if (is.atomic(x) && !is.null(names(x))) return(lapply(as.list(x), as_json))
  if (is.list(x)) return(lapply(x, as_json))
  x
}

# Normalise a user-supplied question so plain lists work as well as
# constructor output; validates type and criteria shape.
normalise_question <- function(q, id) {
  if (inherits(q, "jev_question")) return(unclass(q))
  if (!is.list(q) || is.null(q$type)) {
    rlang::abort(sprintf("Question `%s` must be built with jev_noul(), jev_choice() or jev_score(), or be a list with a `type`.", id))
  }
  switch(q$type,
    noul = {
      out <- list(type = "noul", instructions = as_json(q$instructions))
      if (!is.null(q$criteria)) out$criteria <- lapply(as.list(q$criteria), as_json)
      out
    },
    choice = unclass(jev_choice(q$instructions, q$criteria)),
    score = unclass(jev_score(q$instructions, q$criteria)),
    rlang::abort(sprintf("Question `%s` has unknown type '%s'.", id, q$type))
  )
}

normalise_questions <- function(questions) {
  if (inherits(questions, "jev_question")) {
    rlang::abort("`questions` must be a named list of questions, e.g. list(id = jev_noul(...)).")
  }
  if (!is.list(questions) || length(questions) == 0 ||
      is.null(names(questions)) || any(names(questions) == "")) {
    rlang::abort("`questions` must be a non-empty named list; the names are the answer ids.")
  }
  if (anyDuplicated(names(questions))) {
    rlang::abort("`questions` has duplicated names.")
  }
  purrr::imap(questions, normalise_question)
}
