#' List the models available to your account
#'
#' Calls `GET /v1/models`. Versioned ids such as `"jev-1.13.0"` are accepted by
#' [jev()] whether or not they appear in the list.
#'
#' @inheritParams jev
#' @return A tibble with one row per model or alias and whatever fields the API
#'   returns (typically `name`, `description` and a release date).
#' @examples
#' \dontrun{
#' jev_models()
#' }
#' @export
jev_models <- function(api_key = jev_api_key()) {
  if (!nzchar(api_key)) {
    rlang::abort("No API key. Set the TYPESAFE_API_KEY environment variable or pass `api_key`.")
  }
  resp <- httr2::request(jev_base_url()) |>
    httr2::req_url_path_append("v1", "models") |>
    httr2::req_user_agent(jev_user_agent()) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_error(body = jev_error_body) |>
    httr2::req_perform()
  body <- httr2::resp_body_json(resp)
  items <- body$data %||% body$models %||% body
  if (!is.list(items) || length(items) == 0) return(tibble::tibble())
  purrr::map(items, function(m) tibble::as_tibble(lapply(m, function(v) {
    if (is.null(v)) NA else if (is.list(v)) list(v) else v
  }))) |>
    purrr::list_rbind()
}
