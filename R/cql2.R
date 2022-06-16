#' Convert expression to CQL2 format
#'
#' @name cql2
#'
#' @param expr  An R expression to be represented in CQL2
#'
#'
#' @examples
#'
#' cql2_to_text(datetime >= date("2020-01-01"))
#'
#' cql2_to_json(`eo:cloud_cover` < 0.2)
#'
NULL

#' @rdname cql2
#' @export
cql2_to_text <- function(expr) {
    expr <- substitute(expr, environment())
    expr <- unquote(expr, parent.frame())
    env <- text_cql2_env(expr)
    to_text(eval(expr, env))
}

#' @rdname cql2
#' @export
cql2_to_json <- function(expr) {
    expr <- substitute(expr, environment())
    expr <- unquote(expr, parent.frame())
    env <- json_cql2_env(expr)
    to_json(eval(expr, env))
}
