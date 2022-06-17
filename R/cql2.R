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

to_cql2 <- function(expr) {
    expr <- substitute(expr, environment())
    expr <- unquote(expr, parent.frame())
    env <- cql2_env(expr)
    eval(expr, env)
}

#' @rdname cql2
#' @export
cql2_to_text <- function(expr) {
    to_text(to_cql2(expr))
}

#' @rdname cql2
#' @export
cql2_to_json <- function(expr) {
    to_json(to_cql2(expr))
}
