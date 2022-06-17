#' Convert R expressions to CQL2
#'
#' @description
#' These functions convert R expressions to CQL2 standard (text or JSON).
#' operators
#'
#' @name cql2
#'
#' @param expr  An R expression to be represented in CQL2
#'
#' @examples
#' # basic cql2 examples
#' cql2_text("This is a literal string.")
#' cql2_text("Via dell'Avvento")
#' cql2_text(-100)
#' cql2_text(3.14159)
#' cql2_text(TRUE)
#' cql2_text(FALSE)
#' cql2_text(time("1969-07-20T20:17:40Z")) # also works with timestamp()
#' cql2_json(time("1969-07-20T20:17:40Z"))
#' cql2_text(date("1969-07-20"))
#' cql2_json(date("1969-07-20"))
#' cql2_text(interval("1969-07-16", "1969-07-24"))
#' cql2_text(interval("1969-07-16T05:32:00Z", "1969-07-24T16:50:35Z"))
#' cql2_text(interval("2019-09-09", ".."))
#' cql2_json(interval("1969-07-16", "1969-07-24"))
#' cql2_json(interval("1969-07-16T05:32:00Z", "1969-07-24T16:50:35Z"))
#' cql2_json(interval("2019-09-09", ".."))
#' cql2_text(city == "Toronto")
#' cql2_json(city == "Toronto")
#' cql2_text(balance - 150.0 > 0)
#' cql2_json(balance - 150.0 > 0)
#' cql2_text(updated >= date('1970-01-01'))
#' cql2_json(updated >= date('1970-01-01'))
#' cql2_text(!is_null(geometry))
#' cql2_json(!is_null(geometry))
NULL

to_cql2 <- function(expr) {
    expr <- unquote(expr, parent.frame())
    env <- cql2_env(expr)
    eval(expr, env)
}

#' @rdname cql2
#' @export
cql2_text <- function(expr) {
    expr <- substitute(expr, environment())
    to_text(to_cql2(expr))
}

#' @rdname cql2
#' @export
cql2_json <- function(expr) {
    expr <- substitute(expr, environment())
    to_json(to_cql2(expr))
}
