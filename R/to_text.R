
# convert to text ----

text_quote <- function(x) paste0("'", x, "'")

text_lst <- function(v) paste0("[ ", paste0(v, collapse = ", "), " ]")

text_args <- function(v) paste0(v, collapse = ", ")

text_call <- function(f, args)
    paste0(paste(f), "(", text_args(lapply(args, to_text)), ")")

escape <- function(x) gsub("'", "''", x)

to_text <- function(x) UseMethod("to_text", x)

#' @exportS3Method
to_text.character <- function(x) text_quote(escape(x))

#' @exportS3Method
to_text.numeric <- function(x) x

#' @exportS3Method
to_text.integer <- function(x) x

#' @exportS3Method
to_text.logical <- function(x) if (x) "true" else "false"

#' @exportS3Method
to_text.list <- function(x) {
    if (is_lst(x))
        text_lst(lapply(x, to_text))
    else
        stop("cannot convert object to a cql2 text", call. = FALSE)
}

#' @exportS3Method
to_text.name <- function(x) paste(x)

#' @exportS3Method
to_text.call <- function(x) text_call(x[[1]], args(x))

text_time <- function(x) {
    stopifnot(is_time(x))
    to_text(call("TIMESTAMP", x))
}

text_date <- function(x) {
    stopifnot(is_date(x))
    to_text(call("DATE", x))
}

text_interval <- function(start = NULL, end = NULL) {
    if (!is.null(start)) {
        stopifnot(is_temporal(start))
    } else start <- ".."
    if (!is.null(end)) {
        stopifnot(is_temporal(end))
    } else end <- ".."
    to_text(call("INTERVAL", start, end))
}

text_property <- function(x) {
    stopifnot(is_property(x))
    to_text(as.symbol(x))
}

text_function <- function(x) {
    rlang::new_function(
        args = rlang::exprs(... = ),
        body = rlang::expr(
            to_text(call(!!x, ...))
        ),
        env = rlang::caller_env()
    )
}

text_cql2_env <- function(expr) {

    # add all unknown properties
    env <- make_vars(var_names = all_names(expr), fn_make = text_property,
                     parent_env = parent.env(environment()))

    # add all unknown functions
    env <- make_vars(var_names = all_calls(expr), fn_make = text_function,
                     parent_env = env)

    # add date time functions
    env <- clone_fn(time =       text_time,
                    date =       text_date,
                    interval =   text_interval,
                    list =       list,
                    c =          list,
                    `:` =        `:`,
                    paste0 =     paste0,
                    paste =      paste,
                    parent_env = env)

    # add binary comparison operators

    env
}

cql2_to_text <- function(expr) {
    expr <- substitute(expr, environment())
    expr <- unquote(expr, parent.frame())
    env <- text_cql2_env(expr)
    eval(expr, env)
}
