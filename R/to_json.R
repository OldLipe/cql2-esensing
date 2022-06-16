
json_quote <- function(x) paste0('"', x, '"')

json_lst <- function(v)
    paste0("[ ", paste0(v, collapse = ", "), " ]")

json_obj <- function(k, v)
    paste0("{ ", paste0(json_quote(k), ": ", v, collapse = ", "), " }")

to_json <- function(x) UseMethod("to_json", x)

#' @exportS3Method
to_json.character <- function(x) json_quote(x)

#' @exportS3Method
to_json.numeric <- function(x) x

#' @exportS3Method
to_json.integer <- function(x) x

#' @exportS3Method
to_json.logical <- function(x) if (x) "true" else "false"

#' @exportS3Method
to_json.list <- function(x) {
    if (is_lst(x))
        json_lst(lapply(x, to_json))
    else if (is_obj(x))
        json_obj(names(x), lapply(x, to_json))
    else
        stop("cannot convert list to a valid json", call. = FALSE)
}

#' @exportS3Method
to_json.default <- function(x)
    stop(paste0("cannot handle object of class ", class(x)), call. = FALSE)

json_time <- function(x) {
    stopifnot(is_time(x))
    to_json(list(timestamp = x))
}

json_date <- function(x) {
    stopifnot(is_date(x))
    to_json(list(date = x))
}

json_interval <- function(start = NULL, end = NULL) {
    if (!is.null(start)) {
        stopifnot(is_temporal(start))
    } else start <- ".."
    if (!is.null(end)) {
        stopifnot(is_temporal(end))
    } else end <- ".."
    to_json(list(interval = list(start, end)))
}

json_property <- function(x) {
    stopifnot(is_property(x))
    to_json(list(property = x))
}

json_function <- function(x) {
    rlang::new_function(
        args = rlang::exprs(... = ),
        body = rlang::expr(
            to_json(list(`function` = list(name = !!x, args = list(...))))
        ),
        env = rlang::caller_env()
    )
}

json_cql2_env <- function(expr) {

    # add all unknown properties
    env <- make_vars(var_names = all_names(expr), fn_make = json_property,
                     parent_env = parent.env(environment()))

    # add all unknown functions
    env <- make_vars(var_names = all_calls(expr), fn_make = json_function,
                     parent_env = env)

    # add date time functions
    env <- clone_fn(time =       json_time,
                    date =       json_date,
                    interval =   json_interval,
                    list =       list,
                    c =          list,
                    `:` =        `:`,
                    paste0 =     paste0,
                    paste =      paste,
                    parent_env = env)

    # add binary comparison operators

    env
}

cql2_to_json <- function(expr) {
    expr <- substitute(expr, environment())
    expr <- unquote(expr, parent.frame())
    env <- json_cql2_env(expr)
    eval(expr, env)
}
