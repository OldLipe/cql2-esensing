
#---- prep env ----

text_time <- function(x) {
    stopifnot(is_time(x))
    call("TIMESTAMP", x)
}

text_date <- function(x) {
    stopifnot(is_date(x))
    call("DATE", x)
}

text_interval <- function(start = NULL, end = NULL) {
    if (!is.null(start)) {
        stopifnot(is_temporal(start))
    } else start <- ".."
    if (!is.null(end)) {
        stopifnot(is_temporal(end))
    } else end <- ".."
    call("INTERVAL", start, end)
}

text_property <- function(x) {
    stopifnot(is_property(x))
    as.symbol(x)
}

text_function <- function(x) {
    function(...) as.call(list(as.symbol(x), ...))
}

text_op_binary <- function(x) {
    function(a, b) structure(call(x, a, b), class = "cql2_op_bin")
}

is_not_null_op <- function(x)
    is.call(x) && call_name(x) == "NOT" &&
    is.call(x[[2]]) && call_name(x[[2]]) == "IS NULL"

convert_is_not_null_op <- function(x)
    structure(call("IS NOT NULL", x[[2]][[2]]), class = "cql2_op_un_post")

text_op_unary_pre <- function(x) {
    function(a) {
        res <- structure(call(x, a), class = "cql2_op_un_pre")
        if (is_not_null_op(res))
            res <- convert_is_not_null_op(res)
        res
    }
}

text_op_unary_post <- function(x) {
    function(a) structure(call(x, a), class = "cql2_op_un_post")
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
                    `:` =        function(a, b) { as.list(seq(a, b)) },
                    paste0 =     paste0,
                    paste =      paste,
                    parent_env = env)

    # add binary comparison operators
    env <- clone_fn(`==` =          text_op_binary("="),
                    `!=` =          text_op_binary("<>"),
                    `>=` =          text_op_binary(">="),
                    `>` =           text_op_binary(">"),
                    `<=` =          text_op_binary("<="),
                    `<` =           text_op_binary("<"),
                    `!` =           text_op_unary_pre("NOT"),
                    `is_null` =     text_op_unary_post("IS NULL"),
                    `is_not_null` = text_op_unary_post("IS NOT NULL"),
                    parent_env = env)

    env
}


#---- convert to text ----

text_quote <- function(x) paste0("'", x, "'")

text_lst <- function(v) paste0("[ ", paste0(v, collapse = ", "), " ]")

text_args <- function(v) paste0(v, collapse = ", ")

text_fun <- function(x) {
    f <- call_name(x)
    args <- call_args(x)
    paste0(paste(f), "(", text_args(lapply(args, to_text)), ")")
}

text_bin_op <- function(x) {
    args <- call_args(x)
    paste(to_text(args[[1]]), call_name(x), to_text(args[[2]]))
}

text_pre_un_op <- function(x)
    paste(call_name(x), to_text(call_args(x)))

text_post_un_op <- function(x)
    paste(to_text(call_args(x)), call_name(x))

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
to_text.call <- function(x) text_fun(x)

#' @exportS3Method
to_text.cql2_op_bin <- function(x) text_bin_op(x)

#' @exportS3Method
to_text.cql2_op_un_pre <- function(x) text_pre_un_op(x)

#' @exportS3Method
to_text.cql2_op_un_post <- function(x) text_post_un_op(x)

