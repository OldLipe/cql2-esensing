# 6.3.1 scalar data types ----
is_str <- function(x) is.character(x) && length(x) == 1

is_num <- function(x) is.numeric(x) && length(x) == 1

is_bool <- function(x) is.logical(x) && length(x) == 1

is_time <- function(x)
    inherits(x, "cql2_time") || (is_str(x) && grep_iso_3339_date_time(x))

is_date <- function(x)
    inherits(x, "cql2_date") || (is_str(x) && grep_iso_3339_date(x))

is_scalar <- function(x) {
    switch(class(x),
           character = , numeric = , integer = , logical = {
               length(x) == 1
           },
           cql2_time = , cql2_date = , cql2_prop = , cql2_func = {
               TRUE
           },
           FALSE
    )
}

# 6.3.2 spatial data types ----



# 6.3.3 temporal data types ----

time <- function(x) {
    stopifnot(is_time(x))
    structure(list(timestamp = unlist(x)), class = "cql2_time")
}

date <- function(x) {
    stopifnot(is_date(x))
    structure(list(date = unlist(x)), class = "cql2_date")
}

dotdot <- ".."

.. <- NULL

interval <- function(start = .., end = ..) {
    if (!is.null(start))
        stopifnot(is_date(start) || is_time(start))
    else start <- dotdot
    if (!is.null(end))
        stopifnot(is_date(end) || is_time(end))
    else end <- dotdot
    structure(list(interval = list(unlist(start), unlist(end))),
              class = "cql2_interval")
}


# 6.3.4 arrays ----
is_lst <- function(x) length(x) > 1 && is.null(names(x))

lst <- function(...) {
    dots <- list(...)
    stopifnot(is_lst(dots))
    structure(dots, class = "cql2_array")
}


# 6.4 property references ----

identifier <- "^[a-zA-Z]+[0-9a-zA-Z:.$_]*$"

is_prop <- function(x) is_str(x) && grepl(identifier, x)

prop <- function(x) {
    stopifnot(is_prop(x))
    structure(list(property = x), class = "cql2_prop")
}

prop_name <- function(x) x$property

func <- function(x) {
    rlang::new_function(
        args = rlang::exprs(... = ),
        body = rlang::expr(
            structure(
                list(`function` = list(name = !!x, args = lst(...))),
                class = "cql2_func")
        ),
        env = rlang::caller_env()
    )
}

func_name <- function(x) x$`function`$name

func_args <- function(x) x$`function`$args

# 6.5 standard comparisons predicate ----

# convert to text ----
bracket <- function(x) paste0("[ ", x, " ]")
text_quote <- function(x) paste0("'", x, "'")
text_func <- function(name, args)
    paste0(name, "(", paste0(lapply(args, to_text), collapse = ", "), ")")

escape <- function(x) gsub("'", "''", x)

to_text <- function(x) UseMethod("to_text", x)

#' @exportS3Method
to_text.character <- function(x) {
    if (is_str(x))
        text_quote(escape(x))
    else if (is_lst(x))
        bracket(paste0(lapply(x, to_text), collapse = ", "))
}

#' @exportS3Method
to_text.numeric <- function(x) {
    if (is_num(x))
        x
    else if (is_lst(x))
        bracket(paste0(lapply(x, to_text), collapse = ", "))
}

#' @exportS3Method
to_text.integer <- function(x) to_text.numeric(x)

#' @exportS3Method
to_text.logical <- function(x) {
    if (is_bool(x))
        if (x) "true" else "false"
    else if (is_lst(x))
        bracket(paste0(lapply(x, to_text), collapse = ", "))
}

#' @exportS3Method
to_text.list <- function(x) {
    if (is_lst(x))
        bracket(paste0(lapply(x, to_text), collapse = ", "))
    else
        stop("cannot convert object to a cql2 text", call. = FALSE)
}

#' @exportS3Method
to_text.cql2_time <- function(x) text_func("TIMESTAMP", x$timestamp)

#' @exportS3Method
to_text.cql2_date <- function(x) text_func("DATE", x$date)

#' @exportS3Method
to_text.cql2_interval <- function(x) text_func("INTERVAL", x$interval)

#' @exportS3Method
to_text.cql2_prop <- function(x) to_text(x[[1]])

#' @exportS3Method
to_text.cql2_func <- function(x) text_func(func_name(x), func_args(x))

#' @exportS3Method
to_text.cql2_expr <- function(x) to_text(x[[1]])

#' @exportS3Method
to_text.default <- function(x) to_text(unclass(x))


# convert to json ----

json_quote <- function(x) paste0('"', x, '"')

curly <- function(k, v)
    paste0("{ ", paste0(k, ": ", v, collapse = ", "), " }")

is_obj <- function(x)
    is.list(x) && !is.null(names(x)) && all(names(x) != "")

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
        bracket(paste0(lapply(x, to_json), collapse = ", "))
    else if (is_obj(x))
        curly(json_quote(names(x)), lapply(x, to_json))
    else
        stop("cannot convert list to a valid json", call. = FALSE)
}

#' @exportS3Method
to_json.default <- function(x) to_json(unclass(x))

#' @exportS3Method
print.cql2_expr <- function(x) cat(to_text(x[[1]]), sep = "")

#' @exportS3Method
print.cql2_time <- function(x) cat(to_text(x), sep = "")

#' @exportS3Method
print.cql2_date <- function(x) cat(to_text(x), sep = "")

#' @exportS3Method
print.cql2_interval <- function(x) cat(to_text(x), sep = "")

#' @exportS3Method
print.cql2_array <- function(x) cat(to_text(x), sep = "")

#' @exportS3Method
print.cql2_prop <- function(x) cat(to_text(x), sep = "")

#' @exportS3Method
print.cql2_func <- function(x) cat(to_text(x), sep = "")

is_literal <- function(x) {
    switch(class(x),
           character = , numeric = , integer = , logical = ,
           cql2_time = , cql2_date = {
               length(x) == 1
           },
           FALSE
    )
}

expr_type <- function(x) {
    if (is.symbol(x)) {
        "symbol"
    } else if (is.call(x)) {
        "call"
    } else if (is_literal(x)) {
        "constant"
    } else {
        typeof(x)
    }
}

switch_expr <- function(x, ...) {
    switch(expr_type(x), ...,
           stop("cannot handle type '", class(x), "'", call. = FALSE))
}

all_names_r <- function(x) {
    switch_expr(x,
                constant = character(),
                symbol =   as.character(x),
                call =     unlist(lapply(as.list(x[-1]), all_names),
                                  use.names = FALSE))
}

all_names <- function(x) {
    unique(all_names_r(x))
}

all_calls_r <- function(x) {
    switch_expr(x,
                constant = ,
                symbol =   character(),
                call =     {
                    fname <- as.character(x[[1]])
                    children <- unlist(lapply(as.list(x[-1]), all_calls))
                    c(fname, children)
                })
}

all_calls <- function(x) {
    unique(all_calls_r(x))
}

make_vars <- function(var_names, fn_make, parent_env) {
    env <- new.env(parent = parent_env)
    values <- lapply(var_names, fn_make)
    for (i in seq_along(var_names)) {
        assign(var_names[[i]], values[[i]], envir = env)
    }
    env
}

clone_fn <- function(fn_name, parent_env) {
    env <- new.env(parent = parent_env)
    for (i in seq_along(fn_name)) {
        fn <- get(fn_name[[i]], envir = parent.env(environment()),
                  inherits = FALSE)
        environment(fn) <- env
        assign(fn_name[[i]], fn, envir = env)
    }
    env
}

cql2_env <- function(expr) {

    # add all unknown properties
    env <- make_vars(var_names = all_names(expr), fn_make = prop,
                     parent_env = parent.env(environment()))

    # add all unknown functions
    env <- make_vars(var_names = all_calls(expr), fn_make = func,
                     parent_env = env)

    # add date time functions
    env <- clone_fn(c("time", "date", "interval"), parent_env = env)

    # add binary comparison operators


    # add utility functions
    env$list <- list
    env$c <- c
    env$paste <- paste

    env
}

is_bang <- function(x)
    is.call(x) && length(x) == 2 && paste0(x[[1]]) == "!"

is_bangbang <- function(x)
    is_bang(x) && is_bang(x[[2]])

get_bangbang <- function(x) x[[2]][[2]]

unquote <- function(expr, env) {
    if (is.pairlist(expr))
        as.pairlist(lapply(expr, unquote, env = env))
    else if (is.call(expr)) {
        if (is_bangbang(expr))
            eval(get_bangbang(expr), env)
        else
            as.call(lapply(expr, unquote, env = env))
    }
    else expr
}

to_cql2 <- function(expr) {
    expr <- substitute(expr, environment())
    expr <- unquote(expr, parent.frame())
    env <- cql2_env(expr)

    structure(list(eval(expr, env)), class = "cql2_expr")
}
