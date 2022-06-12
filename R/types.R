# 6.3.1 scalar data types ----
is_str <- function(x) is.character(x) && length(x) == 1

is_num <- function(x) is.numeric(x) && length(x) == 1

is_bool <- function(x) is.logical(x) && length(x) == 1

is_time <- function(x) is_str(x) && grep_iso_3339_date_time(x)

is_date <- function(x) is_str(x) && grep_iso_3339_date(x)

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
    structure(list(timestamp = x), class = "cql2_time")
}

date <- function(x) {
    stopifnot(is_date(x))
    structure(list(date = x), class = "cql2_date")
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
    structure(list(interval = list(start, end)), class = "cql2_interval")
}

# 6.3.4 arrays ----
is_lst <- function(x) is.list(x) && is.null(names(x))

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

# 6.5 standard comparisons predicate ----


# character representation ----
curly <- function(x) paste0("{ ", paste(x, collapse = ", "), " }")
bracket <- function(x) paste0("[ ", paste(x, collapse = ", "), " ]")
quote <- function(x) paste0('"', x, '"')
`%:%` <- function(x, y) paste0(x, ": ", y)

#' @exportS3Method
as.character.cql2_time <- function(x) paste0("TIMESTAMP('", unclass(x), "')")

#' @exportS3Method
as.character.cql2_date <- function(x) paste0("DATE('", unclass(x), "')")

#' @exportS3Method
as.character.cql2_interval <- function(x)
    paste0("INTERVAL('", x$interval[[1]], "', '", x$interval[[2]], "')")

#' @exportS3Method
as.character.cql2_array <- function(x) bracket(lapply(x, paste0))

#' @exportS3Method
as.character.cql2_prop <- function(x)
    curly(quote(names(x)) %:% quote(unname(unclass(x))))

#' @exportS3Method
as.character.cql2_func <- function(x)
    curly(quote("function") %:% curly(c(
        quote("name") %:% quote(x$`function`$name),
        quote("args") %:% x$`function`$args
    )))

#' @exportS3Method
print.cql2_time <- function(x) cat(paste0(x), sep = "")

#' @exportS3Method
print.cql2_date <- function(x) cat(paste(x), sep = "")

#' @exportS3Method
print.cql2_interval <- function(x) cat(paste(x), sep = "")

#' @exportS3Method
print.cql2_array <- function(x) cat(paste0(x), sep = "")

#' @exportS3Method
print.cql2_prop <- function(x) cat(paste0(x), sep = "")

#' @exportS3Method
print.cql2_func <- function(x) cat(paste0(x), sep = "")



# convert to text ----

to_cql2_text <- function(expr) UseMethod("to_cql2_text", expr)

#' @exportS3Method
to_cql2_text.character <- function(expr) {
    escape(expr)
}

escape <- function(x) gsub("'", "''", x)

unescape <- function(x) gsub("''", "'", x)

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
    if (is_literal(x)) {
        "constant"
    } else if (is.symbol(x)) {
        "symbol"
    } else if (is.call(x)) {
        "call"
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


    env
}

to_cql2 <- function(expr) {
    expr <- substitute(expr, environment())
    env <- cql2_env(expr)
    eval(expr, env)
}
