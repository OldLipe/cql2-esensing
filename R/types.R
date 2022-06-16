# scalar data types ----
is_str <- function(x) is.character(x) && length(x) == 1

is_num <- function(x) is.numeric(x) && length(x) == 1

is_bool <- function(x) is.logical(x) && length(x) == 1

is_time <- function(x)
    is_str(x) && grep_iso_3339_date_time(x)

is_date <- function(x)
    is_str(x) && grep_iso_3339_date(x)

property_identifier <- "^[a-zA-Z]+[0-9a-zA-Z:.$_]*$"

is_property <- function(x) is_str(x) && grepl(property_identifier, x)

is_lst <- function(x) is.list(x) && is.null(names(x))

is_obj <- function(x)
    is.list(x) && !is.null(names(x)) && all(names(x) != "")

# 6.5 standard comparisons predicate ----
is_vec <- function(x)
    is.call(x) && paste0(x[[1]]) %in% c("list", "c", ":")

args <- function(x) unname(as.list(x)[-1])

is_literal <- function(x) {
    switch(class(x),
           character = , numeric = , integer = ,
           logical = TRUE,
           call = {
               if (is_vec(x))
                   all(vapply(args(x), is_literal, TRUE))
               else
                   FALSE
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

clone_fn <- function(..., parent_env) {
    dots <- list(...)
    env <- new.env(parent = parent_env)
    for (n in names(dots)) {
        if (is.function(dots[[n]]))
            environment(dots[[n]]) <- env
        assign(n, dots[[n]], envir = env)
    }
    env
}

#---- auxiliary functions

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
