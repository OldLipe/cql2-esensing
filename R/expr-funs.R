
# ---- unquote !! ----

is_bang <- function(x) {is.call(x) && length(x) == 2 && paste0(x[[1]]) == "!"}

is_bangbang <- function(x) {is_bang(x) && is_bang(x[[2]])}

get_bangbang <- function(x) {x[[2]][[2]]}

unquote <- function(expr, env) {
    if (is.pairlist(expr))
        as.pairlist(lapply(expr, unquote, env = env))
    else if (is.call(expr)) {
        if (is_bangbang(expr)) {
            eval(get_bangbang(expr), env)
        } else {
            as.call(lapply(expr, unquote, env = env))
        }
    }
    else expr
}

#---- switch_expr ----

is_vec <- function(x) is.call(x) && paste0(x[[1]]) %in% c("list", "c", ":")

call_args <- function(x) unname(as.list(x)[-1])

is_literal <- function(x) {
    switch(class(x)[[1]],
           character = , numeric = , integer = ,
           logical =   TRUE,
           call =      {
               if (is_vec(x))
                   all(vapply(call_args(x), is_literal, TRUE))
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

# ---- all names ----

all_names_r <- function(x) {
    switch_expr(x,
                list = list(),
                constant = character(),
                symbol =   paste0(x),
                call =     unlist(lapply(as.list(x[-1]), all_names),
                                  use.names = FALSE))
}

all_names <- function(x) {
    unique(all_names_r(x))
}

# ---- all calls ----

all_calls_r <- function(x) {
    switch_expr(x,
                constant = ,
                symbol =   character(),
                call =     {
                    fname <- paste0(x[[1]])
                    children <- unlist(lapply(as.list(x[-1]), all_calls))
                    c(fname, children)
                })
}

all_calls <- function(x) {
    unique(all_calls_r(x))
}

# ---- new env ----

new_env <- function(..., parent_env = emptyenv()) {
    dots <- list(...)
    list2env(dots, envir = NULL, parent = parent_env, hash = TRUE)
}
