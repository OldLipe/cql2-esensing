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
