
# ---- cql2 core classes ----

# - cql2_logic_op
# - cql2_not_op
# - cql2_comp_op
# - cql2_isnull_op
# - cql2_math_op
# - cql2_minus_op
# - cql2_time
# - cql2_date
# - cql2_interval
# - cql2_prop_ref

# ---- constructor functions ----

# Boolean expressions
new_logic_op <- function(op) {
    function(a, b) {
        stopifnot(is_bool_expr(a))
        stopifnot(is_bool_expr(b))
        structure(list(op = op, args = list(a, b)),
                  class = "cql2_logic_op")
    }
}

not_op <- function(a) {
    stopifnot(is_bool_expr(a))
    structure(list(op = "not", args = list(a)), class = "cql2_not_op")
}

# binary comparison operators
new_comp_op <- function(op) {
    function(a, b) {
        stopifnot(is_scalar(a))
        stopifnot(is_scalar(b))
        structure(list(op = op, args = list(a, b)),
                  class = "cql2_comp_op")
    }
}

# is_null operator
isnull_op <- function(a) {
    stopifnot(is_isnull_operand(a))
    structure(list(op = "isNull", args = list(a)),
              class = "cql2_isnull_op")
}

# basic math operators
new_math_op <- function(op) {
    function(a, b = NULL) {
        stopifnot(is_num_expr(a))
        stopifnot(is_num_expr(b))
        structure(list(op = op, args = list(a, b)),
                  class = "cql2_math_op")
    }
}

minus_op <- function(a, b) {
    stopifnot(is_num_expr(a))
    if (missing(b))
        args <- list(a)
    else {
        stopifnot(is_num_expr(b))
        args <- list(a, b)
    }
    structure(list(op = "-", args = args), class = "cql2_minus_op")
}

# temporal literals
timestamp <- function(x) {
    stopifnot(is_time(x))
    structure(list(timestamp = x), class = "cql2_timestamp")
}

date <- function(x) {
    stopifnot(is_date(x))
    structure(list(date = x), class = "cql2_date")
}

interval_lit <- function(start = "..", end = "..") {
    if (start != "..")
        stopifnot(is_temporal(start))
    if (end != "..")
        stopifnot(is_temporal(end))
    structure(list(interval = list(start, end)), class = "cql2_interval")
}

# input property identifiers
prop_ref <- function(a) {
    stopifnot(is_prop_name(a))
    structure(list(property = a), class = "cql2_prop_ref")
}

get_all_props <- function(expr) {
    props <- all_names(expr)
    names(props) <- props
    lapply(props, prop_ref)
}

# input property identifiers
func_def <- function(a) {
    stopifnot(is_func_name(a))
    function(...) {

        structure(list(`function` = list(name = a, args = list(...))),
                  class = "cql2_func")
    }
}

get_all_funcs <- function(expr) {
    funcs <- all_calls(expr)
    names(funcs) <- funcs
    lapply(funcs, func_def)
}

# ---- cql2 environments ----

# environment of expression's identifiers (i.e. properties and functions)
cql2_ident_env <- new_env(parent_env = emptyenv())

# environment for cql2 core evaluation
cql2_core_env <- new_env(
    # basic R functions and constants
    `{` =     `{`,
    `(` =     `(`,
    `T` =     TRUE,
    `TRUE` =  TRUE,
    `F` =     FALSE,
    `FALSE` = FALSE,
    list =    list,
    c =       list,
    `:` =     function(from, to) {
        stopifnot(is_num(from))
        stopifnot(is_num(to))
        as.list(seq(from, to))
    },
    # cql2 basic expressions
    # Boolean expressions
    `&&` = new_logic_op("and"),
    `&` =  new_logic_op("and"),
    `||` = new_logic_op("or"),
    `|` =  new_logic_op("or"),
    `!` =  not_op,
    # comparison predicate
    # binary comparison operators
    `==` = new_comp_op("="),
    `!=` = new_comp_op("<>"),
    `<` =  new_comp_op("<"),
    `>` =  new_comp_op(">"),
    `<=` = new_comp_op("<="),
    `>=` = new_comp_op(">="),
    # is_null operator
    `is_null` = isnull_op,
    # basic math operators
    `-` = minus_op, # can be both binary and unary operator
    `+` = new_math_op("+"),
    `*` = new_math_op("*"),
    `/` = new_math_op("/"),
    # temporal literals
    timestamp =  timestamp,
    date =       date,
    interval =   interval_lit,
    parent_env = cql2_ident_env
)

cql2_env <- function(expr) {

    # order of evaluation:
    # cql2_env --> cql2_core_env --> cql2_ident_env.
    # update `ident_env` environment with all input properties
    list2env(get_all_props(expr), envir = cql2_ident_env)
    list2env(get_all_funcs(expr), envir = cql2_ident_env)

    cql2_core_env
    cql2_adv_comp_env
}

# ---- convert to cql2 ----

to_cql2 <- function(expr) {
    expr <- unquote(expr, parent.frame())
    eval(expr, cql2_env(expr))
}
