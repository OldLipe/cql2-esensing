
#---- cql2 classes ----

and_expr <- function(a, b) {
    stopifnot(is_bool_expr(a))
    stopifnot(is_bool_expr(b))
    structure(list(op = "and", args = list(a, b)), class = "cql2_and_expr")
}

or_expr <- function(a, b) {
    stopifnot(is_bool_expr(a))
    stopifnot(is_bool_expr(b))
    structure(list(op = "or", args = list(a, b)), class = "cql2_or_expr")
}

not_expr <- function(a) {
    stopifnot(is_bool_expr(a))
    structure(list(op = "not", args = list(a)), class = "cql2_not_expr")
}

bin_comp_pred <- function(op) {
    function(a, b) {
        stopifnot(is_scalar(a))
        stopifnot(is_scalar(b))
        structure(list(op = op, args = list(a, b)),
                  class = "cql2_bin_comp_pred")
    }
}

is_null_pred <- function(a) {
    stopifnot(is_null_operand(a))
    structure(list(op = "isNull", args = list(a)),
              class = "cql2_is_null_pred")
}

prop_ref <- function(a) {
    is_prop_name(a)
    structure(list(property = a), class = "cql2_prop_ref")
}

get_all_props <- function(expr) {
    props <- all_names(expr)
    names(props) <- props
    lapply(props, prop_ref)
}

time_inst <- function(x) {
    stopifnot(is_time(x))
    structure(list(timestamp = x), class = "cql2_time_inst")
}

date_inst <- function(x) {
    stopifnot(is_date(x))
    structure(list(date = x), class = "cql2_date_int")
}

interval_lit <- function(start = "..", end = "..") {
    if (start != "..")
        stopifnot(is_temporal(start))
    if (end != "..")
        stopifnot(is_temporal(end))
    structure(list(interval = list(start, end)), class = "cql2_interval_lit")
}

cql2_env <- function(expr) {

    # update environment with all input properties
    basic_r <- list2env(get_all_props(expr), parent = emptyenv(),
                        hash = TRUE)

    # basic R expressions
    list2env(list(
        `{` =  `{`,
        `(` =  `(`,
        list = list,
        c =    list
    ), envir = basic_r)

    # basic cql2 expressions
    basic_cql2 <- new_env(
        # and expression
        `&&` = and_expr,
        `&` =  and_expr,
        # or expression
        `||` = or_expr,
        `|` =  or_expr,
        # not expression
        `!` =  not_expr,
        # comparison predicate
        # binary comparison predicate
        `==` = bin_comp_pred("="),
        `!=` = bin_comp_pred("<>"),
        `<` =  bin_comp_pred("<"),
        `>` =  bin_comp_pred(">"),
        `<=` = bin_comp_pred("<="),
        `>=` = bin_comp_pred(">="),
        # is null predicate,
        `is_null` = is_null_pred,
        # temporal literal
        timestamp = time_inst,
        date = date_inst,
        interval = interval_lit,
        parent_env = basic_r
    )

    basic_cql2
}
