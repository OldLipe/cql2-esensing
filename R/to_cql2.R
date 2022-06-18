
#---- cql2 classes ----

new_logic_bin_op <- function(op) {
    function(a, b) {
        stopifnot(is_bool_expr(a))
        stopifnot(is_bool_expr(b))
        structure(list(op = op, args = list(a, b)),
                  class = "cql2_logic_bin_op")
    }
}

logic_not_op <- function(a) {
    stopifnot(is_bool_expr(a))
    structure(list(op = "not", args = list(a)), class = "cql2_logic_not_op")
}

new_comp_bin_op <- function(op) {
    function(a, b) {
        stopifnot(is_scalar(a))
        stopifnot(is_scalar(b))
        structure(list(op = op, args = list(a, b)),
                  class = "cql2_comp_bin_op")
    }
}

isnull_op <- function(a) {
    stopifnot(is_null_operand(a))
    structure(list(op = "isNull", args = list(a)),
              class = "cql2_isnull_op")
}

new_math_bin_op <- function(op) {
    function(a, b = NULL) {
        stopifnot(is_num_expr(a))
        stopifnot(is_num_expr(b))
        structure(list(op = op, args = list(a, b)),
                  class = "cql2_math_bin_op")
    }
}

math_minus_op <- function(a, b) {
    stopifnot(is_num_expr(a))
    if (missing(b))
        args <- list(a)
    else {
        stopifnot(is_num_expr(b))
        args <- list(a, b)
    }
    structure(list(op = "-", args = args), class = "cql2_math_minus_op")
}

prop_ref <- function(a) {
    stopifnot(is_prop_name(a))
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
    structure(list(date = x), class = "cql2_date_inst")
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
        }
    ), envir = basic_r)

    # basic cql2 expressions
    basic_cql2 <- new_env(
        # and, or, not expressions
        `&&` = new_logic_bin_op("and"),
        `&` =  new_logic_bin_op("and"),
        `||` = new_logic_bin_op("or"),
        `|` =  new_logic_bin_op("or"),
        `!` =  logic_not_op,
        # comparison predicate
        # binary comparison predicate
        `==` = new_comp_bin_op("="),
        `!=` = new_comp_bin_op("<>"),
        `<` =  new_comp_bin_op("<"),
        `>` =  new_comp_bin_op(">"),
        `<=` = new_comp_bin_op("<="),
        `>=` = new_comp_bin_op(">="),
        # is null predicate,
        `is_null` = isnull_op,
        # basic arithmetic expression
        `-` = math_minus_op, # can be both binary and unary operator
        `+` = new_math_bin_op("+"),
        `*` = new_math_bin_op("*"),
        `/` = new_math_bin_op("/"),
        # temporal literal
        timestamp =  time_inst,
        time =       time_inst,
        date =       date_inst,
        interval =   interval_lit,
        parent_env = basic_r
    )

    basic_cql2
}
