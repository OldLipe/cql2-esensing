# boolean expression ----

is_bool_expr <- function(x) {
    inherits(x, c("cql2_and_expr", "cql2_or_expr", "cql2_not_expr",
                  "cql2_comp_bin_op", "cql2_like_pred",
                  "cql2_is_between_pred", "cql2_is_inlist_pred",
                  "cql2_isnull_op", "cql2_spat_pred",
                  "cql2_temp_pred", "cql2_array_pred", "logical"))
}

is_null_operand <- function(x)
    is_str(x) || is_num(x) || is_bool(x) ||
    inherits(x, c("cql2_time_inst", "cql2_date_inst",
                  "cql2_prop_ref", "cql2_func", "cql2_geom"))

is_num_expr <- function(x)
    is_num(x) ||
    inherits(x, c("cql2_math_bin_op", "cql2_prop_ref", "cql2_func"))

# scalar data types ----

is_str <- function(x) is.character(x) && length(x) == 1

is_num <- function(x) is.numeric(x) && length(x) == 1

is_bool <- function(x) is.logical(x) && length(x) == 1

is_scalar <- function(x)
    is_str(x) || is_num(x) || is_bool(x) ||
    inherits(x, c("cql2_math_bin_op", "cql2_time_inst", "cql2_date_inst",
                  "cql2_prop_ref", "cql2_func"))

# input check ----

is_time <- function(x) is_str(x) && grep_iso_3339_date_time(x)

is_date <- function(x) is_str(x) && grep_iso_3339_date(x)

is_temporal <- function(x) is_time(x) || is_date(x)

prop_ident <- "^[a-zA-Z]+[0-9a-zA-Z:.$_]*$"

is_prop_name <- function(x) is_str(x) && grepl(prop_ident, x)

is_lst <- function(x) is.list(x) && is.null(names(x))

is_obj <- function(x) is.list(x) && !is.null(names(x)) && all(names(x) != "")




is_vec <- function(x) is.call(x) && paste0(x[[1]]) %in% c("list", "c", ":")

call_args <- function(x) unname(as.list(x)[-1])

is_literal <- function(x) {
    switch(class(x),
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

all_names_r <- function(x) {
    switch_expr(x,
                constant = character(),
                symbol =   paste0(x),
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
                    fname <- paste0(x[[1]])
                    children <- unlist(lapply(as.list(x[-1]), all_calls))
                    c(fname, children)
                })
}

all_calls <- function(x) {
    unique(all_calls_r(x))
}

new_env <- function(..., parent_env = emptyenv()) {
    dots <- list(...)
    list2env(dots, envir = NULL, parent = parent_env, hash = TRUE)
}
