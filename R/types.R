
# scalar data types ----

is_str <- function(x) is.character(x) && length(x) == 1

is_num <- function(x) is.numeric(x) && length(x) == 1

is_bool <- function(x) is.logical(x) && length(x) == 1

is_scalar <- function(x)
    is_str(x) || is_num(x) || is_bool(x) ||
    inherits(x, c("cql2_math_op", "cql2_minus_op",
                  "cql2_time", "cql2_date", "cql2_interval",
                  "cql2_prop_ref", "cql2_func"))

# input check ----

# check timestamp instant
is_time <- function(x) is_str(x) && grep_iso_3339_date_time(x)

# check date instant
is_date <- function(x) is_str(x) && grep_iso_3339_date(x)

# check temporal instant
is_temporal <- function(x) is_time(x) || is_date(x)

# check property name
prop_ident <- "^[a-zA-Z]+[0-9a-zA-Z:.$_]*$"

is_prop_name <- function(x) is_str(x) && grepl(prop_ident, x)

# check list (array)
is_lst <- function(x) is.list(x) && is.null(names(x))

# check object (named members)
is_obj <- function(x) is.list(x) && !is.null(names(x)) && all(names(x) != "")

# check Boolean expression
is_bool_expr <- function(x)
    inherits(x, c("cql2_logic_op", "cql2_not_op",
                  "cql2_comp_op", "cql2_like_op",
                  "cql2_between_op", "cql2_inlist_op",
                  "cql2_isnull_op", "cql2_spat_pred",
                  "cql2_temp_pred", "cql2_array_pred", "logical"))

# check is_null operand
is_isnull_operand <- function(x)
    is_str(x) || is_num(x) || is_bool(x) ||
    inherits(x, c("cql2_time", "cql2_date", "cql2_interval",
                  "cql2_prop_ref", "cql2_func", "cql2_geom"))

# check number expressions
is_num_expr <- function(x)
    is_num(x) ||
    inherits(x, c("cql2_math_op", "cql2_minus_op",
                  "cql2_prop_ref", "cql2_func"))

# check character expression
is_str_expr <- function(x)
    is_str(x) || inherits(x, c("cql2_casei_op", "cql2_accenti_op",
                               "cql2_prop_ref", "cql2_func"))

# check pattern expression
is_patt_expr <- function(x)
    is_str(x) || inherits(x, c("cql2_casei_op", "cql2_accenti_op"))

# check list of scalars (at least one element)
is_scalar_lst <- function(x)
    is_lst(x) && length(x) > 0 && all(vapply(x, is_scalar, TRUE))
