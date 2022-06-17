
#---- convert to json ----

json_quote <- function(x) paste0('"', x, '"')

json_lst <- function(x)
    paste0("[ ", paste0(lapply(x, to_json), collapse = ", "), " ]")

json_obj <- function(x)
    paste0("{ ", paste0(json_quote(names(x)), ": ",
                        unname(lapply(x, to_json)), collapse = ", "), " }")

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
        json_lst(x)
    else if (is_obj(x))
        json_obj(x)
    else
        stop("cannot convert list value to a valid cql2 json", call. = FALSE)
}

#' @exportS3Method
to_json.cql2_and_expr <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_or_expr <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_not_expr <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_comp_bin_op <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_isnull_op <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_math_bin_op <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_prop_ref <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_time_inst <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_date_inst <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_interval_lit <- function(x) json_obj(x)

#' @exportS3Method
to_json.default <- function(x)
    stop(paste("cannot handle value of class ", class(x)), call. = FALSE)
