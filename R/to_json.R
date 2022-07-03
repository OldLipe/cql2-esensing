
# ---- auxiliary functions ----

json_quote <- function(x) paste0('"', x, '"')

json_lst <- function(x)
    paste0("[ ", paste0(lapply(x, to_json), collapse = ", "), " ]")

json_obj <- function(x)
    paste0("{ ", paste0(json_quote(names(x)), ": ",
                        unname(lapply(x, to_json)), collapse = ", "), " }")

# ---- convert to json ----

to_json <- function(x) UseMethod("to_json", x)

#' @exportS3Method
to_json.character <- function(x) json_quote(x)

#' @exportS3Method
to_json.numeric <- function(x) paste0(x)

#' @exportS3Method
to_json.integer <- function(x) paste0(x)

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
to_json.sf <- function(x) to_json.sfc(x$geometry)

# TODO: this can be implemented, use this package for testing
#' @exportS3Method
to_json.sfc <- function(x) geojsonsf::sfc_geojson(x)

#' @exportS3Method
to_json.cql2_logic_op <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_not_op <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_comp_op <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_isnull_op <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_math_op <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_minus_op <- function(x) {
    if (length(x$args) == 1 && is_num(x$args[[1]]))
        paste0(-x$args[[1]])
    else
        json_obj(x)
}

#' @exportS3Method
to_json.cql2_spatial_op <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_prop_ref <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_time <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_date <- function(x) json_obj(x)

#' @exportS3Method
to_json.cql2_interval <- function(x) json_obj(x)

#' @exportS3Method
to_json.default <- function(x)
    stop(paste("cannot handle value of class ", class(x)), call. = FALSE)
