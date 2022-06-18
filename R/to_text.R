
#---- convert to text ----

text_quote <- function(x) paste0("'", x, "'")

text_lst <- function(v) paste0("[ ", paste0(v, collapse = ", "), " ]")

escape <- function(x) gsub("'", "''", x)

text_logic_not_op <- function(x) UseMethod("text_logic_not_op", x$args[[1]])

#' @exportS3Method
text_logic_not_op.cql2_isnull_op <- function(x)
    paste(to_text(x$args[[1]]$args[[1]]), "IS NOT NULL")

#' @exportS3Method
text_logic_not_op.default <- function(x)
    paste("NOT", to_text(x$args[[1]]))

to_text <- function(x) UseMethod("to_text", x)

#' @exportS3Method
to_text.character <- function(x) text_quote(escape(x))

#' @exportS3Method
to_text.numeric <- function(x) paste0(x)

#' @exportS3Method
to_text.integer <- function(x) paste0(x)

#' @exportS3Method
to_text.logical <- function(x) if (x) "true" else "false"

#' @exportS3Method
to_text.list <- function(x) {
    if (is_lst(x))
        text_lst(lapply(x, to_text))
    else
        stop("cannot convert list object to cql2 text", call. = FALSE)
}

#' @exportS3Method
to_text.cql2_logic_bin_op <- function(x)
    paste(to_text(x$args[[1]]), toupper(x$op), to_text(x$args[[2]]))

#' @exportS3Method
to_text.cql2_logic_not_op <- function(x) {
    text_logic_not_op(x)
}

#' @exportS3Method
to_text.cql2_comp_bin_op <- function(x)
    paste(to_text(x$args[[1]]), x$op, to_text(x$args[[2]]))

#' @exportS3Method
to_text.cql2_isnull_op <- function(x)
    paste(to_text(x$args[[1]]), "IS NULL")

#' @exportS3Method
to_text.cql2_math_bin_op <- function(x)
    paste(to_text(x$args[[1]]), x$op, to_text(x$args[[2]]))

#' @exportS3Method
to_text.cql2_math_minus_op <- function(x) {
    if (length(x$args) == 1)
        paste0(x$op, to_text(x$args[[1]]))
    else
        paste(to_text(x$args[[1]]), x$op, to_text(x$args[[2]]))
}

#' @exportS3Method
to_text.cql2_prop_ref <- function(x)
    x$property[[1]]

#' @exportS3Method
to_text.cql2_time_inst <- function(x)
    paste0("TIMESTAMP(", to_text(x$timestamp), ")")

#' @exportS3Method
to_text.cql2_date_inst <- function(x)
    paste0("DATE(", to_text(x$date), ")")

#' @exportS3Method
to_text.cql2_interval_lit <- function(x)
    paste0("INTERVAL(", to_text(x$interval[[1]]), ",",
           to_text(x$interval[[2]]), ")")

#' @exportS3Method
to_text.default <- function(x)
    stop(paste("cannot handle value of class", class(x)), call. = FALSE)
