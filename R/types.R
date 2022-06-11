# basic validation ----
is_str <- function(x) is.character(x) && length(x) == 1

is_num <- function(x) is.numeric(x) && length(x) == 1

is_bool <- function(x) is.logical(x) && length(x) == 1

is_time <- function(x) is_str(x) && grep_iso_3339_date_time(x)

is_date <- function(x) is_str(x) && grep_iso_3339_date(x)

is_literal <- function(x) {
    switch(class(x),
        `NULL` = TRUE,
        character = , integer = , logical = , cql2_time = , cql2_date = {
            length(x) == 1
        },
        FALSE
    )
}

is_array <- function(x) is.list(x) && is.null(names(x))

expr_type <- function(x) {
    if (is_literal(x)) {
        "constant"
    } else if (is.symbol(x)) {
        "symbol"
    } else if (is.call(x)) {
        "call"
    } else if (is.pairlist(x)) {
        "pairlist"
    } else {
        typeof(x)
    }
}

switch_expr <- function(x, ...) {
    switch(expr_type(x), ...,
           stop("cannot handle type '", class(x), "'", call. = FALSE))
}

# escape ----
quote <- "'"

escaped_quote <- "''"

escape <- function(x) gsub(quote, escaped_quote, x)

unescape <- function(x) gsub(escaped_quote, quote, x)

# date time ----
time <- function(x) {
    stopifnot(is_time(x))
    structure(list(timestamp = x), class = "cql2_time")
}

#' @exportS3Method
print.cql2_time <- function(x) cat("TIMESTAMP('", x, "')", "\n", sep = "")

date <- function(x) {
    stopifnot(is_date(x))
    structure(list(date = x), class = "cql2_date")
}

#' @exportS3Method
print.cql2_date <- function(x) cat("DATE('", x, "')", "\n", sep = "")

dotdot <- ".."

.. <- NULL

interval <- function(start = .., end = ..) {
    if (!is.null(start))
        stopifnot(is_date(start) || is_time(start))
    else start <- dotdot
    if (!is.null(end))
        stopifnot(is_date(end) || is_time(end))
    else end <- dotdot
    structure(list(interval = list(start, end)), class = "cql2_interval")
}

#' @exportS3Method
print.cql2_interval <- function(x)
    cat("INTERVAL('", x$interval[[1]], "', '", x$interval[[2]], "')", sep = "")

# convert to text ----
to_cql2_text <- function(expr) UseMethod("to_cql2_text", expr)

#' @exportS3Method
to_cql2_text.character <- function(expr) {
    escape(expr)
}

all_names_recursive <- function(x) {
    switch_expr(x,
                constant = character(),
                symbol =   as.character(x),
                call =     unlist(lapply(as.list(x[-1]), all_names),
                                  use.names = FALSE)
    )
}

all_names <- function(x) {
    unique(all_names_recursive(x))
}
