
# ---- auxiliary functions ----

text_quote <- function(x) paste0("'", x, "'")

text_lst <- function(v) paste0("[", paste0(v, collapse = ","), "]")

escape <- function(x) gsub("'", "''", x)

# ---- not_op + some operator ----

text_not_op <- function(x) UseMethod("text_not_op", x$args[[1]])

#' @exportS3Method
text_not_op.cql2_isnull_op <- function(x)
    paste(to_text(x$args[[1]]$args[[1]]), "IS NOT NULL")

#' @exportS3Method
text_not_op.default <- function(x)
    paste("NOT", to_text(x$args[[1]]))

# ---- convert to text ----

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
to_text.cql2_spatial <- function(x) to_wkt(x)

#' @exportS3Method
to_text.list <- function(x) {
    if (is_lst(x))
        text_lst(lapply(x, to_text))
    else
        stop("cannot convert list object to cql2 text", call. = FALSE)
}

#' @exportS3Method
to_text.cql2_logic_op <- function(x)
    paste(to_text(x$args[[1]]), toupper(x$op), to_text(x$args[[2]]))

#' @exportS3Method
to_text.cql2_not_op <- function(x) {
    text_not_op(x)
}

#' @exportS3Method
to_text.cql2_spatial_op <- function(x) {
    paste0(
        toupper(x$op), "(",
        to_text(x$args[[1]]), ",",
        to_text(x$args[[2]]),
        ")"
    )
}

#' @exportS3Method
to_text.cql2_temporal_op <- function(x) {
    paste0(
        toupper(x$op), "(",
        to_text(x$args[[1]]), ",",
        to_text(x$args[[2]]),
        ")"
    )
}

#' @exportS3Method
to_text.cql2_func <- function(x) {
    args <- paste0(lapply(x$`function`$args, to_text), collapse = ",")
    paste0(x$`function`$name, "(", args, ")")
}

#' @exportS3Method
to_text.cql2_comp_op <- function(x)
    paste(to_text(x$args[[1]]), x$op, to_text(x$args[[2]]))

#' @exportS3Method
to_text.cql2_isnull_op <- function(x)
    paste(to_text(x$args[[1]]), "IS NULL")

#' @exportS3Method
to_text.cql2_math_op <- function(x)
    paste(to_text(x$args[[1]]), x$op, to_text(x$args[[2]]))

#' @exportS3Method
to_text.cql2_minus_op <- function(x) {
    if (length(x$args) == 1)
        paste0(x$op, to_text(x$args[[1]]))
    else
        paste(to_text(x$args[[1]]), x$op, to_text(x$args[[2]]))
}

#' @exportS3Method
to_text.cql2_prop_ref <- function(x)
    x$property[[1]]

#' @exportS3Method
to_text.cql2_timestamp <- function(x)
    paste0("TIMESTAMP(", to_text(x$timestamp), ")")

#' @exportS3Method
to_text.cql2_date <- function(x)
    paste0("DATE(", to_text(x$date), ")")

#' @exportS3Method
to_text.cql2_interval <- function(x)
    paste0("INTERVAL(", to_text(x$interval[[1]]), ",",
           to_text(x$interval[[2]]), ")")

#' @exportS3Method
to_text.default <- function(x)
    stop(paste("cannot handle value of class", class(x)), call. = FALSE)


spatial_type <- function(x) {
    stopifnot(!is.null(x[["type"]]))
    x[["type"]]
}

spatial_switch <- function(x, ...) {
    switch(spatial_type(x), ...)
}

to_wkt <- function(x) {
    spatial_switch(
        x,
        "Point" = paste0("POINT(", wkt_coord0(x), ")"),
        "MultiPoint" = paste0("MULTIPOINT(", wkt_coord1(x) , ")"),
        "LineString" = paste0("LINESTRING(", wkt_coord1(x) , ")"),
        "MultiLineString" = paste0("MULTILINESTRING(", wkt_coord2(x) , ")"),
        "Polygon" = paste0("POLYGON(", wkt_coord2(x) , ")"),
        "MultiPolygon" = paste0("MULTIPOLYGON(", wkt_coord3(x) , ")"),
        "GeometryCollection" = paste0("GEOMETRYCOLLECTION(", wkt_collection(x), ")")
    )
}

wkt_collection <- function(x) {
    paste0(vapply(x[["geometries"]], to_wkt, character(1)), collapse = ",")
}

wkt_coord0 <- function(x) {
    paste(x[["coordinates"]], collapse = " ")
}

wkt_coord1 <- function(x) {
    paste(apply(x[["coordinates"]], 1, paste, collapse = " ", simplify = TRUE),
          collapse = ",")
}

wkt_coord2 <- function(x) {
    paste0("(",
           vapply(x[["coordinates"]], function(y) {
               paste(apply(y, 1, paste, collapse = " ", simplify = TRUE),
                     collapse = ",")
           }, character(1)), ")", collapse = ","
    )
}

wkt_coord3 <- function(x) {
    paste0("(",
    vapply(x[["coordinates"]], function(p) {
        paste0("(",
               vapply(p, function(y) {
                   paste(apply(y, 1, paste, collapse = " ", simplify = TRUE),
                         collapse = ",")
               }, character(1)), ")", collapse = ","
        )
    }, character(1)), ")", collapse = ",")

}
