# TODO: implementar todos os properties para que não precisa fornecer como string
# contrutor de operadores espaciais
spatial_op <- function(op) {

    rlang::new_function(
        args = rlang::exprs(geom1 = , geom2 = ),
        body = rlang::expr(
            structure(
                list(op = !!op, args = list(geom1, geom2)),
                class = c("cql2_expr", "list")
            )
        ),
        env = rlang::caller_env()
    )
}

#' @exportS3Method
print.cql2_expr <- function(x) {
    cat(
        jsonlite::toJSON(
            x = x,
            pretty = TRUE,
            auto_unbox = TRUE
        )
    )
}

# A geometric expression is a property name of a geometry-valued property,
# a geometric literal (expressed as WKT) or a function that returns a
# geometric value.

#' Spatial operators
#'
#' @description A set of functions to deal with spatial data.
#'
#' @param geom1,geom2 a geometry literal in WKT, a property name listed in
#'                    queryables, or a function that returns a geometric value.
#'
#' @note add note about parameter side of literal geometry and property name.
#'
#' @examples
#' s_intersects("1", list(2))
#'
#' @export
s_intersects <- spatial_op("s_intersects")


# TODO: forma de uso
#... %>% cql_filter(s_intersects(geometry, geometry2))

#S_INTERSECTS(geometry,POLYGON\((36.319836 32.288087,36.320041 32.288032,36.320210 32.288402,36.320008 32.288458,36.319836 32.288087)))

# {
#     "op": "s_intersects",
#     "args": [
#         { "property": "geometry" },
#         {
#             "type": "Polygon",
#             "coordinates": [
#                 [
#                     [36.319836, 32.288087],
#                     [36.320041, 32.288032],
#                     [36.320210, 32.288402],
#                     [36.320008, 32.288458],
#                     [36.319836, 32.288087]
#                 ]
#             ]
#         }
#     ]
# }
