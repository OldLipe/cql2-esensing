#' Convert R expressions to CQL2
#'
#' @description
#' These functions convert R expressions to CQL2 standard (text or JSON).
#'
#' @name cql2
#'
#' @param expr  An R expression to be represented in CQL2
#'
#' @examples
#' # basic cql2 examples
#' cql2_text("This is a literal string.")
#' cql2_text("Via dell'Avvento")
#' cql2_json("Via dell'Avvento")
#' cql2_text(-100)
#' cql2_json(-100)
#' cql2_text(3.14159)
#' cql2_text(TRUE)
#' cql2_text(FALSE)
#' cql2_text(timestamp("1969-07-20T20:17:40Z")) # it also works with timestamp()
#' cql2_json(timestamp("1969-07-20T20:17:40Z"))
#' cql2_text(date("1969-07-20"))
#' cql2_json(date("1969-07-20"))
#' cql2_text(interval("1969-07-16", "1969-07-24"))
#' cql2_text(interval("1969-07-16T05:32:00Z", "1969-07-24T16:50:35Z"))
#' cql2_text(interval("2019-09-09", ".."))
#' cql2_json(interval("1969-07-16", "1969-07-24"))
#' cql2_json(interval("1969-07-16T05:32:00Z", "1969-07-24T16:50:35Z"))
#' cql2_json(interval("2019-09-09", ".."))
#' cql2_text(city == "Toronto")
#' cql2_json(city == "Toronto")
#' cql2_text(balance - 150.0 > 0)
#' cql2_json(balance - 150.0 > 0)
#' cql2_text(updated >= date("1970-01-01"))
#' cql2_json(updated >= date("1970-01-01"))
#' cql2_text(!is_null(geometry))
#' cql2_json(!is_null(geometry))
#' poly_sf <- sfheaders::sf_polygon(matrix(c(0,0,0,0,1,1), ncol = 2))
#' cql2_text(s_intersects({{poly_sf}}, geometry))
#' cql2_json(s_intersects({{poly_sf}}, geometry))
#' cql2_text(s_crosses(geometry, {{poly_sf}}))
#' cql2_json(s_crosses(geometry, {{poly_sf}}))
#' cql2_text(t_intersects(event_date,
#'           interval("1969-07-16T05:32:00Z", "1969-07-24T16:50:35Z")))
#' cql2_json(t_intersects(event_date,
#'           interval("1969-07-16T05:32:00Z", "1969-07-24T16:50:35Z")))
#' cql2_text(t_during(touchdown,
#'           interval("1969-07-16T13:32:00Z", "1969-07-24T16:50:35Z")))
#' cql2_json(t_during(touchdown,
#'           interval("1969-07-16T13:32:00Z", "1969-07-24T16:50:35Z")))
#' cql2_text(s_within(road,Buffer(geometry,10,"m")))
#' cql2_json(s_within(road,Buffer(geometry,10,"m")))
#' cql2_text(t_during(timestamp("1969-07-20T20:17:40Z"),
#'           interval("1969-07-16T13:32:00Z", "1969-07-24T16:50:35Z")))
#' cql2_json(t_during(timestamp("1969-07-20T20:17:40Z"),
#'           interval("1969-07-16T13:32:00Z", "1969-07-24T16:50:35Z")))
NULL

#' @rdname cql2
#' @export
cql2_text <- function(expr) {
    expr <- substitute(expr, environment())
    # TODO: implement support for field filter-crs and detect crs inside expr
    structure(
        list(`filter-lang` = "cql2-text",
             filter = to_text(to_cql2(expr))),
        class = "cql2_query"
    )
}

#' @rdname cql2
#' @export
cql2_json <- function(expr) {
    expr <- substitute(expr, environment())
    # TODO: implement support for field filter-crs and detect crs inside expr
    structure(
        list(`filter-lang` = "cql2-json",
             filter = to_json(to_cql2(expr))),
        class = "cql2_query"
    )
}
