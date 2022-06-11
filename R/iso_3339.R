
iso_3339_date_fullyear <- "[0-9]{4}"
iso_3339_date_month <- "(1[0-2]|0[1-9])"
iso_3339_date_mday <- "(3[01]|[12][0-9]|0[1-9])"
iso_3339_time_hour <- "(2[0-3]|[01][0-9])"
iso_3339_time_minute <- "([0-5][0-9])"
iso_3339_time_second <- "(60|[0-5][0-9])"
iso_3339_time_secfrac <- "(\\.[0-9]+)?"
iso_3339_time_numoffset <- paste0(
    "[+-]",
    paste(
        iso_3339_time_hour,
        iso_3339_time_minute,
        sep = ":"
    )
)
iso_3339_time_offset <- paste0(
    "(Z|", iso_3339_time_numoffset, ")"
)
iso_3339_partial_time <- paste0(
    paste(
        iso_3339_time_hour,
        iso_3339_time_minute,
        iso_3339_time_second,
        sep = ":"
    ),
    iso_3339_time_secfrac
)
iso_3339_full_date <- paste(
    iso_3339_date_fullyear,
    iso_3339_date_month,
    iso_3339_date_mday,
    sep = "-"
)
iso_3339_full_time <- paste0(
    iso_3339_partial_time,
    iso_3339_time_offset
)
iso_3339_date_time <- paste0(
    iso_3339_full_date,
    "T",
    iso_3339_full_time
)
grep_iso_3339_date <- function(x) {
    grepl(paste0("^", iso_3339_full_date, "$"), x)
}
grep_iso_3339_date_time <- function(x) {
    grepl(paste0("^", iso_3339_date_time, "$"), x)
}
