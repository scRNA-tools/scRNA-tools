#' Tidy software table
#'
#' Convert the software table to tidy format
#'
#' @param swsheet Tibble containing software table
#'
#' @return Tidy swsheet tibble
tidy_swsheet <- function(swsheet) {

    `%>%` <- magrittr::`%>%`

    futile.logger::flog.info("Tidying data...")

    tidyr::gather(swsheet, key = 'Category', value = 'Val',
                  -Description, -Name, -Platform, -DOIs, -PubDates, -Updated,
                  -Added, -Code, -Github, -License, -Refs, -BioC, -CRAN, -PyPI,
                  -Conda, -Citations, -Publications, -Preprints) %>%
        dplyr::filter(Val == TRUE) %>%
        dplyr::select(-Val) %>%
        dplyr::arrange(Name)
}


#' Add categories
#'
#' Add categories column to software table
#'
#' @param swsheet Tibble containing software table
#' @param tidysw Tibble containing tidy software table
#'
#' @return swsheet with additional categories column
add_cats <- function(swsheet, tidysw) {

    futile.logger::flog.info("Adding categories to table...")

    catlist <- split(tidysw$Category, f = tidysw$Name)

    catdf <- data.frame(Name = names(catlist), stringsAsFactors = FALSE)
    catdf[["Categories"]] <- catlist

    swsheet <- dplyr::left_join(swsheet, catdf, by = "Name")
}
