#' Get tools JSON
#'
#' Create tools JSON
#'
#' @param tidysw Tibble containing tidy software table
#'
#' @return tools JSON
make_tools_json <- function(tidysw) {

    message("Converting tools...")

    catlist <- split(tidysw$Category, f = tidysw$Name)

    tools <- tidysw %>%
        select(-Category) %>%
        unique() %>%
        mutate(Categories = catlist[Name]) %>%
        toJSON(pretty = TRUE)
}


#' Get categories JSON
#'
#' Create categories JSON
#'
#' @param tidysw Tibble containing tidy software table
#' @param swsheet Tibble containing software table
#' @param descs data.frame containing category descriptions
#'
#' @return categories JSON
make_cats_json <- function(tidysw, swsheet, descs) {

    message("Converting categories...")

    namelist <- split(tidysw$Name, f = tidysw$Category)
    namelist <- lapply(namelist, function(x) {
        swsheet %>%
            filter(Name %in% x) %>%
            select(Name, Citations, Publications, Preprints, BioC, CRAN, PyPI,
                   Conda, Added, Updated)
    })

    cats <- tidysw %>%
        select(Category) %>%
        arrange(Category) %>%
        unique() %>%
        mutate(Tools = namelist[Category]) %>%
        left_join(descs, by = "Category") %>%
        toJSON(pretty = TRUE)
}


#' Write footer
#'
#' Write a HTML footer to use on website pages
write_footer <- function() {
    datetime <- Sys.time()
    attr(datetime, "tzone") <- "UTC"

    writeLines(paste0('<p class="text-muted">Last updated: ', datetime,
                      ' UTC</p>'),
               "docs/footer_content.html")
}
