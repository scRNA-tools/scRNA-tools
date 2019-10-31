#' Get packages cache
#'
#' Get a list of the packages that are available in various software
#' repositories
#'
#' @return tibble
get_pkgs_cache <- function() {

    `%>%` <- magrittr::`%>%`

    bioc_pkgs  <- get_bioc_pkgs()
    usethis::ui_done("Got Bioconductor packages")
    cran_pkgs  <- get_cran_pkgs()
    usethis::ui_done("Got CRAN packages")
    pypi_pkgs  <- get_pypi_pkgs()
    usethis::ui_done("Got PyPI packages")
    usethis::ui_todo("Getting Conda packages...")
    conda_pkgs <- get_conda_pkgs()
    usethis::ui_done("Got Conda packages")

    pkgs_cache <- tibble::tibble(
        Name = c(bioc_pkgs, cran_pkgs, pypi_pkgs, conda_pkgs),
        Type = c(
            rep("Bioc",  length(bioc_pkgs)),
            rep("CRAN",  length(cran_pkgs)),
            rep("PyPI",  length(pypi_pkgs)),
            rep("Conda", length(conda_pkgs))
        ),
        Added = lubridate::now("UTC")
    ) %>%
        dplyr::mutate(Repository = paste(Name, Type, sep = "@")) %>%
        dplyr::select(Repository, Name, Type, Added)

    return(pkgs_cache)
}

#' Get Bioconductor packages
#'
#' Get a list of the packages that are available in Bioconductor
#'
#' @return character vector
get_bioc_pkgs <- function() {

    `%>%` <- magrittr::`%>%`

    bioc_url <- "https://bioconductor.org/packages/release/bioc/"

    xml2::read_html(bioc_url) %>%
        rvest::html_nodes("table") %>%
        rvest::html_nodes("a") %>%
        rvest::html_text()
}

#' Get CRAN packages
#'
#' Get a list of the packages that are available in CRAN
#'
#' @return character vector
get_cran_pkgs <- function() {

    `%>%` <- magrittr::`%>%`

    cran_url <- "https://cran.r-project.org/web/packages/available_packages_by_name.html"

    xml2::read_html(cran_url) %>%
        rvest::html_nodes("a") %>%
        rvest::html_text() %>%
        setdiff(LETTERS) # Remove letter links at top of page
}

#' Get PyPI packages
#'
#' Get a list of the packages that are available in PyPI
#'
#' @return character vector
get_pypi_pkgs <- function() {

    `%>%` <- magrittr::`%>%`

    xml2::read_html("https://pypi.python.org/simple/") %>%
        rvest::html_nodes("a") %>%
        rvest::html_text()
}

#' Get Conda packages
#'
#' Get a list of the packages that are available in Conda
#'
#' @return character vector
get_conda_pkgs <- function() {

    `%>%` <- magrittr::`%>%`

    conda_pages <- xml2::read_html("https://anaconda.org/anaconda/repo") %>%
        rvest::html_nodes(".unavailable:nth-child(2)") %>%
        rvest::html_text() %>%
        stringr::str_split(" ") %>%
        unlist()
    conda_pages <- as.numeric(conda_pages[4])

    pbapply::pboptions(type = "timer", char = "=", style = 3)
    conda_pkgs <- pbapply::pbsapply(seq_len(conda_pages), function(page) {
        url <- paste0(
            "https://anaconda.org/anaconda/repo?sort=_name&sort_order=asc&page=",
            page
        )

        xml2::read_html(url) %>%
            rvest::html_nodes(".packageName") %>%
            rvest::html_text()
    })

    conda_pkgs <- unlist(conda_pkgs)

    return(conda_pkgs)
}

#' Check repositories
#'
#' Check for package repositories that match tools in the database.
#'
#' @param database Database object
#' @param pkgs_cache Packages cache table
#' @param new Whether to only check new packages
#'
#' @return character vector
check_pkgs <- function(database, pkgs_cache, new = TRUE) {

    if (new) {
        last_week <- lubridate::today("UTC") - 7

        pkgs_cache <- dplyr::filter(pkgs_cache, Added > last_week)

        if (nrow(pkgs_cache) == 0) {
            usethis::ui_done("Repositories up to date")
            return(database)
        }
    }

    usethis::ui_todo(glue::glue(
        "Checking {usethis::ui_value(nrow(pkgs_cache))} repositories..."
    ))

    pb <- progress::progress_bar$new(
        format = paste(
            "[:bar] :current/:total :percent",
            "Elapsed: :elapsedfull ETA: :eta"
        ),
        total = length(database$Tools),
        clear = FALSE
    )
    pb$tick(0)
    for (name in names(database$Tools)) {
        pb$tick()
        database <- update_repositories(name, database, pkgs_cache,
                                        prompt = FALSE)
    }
    usethis::ui_done("Repositories updated")

    return(database)
}
