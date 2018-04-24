#' Get software sheet
#'
#' Read `single_cell_software.csv`
#'
#' @return Tibble containing table
get_swsheet <- function() {

    futile.logger::flog.info("Loading 'single_cell_software.csv'...")
    swsheet <- readr::read_csv("single_cell_software.csv",
                               col_types       = readr::cols(
                                   .default    = readr::col_logical(),
                                   Name        = readr::col_character(),
                                   Platform    = readr::col_character(),
                                   DOIs        = readr::col_character(),
                                   PubDates    = readr::col_character(),
                                   Code        = readr::col_character(),
                                   Description = readr::col_character(),
                                   License     = readr::col_character(),
                                   Added       = readr::col_date(format = ""),
                                   Updated     = readr::col_date(format = "")
                              ))
}


#' Get packages
#'
#' Get lists of the packages available in Bioconductor, CRAN and PyPI
#'
#' @return List of named vectors containg available packages
get_pkgs <- function() {

    `%>%` <- magrittr::`%>%`

    futile.logger::flog.info("Getting package repositories...")

    futile.logger::flog.info("Getting Bioconductor package list...")
    bioc.pkgs <- BiocInstaller::all_group()
    names(bioc.pkgs) <- stringr::str_to_lower(bioc.pkgs)

    futile.logger::flog.info("Getting CRAN package list...")
    cran.url <- "https://cran.r-project.org/web/packages/available_packages_by_name.html"
    #cran.url <- "cran_packages.html"
    cran.pkgs <- xml2::read_html(cran.url) %>%
        rvest::html_nodes("a") %>%
        rvest::html_text() %>%
        setdiff(LETTERS) # Remove letter links at top of page
    names(cran.pkgs) <- stringr::str_to_lower(cran.pkgs)

    futile.logger::flog.info("Getting PyPI package list...")
    pypi.pkgs <- xml2::read_html("https://pypi.python.org/simple/") %>%
        rvest::html_nodes("a") %>%
        rvest::html_text()
    names(pypi.pkgs) <- stringr::str_to_lower(pypi.pkgs)

    futile.logger::flog.info("Getting Anaconda package list...")
    pages <- xml2::read_html("https://anaconda.org/anaconda/repo") %>%
        rvest::html_nodes(".unavailable:nth-child(2)") %>%
        rvest::html_text() %>%
        stringr::str_split(" ") %>%
        unlist()
    pages <- as.numeric(pages[4])

    conda.pkgs <- pbapply::pbsapply(seq_len(pages), function(p) {
        url <- paste0("https://anaconda.org/anaconda/repo?sort=_name&sort_order=asc&page=",
                      p)

        xml2::read_html(url) %>%
            rvest::html_nodes(".packageName") %>%
            rvest::html_text()
    })
    conda.pkgs <- unlist(conda.pkgs)
    names(conda.pkgs) <- stringr::str_to_lower(conda.pkgs)

    pkgs <- list(BioC = bioc.pkgs,
                 CRAN = cran.pkgs,
                 PyPI = pypi.pkgs,
                 Conda = conda.pkgs)
}


#' Get category descriptions
#'
#' Read `docs/data/descriptions.json`
#'
#' @return data.frame containing categories and descriptions
get_descriptions <- function() {
    futile.logger::flog.info("Getting category descriptions...")
    descs <- jsonlite::read_json("docs/data/descriptions.json",
                                 simplifyVector = TRUE)
}
