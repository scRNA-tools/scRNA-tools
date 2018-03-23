#' Get software sheet
#'
#' Read `single_cell_software.csv`
#'
#' @return Tibble containing table
get_swsheet <- function() {

    message("Loading 'single_cell_software.csv'...")
    swsheet <- read_csv("single_cell_software.csv",
                        col_types = cols(
                            .default = col_logical(),
                            Name = col_character(),
                            Platform = col_character(),
                            DOIs = col_character(),
                            PubDates = col_character(),
                            Code = col_character(),
                            Description = col_character(),
                            License = col_character(),
                            Added = col_date(format = ""),
                            Updated = col_date(format = "")
                        ))
}


#' Get packages
#'
#' Get lists of the packages available in Bioconductor, CRAN and PyPI
#'
#' @return List of named vectors containg available packages
get_pkgs <- function() {

    message("Getting package repositories...")

    message("Getting Bioconductor package list...")
    bioc.pkgs <- all_group()
    names(bioc.pkgs) <- str_to_lower(bioc.pkgs)

    message("Getting CRAN package list...")
    cran.url <- "https://cran.r-project.org/web/packages/available_packages_by_name.html"
    #cran.url <- "cran_packages.html"
    cran.pkgs <- read_html(cran.url) %>%
        html_nodes("a") %>%
        html_text() %>%
        setdiff(LETTERS) # Remove letter links at top of page
    names(cran.pkgs) <- str_to_lower(cran.pkgs)

    message("Getting PyPI package list...")
    pypi.pkgs <- read_html("https://pypi.python.org/simple/") %>%
        html_nodes("a") %>%
        html_text()
    names(pypi.pkgs) <- str_to_lower(pypi.pkgs)

    message("Getting Anaconda package list...")
    pages <- read_html("https://anaconda.org/anaconda/repo") %>%
        html_nodes(".unavailable:nth-child(2)") %>%
        html_text() %>% str_split(" ") %>%
        unlist()
    pages <- as.numeric(pages[4])

    conda.pkgs <- pbsapply(seq_len(pages), function(p) {
        url <- paste0("https://anaconda.org/anaconda/repo?sort=_name&sort_order=asc&page=",
                      p)

        read_html(url) %>%
            html_nodes(".packageName") %>%
            html_text()
    })
    conda.pkgs <- unlist(conda.pkgs)
    names(conda.pkgs) <- str_to_lower(conda.pkgs)

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
    message("Getting category descriptions...")
    descs <- read_json("docs/data/descriptions.json", simplifyVector = TRUE)
}


