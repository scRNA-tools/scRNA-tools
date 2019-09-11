create_pkgs_cache <- function(dir) {

    `%>%` <- magrittr::`%>%`

    bioc_pkgs  <- get_bioc_pkgs()
    cran_pkgs  <- get_cran_pkgs()
    pypi_pkgs  <- get_pypi_pkgs()
    conda_pkgs <- get_conda_pkgs()

    pkgs_cache <- tibble::tibble(
        Name = c(bioc_pkgs, cran_pkgs, pypi_pkgs, conda_pkgs),
        Type = c(
            rep("Bioc",  length(bioc_pkgs)),
            rep("CRAN",  length(cran_pkgs)),
            rep("PyPI",  length(pypi_pkgs)),
            rep("Conda", length(conda_pkgs))
        )
    ) %>%
        dplyr::mutate(Repository = paste(Name, Type, sep = "@"))

    fs::dir_create(dir)
    readr::write_tsv(pkgs_cache, fs::path(dir, "packages-cache.tsv"))

    return(pkgs_cache)
}

get_bioc_pkgs <- function() {

    `%>%` <- magrittr::`%>%`

    bioc_url <- "https://bioconductor.org/packages/release/bioc/"

    xml2::read_html(bioc_url) %>%
        rvest::html_nodes("table") %>%
        rvest::html_nodes("a") %>%
        rvest::html_text()
}

get_cran_pkgs <- function() {

    `%>%` <- magrittr::`%>%`

    cran_url <- "https://cran.r-project.org/web/packages/available_packages_by_name.html"

    xml2::read_html(cran_url) %>%
        rvest::html_nodes("a") %>%
        rvest::html_text() %>%
        setdiff(LETTERS) # Remove letter links at top of page
}

get_pypi_pkgs <- function() {

    `%>%` <- magrittr::`%>%`

    xml2::read_html("https://pypi.python.org/simple/") %>%
        rvest::html_nodes("a") %>%
        rvest::html_text()
}

get_conda_pkgs <- function() {

    `%>%` <- magrittr::`%>%`

    conda_pages <- xml2::read_html("https://anaconda.org/anaconda/repo") %>%
        rvest::html_nodes(".unavailable:nth-child(2)") %>%
        rvest::html_text() %>%
        stringr::str_split(" ") %>%
        unlist()
    conda_pages <- as.numeric(conda_pages[4])

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

check_pkgs_cache <- function(name, pkgs_cache, current = c(), ignored = c()) {

    `%>%` <- dplyr::`%>%`

    matched_name <- tolower(pkgs_cache$Name) == tolower(name)
    matches <- pkgs_cache$Repository[matched_name]
    matches <- matches[!(matches %in% current)]
    matches <- matches[!(matches %in% ignored)]
    real <- rep(FALSE, length(matches))
    names(real) <- matches

    if (length(matches) > 0) {
        for (repo in matches) {
            is_real <- prompt_yn(glue::glue(
                "Is {usethis::ui_value(repo)} a matching package for ",
                "{usethis::ui_value(name)} (y/n)?:"
            ))

            if (is_real) {
                real[repo] <- TRUE
            }
        }
    }

    repositories <- tibble::tibble(
        Repository = c(current, ignored, matches)
    ) %>%
        dplyr::mutate(
            Type = stringr::str_remove(Repository, "^.*@"),
            Name = stringr::str_remove(Repository, "@.*$")
        )

    list(
        Real         = c(current, matches[real]),
        Ignored      = c(ignored, matches[!real]),
        Repositories = repositories
    )
}
