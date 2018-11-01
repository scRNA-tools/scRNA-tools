#' Check repositories
#'
#' Check if tools are in pkg repositories
#'
#' @param names Vector of tool names
#' @param pkgs List of available packages
#'
#' @return list with repo information
check_repos <- function(names, pkgs) {

    futile.logger::flog.info("Checking repositories...")
    repos <- jsonlite::fromJSON("docs/data/repositories.json")

    added <- 0
    ignored <- 0

    # Iterate over tool names
    for (name in names) {
        # Iterate over package lists
        for(repo in names(pkgs)) {
            # Check this repo isn't already set
            if (is.null(repos[[name]][[repo]])) {

                lower <- stringr::str_to_lower(name)
                # Check if tool in repository
                if (lower %in% names(pkgs[[repo]])) {

                    repo_name <- pkgs[[repo]][[lower]]
                    repo_path <- paste(repo, repo_name, sep = "/")

                    # Skip if ignored
                    if (repo_path %in% repos[[name]]$Ignored) {
                        next
                    }

                    # Create tool entry if missing
                    if (!(name %in% names(repos))) {
                        repos[[name]] <- list()
                    }

                    # Ask for confirmation repository is correct
                    message("Suggested repository for ", name, ": ", repo_path)
                    confirmed <- prompt_yn("Confirm")
                    if (confirmed) {
                        # Set repository
                        repos[[name]][[repo]] <- repo_name
                        message("Confirmed")
                        added <- added + 1
                    } else {
                        # Ignore repository
                        repos[[name]]$Ignored <- c(repos[[name]]$Ignored,
                                                   repo_path)
                        message("Added to ignore list")
                        ignored <- ignored + 1
                    }
                }
            }
        }
    }

    msg <- paste("Added", added, "new repositories")
    futile.logger::flog.info(msg)
    msg <- paste("Ignored", ignored, "repostories")
    futile.logger::flog.info(msg)
    readr::write_lines(jsonlite::toJSON(repos, pretty = TRUE),
                       "docs/data/repositories.json")

    return(repos)
}


#' Add Github
#'
#' Add field indicating Github repositories
#'
#' @param swsheet Tibble containing software table
#'
#' @return swsheet with Github column
add_github <- function(swsheet) {

    `%>%` <- magrittr::`%>%`

    futile.logger::flog.info("Adding Github...")

    swsheet %>%
        dplyr::mutate(Github = ifelse(stringr::str_detect(Code, "github"),
                                      stringr::str_replace(Code,
                                                           "https://github.com/",
                                                           ""),
                                      NA))
}


#' Add repositories
#'
#' Add fields indicating if tools are available from various software
#' repositories
#'
#' @param swsheet Tibble containing software table
#' @param repos List of repo information
#'
#' @return swsheet with additional repository columns
add_repos <- function(swsheet, repos) {

    `%>%` <- magrittr::`%>%`

    futile.logger::flog.info("Adding package repositories...")

    swsheet %>%
        dplyr::rowwise() %>%
        dplyr::mutate(BioC = ifelse(!is.null(repos[[Name]]$BioC),
                                    repos[[Name]]$BioC, NA)) %>%
        dplyr::mutate(CRAN = ifelse(!is.null(repos[[Name]]$CRAN),
                                    repos[[Name]]$CRAN, NA)) %>%
        dplyr::mutate(PyPI = ifelse(!is.null(repos[[Name]]$PyPI),
                                    repos[[Name]]$PyPI, NA)) %>%
        dplyr::mutate(Conda = ifelse(!is.null(repos[[Name]]$Conda),
                                     repos[[Name]]$Conda, NA)) %>%
        dplyr::ungroup()
}


#' Get shields
#'
#' Download shields describing various repositories
#'
#' @param swsheet Tibble containing software table
get_shields <- function(swsheet) {

    futile.logger::flog.info("Getting shields...")
    pb_format <- "   |:bar| :percent Elapsed: :elapsed Remaining: :eta"

    pb <- progress::progress_bar$new(total = nrow(swsheet), format = pb_format,
                                     clear = FALSE)
    futile.logger::flog.info("Downloading Bioconductor shields...")
    for (repo in swsheet$BioC) {
        pb$tick()
        if (!is.na(repo)) {
            years_url <- paste0("http://bioconductor.org/shields/years-in-bioc/",
                                repo, ".svg")
            down_url <- paste0("http://bioconductor.org/shields/downloads/",
                               "release/", repo, ".svg")

            download.file(years_url,
                          paste0("docs/img/shields/BioC/", repo, "_years.svg"),
                          quiet = TRUE)
            download.file(down_url,
                          paste0("docs/img/shields/BioC/", repo,
                                 "_downloads.svg"),
                          quiet = TRUE)
        }
    }

    pb <- progress::progress_bar$new(total = nrow(swsheet),format = pb_format,
                                     clear = FALSE)
    futile.logger::flog.info("Downloading CRAN shields...")
    for (repo in swsheet$CRAN) {
        pb$tick()
        if (!is.na(repo)) {
            version_url <- paste0("http://www.r-pkg.org/badges/version/",
                                  repo)
            down_url <- paste0("http://cranlogs.r-pkg.org/badges/grand-total/",
                               repo)

            download.file(version_url,
                          paste0("docs/img/shields/CRAN/", repo, "_version.svg"),
                          quiet = TRUE)
            download.file(down_url,
                          paste0("docs/img/shields/CRAN/", repo,
                                 "_downloads.svg"),
                          quiet = TRUE)
        }
    }

    pb <- progress::progress_bar$new(total = nrow(swsheet), format = pb_format,
                                     clear = FALSE)
    futile.logger::flog.info("Downloading PyPI shields...")
    for (repo in swsheet$PyPI) {
        pb$tick()
        if (!is.na(repo)) {
            version_url <- paste0("https://img.shields.io/pypi/v/",
                                  repo, ".svg")
            python_url <- paste0("https://img.shields.io/pypi/pyversions/",
                                 repo, ".svg")
            status_url <- paste0("https://img.shields.io/pypi/status/",
                                 repo, ".svg")

            download.file(version_url,
                          paste0("docs/img/shields/PyPI/", repo,
                                 "_version.svg"),
                          quiet = TRUE)
            download.file(python_url,
                          paste0("docs/img/shields/PyPI/", repo,
                                 "_python.svg"),
                          quiet = TRUE)
            download.file(status_url,
                          paste0("docs/img/shields/PyPI/", repo,
                                 "_status.svg"),
                          quiet = TRUE)
        }
    }

    pb <- progress::progress_bar$new(total = nrow(swsheet), format = pb_format,
                                     clear = FALSE)
    futile.logger::flog.info("Downloading GitHub shields...")
    for (repo in swsheet$Github) {
        pb$tick()
        if (!is.na(repo)) {
            stars_url <- paste0("https://img.shields.io/github/stars/",
                                repo, ".svg")
            forks_url <- paste0("https://img.shields.io/github/forks/",
                                repo, ".svg")
            commit_url <- paste0("https://img.shields.io/github/last-commit/",
                                 repo, ".svg")

            repo_clean <- stringr::str_replace(repo, "/", "_")
            download.file(stars_url,
                          paste0("docs/img/shields/GitHub/", repo_clean,
                                 "_stars.svg"),
                          quiet = TRUE)
            download.file(forks_url,
                          paste0("docs/img/shields/GitHub/", repo_clean,
                                 "_forks.svg"),
                          quiet = TRUE)
            download.file(commit_url,
                          paste0("docs/img/shields/GitHub/", repo_clean,
                                 "_commit.svg"),
                          quiet = TRUE)
        }
    }
}
