#' Get references
#'
#' Searches Crossref and arXiv for a list of DOIs and checks that the references
#' are correct. Details are returned for the references that were confirmed by
#' the user.
#'
#' @param dois Vector of DOIs
#'
#' @return tibble
get_references <- function(dois) {

    dummy <- tibble::tibble(
        DOI        = character(),
        arXiv      = logical(),
        Preprint   = logical(),
        Date       = character(),
        Title      = character(),
        Citations  = double(),
        Timestamp  = lubridate::ydm_hms(),
        Delay      = double()
    )

    if (length(dois) == 0) {
        return(dummy)
    }

    references <- purrr::map_dfr(dois, function(.doi) {

        arxiv    <- stringr::str_detect(.doi, "arxiv")

        if (!arxiv) {
            cr_data <- rcrossref::cr_works(.doi, .progress = "text")$data
            title <- cr_data$title
            
            peerj    <- stringr::str_detect(.doi, "^10.7287/")
            square   <- stringr::str_detect(.doi, "^10.21203/")
            biorxiv  <- stringr::str_detect(.doi, "^10.1101/")
            biorxiv  <- biorxiv && !("container.title" %in% colnames(cr_data))
            preprint <- biorxiv || arxiv || peerj || square
            
            date <- dplyr::if_else(preprint, NA_character_, cr_data$issued)
            citations <- rcrossref::cr_citation_count(.doi)$count
        } else {
            arxiv_id <- stringr::str_remove(.doi, "arxiv/")
            arxiv_data <- aRxiv::arxiv_search(id_list = arxiv_id)
            title <- arxiv_data$title
            preprint <- TRUE
            date <- NA
            citations <- NA
        }
        title <- stringr::str_squish(title)

        cat("\n")
        usethis::ui_todo("Found the following reference:")
        cat("\n")
        usethis::ui_line(glue::glue(
            "{usethis::ui_field('DOI:')} {usethis::ui_value(.doi)}")
        )
        usethis::ui_line(glue::glue(
            "{usethis::ui_field('arXiv:')} {usethis::ui_value(arxiv)}")
        )
        usethis::ui_line(glue::glue(
            "{usethis::ui_field('Preprint:')} {usethis::ui_value(preprint)}")
        )
        usethis::ui_line(glue::glue(
            "{usethis::ui_field('Title:')} {usethis::ui_value(title)}")
        )
        usethis::ui_line(glue::glue(
            "{usethis::ui_field('Date:')} {usethis::ui_value(date)}")
        )
        usethis::ui_line(glue::glue(
            "{usethis::ui_field('Citations:')} {usethis::ui_value(citations)}")
        )

        correct <- prompt_yn("Is this correct (y/n)?:")

        if (correct) {
            if (arxiv) {
                timestamp <- NA
            } else {
                timestamp <- lubridate::now("UTC")
            }

            ref <- tibble::tibble(
                DOI        = .doi,
                arXiv      = arxiv,
                Preprint   = preprint,
                Date       = date,
                Title      = title,
                Citations  = citations,
                Timestamp  = timestamp,
                Delay      = 0
            )
        } else {
            ref <- dummy
        }
    })

    usethis::ui_done("Checking references complete")

    return(references)
}

#' Get publications list
#'
#' Get a list of publications associated with each tool in the database
#'
#' @param database Database object
#'
#' @return list of tibbles
get_pubs_list <- function(database) {

    pubs <- dplyr::filter(database$References, !Preprint)

    purrr::map(database$Tools, function(.tool) {
        dplyr::select(
            dplyr::filter(pubs, DOI %in% .tool$DOIs),
            Title, DOI, Date, Citations
        )
    })
}

#' Get preprints list
#'
#' Get a list of preprints associated with each tool in the database
#'
#' @param database Database object
#'
#' @return list of tibbles
get_preprints_list <- function(database) {

    preprints <- dplyr::filter(database$References, Preprint)

    purrr::map(database$Tools, function(.tool) {
        dplyr::select(
            dplyr::filter(preprints, DOI %in% .tool$DOIs),
            Title, DOI, Citations
        )
    })
}
