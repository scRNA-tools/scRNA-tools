get_references <- function(dois) {

    dummy <- tibble::tibble(
        DOI        = character(),
        arXiv      = character(),
        Preprint   = character(),
        Date       = character(),
        Title      = character(),
        Citations  = double(),
        Timestamp  = lubridate::ydm_hms()
    )

    if (length(dois) == 0) {
        return(dummy)
    }

    references <- purrr::map_dfr(dois, function(.doi) {

        biorxiv  <- stringr::str_detect(.doi, "^10.1101/")
        arxiv    <- stringr::str_detect(.doi, "arxiv")
        peerj    <- stringr::str_detect(.doi, "10.7287/")
        preprint <- biorxiv || arxiv || peerj

        if (!arxiv) {
            cr_data <- rcrossref::cr_works(.doi, .progress = "text")$data
            title <- cr_data$title
            date <- dplyr::if_else(preprint, NA_character_, cr_data$issued)
            citations <- rcrossref::cr_citation_count(.doi)$count
        } else {
            arxiv_id <- stringr::str_remove(.doi, "arxiv/")
            arxiv_data <- aRxiv::arxiv_search(id_list = arxiv_id)
            title <- arxiv_data$title
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
                timestamp <- lubridate::now()
            }

            ref <- tibble::tibble(
                DOI        = .doi,
                arXiv      = arxiv,
                Preprint   = preprint,
                Date       = date,
                Title      = title,
                Citations  = citations,
                Timestamp  = timestamp
            )
        } else {
            ref <- dummy
        }
    })

    usethis::ui_done("Checking references complete")

    return(references)
}
