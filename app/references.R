#' Get references
#'
#' Searches Crossref and arXiv for a list of DOIs and checks that the references
#' are correct. Details are returned for the references that were confirmed by
#' the user. Also checks for linked references
#'
#' @param dois Vector of DOIs
#' @param ref_links data.frame with known reference links
#' 
#' @details
#' Any reference links found are stored in a "Links" attribute of the returned
#' reference table.
#'
#' @return tibble with reference data
get_references <- function(dois, ref_links) {

    references <- get_dummy_reference()
    new_links <- get_dummy_link()
    
    for (.doi in dois) {

        arxiv <- stringr::str_detect(.doi, "arxiv")

        if (!arxiv) {
            cr_data <- rcrossref::cr_works(.doi, .progress = "text")$data
            title <- cr_data$title
            
            preprint <- is_doi_preprint(
                .doi,
                container_title = "container.title" %in% colnames(cr_data)
            )
            
            date <- dplyr::if_else(preprint, NA_character_, cr_data$issued)
            citations <- rcrossref::cr_citation_count(.doi)$count
        } else {
            arxiv_id   <- stringr::str_remove(.doi, "arxiv/")
            arxiv_data <- aRxiv::arxiv_search(id_list = arxiv_id)
            title      <- arxiv_data$title
            preprint   <- TRUE
            date       <- NA
            citations  <- NA
        }
        title <- stringr::str_squish(title)

        cat("\n")
        usethis::ui_todo("Found the following reference:")
        cat("\n")
        show_reference(.doi, arxiv, preprint, title, date, citations)
        
        correct <- prompt_yn("Is this correct (y/n)?:")

        if (correct) {
            ref <- tibble::tibble(
                DOI        = .doi,
                arXiv      = arxiv,
                Preprint   = preprint,
                Date       = date,
                Title      = title,
                Citations  = citations,
                Timestamp  = lubridate::now("UTC"),
                Delay      = 0
            )
            
            is_linked <- .doi %in% ref_links$Preprint ||
                .doi %in% ref_links$Publication
            if (!is_linked && !arxiv) {
                cat("\n")
                usethis::ui_todo("Checking for linked references...")
                linked_ref <- get_linked_ref(ref)
                
                if (nrow(linked_ref) > 0) {
                    ref <- dplyr::bind_rows(ref, linked_ref)
                    new_links <- dplyr::bind_rows(
                        new_links,
                        attr(linked_ref, "Links")
                    )
                }
                
            }
            
            references <- dplyr::bind_rows(references, ref)
        }
    }
    
    attr(references, "Links") <- new_links
    
    usethis::ui_done("Checking references complete")
    
    return(references)
}

#' Show reference
#'
#' Display a reference
#'
#' @param doi Reference DOI
#' @param arxiv Is reference from arXiv?
#' @param preprint Is reference a preprint?
#' @param title Reference title
#' @param date Reference data
#' @param citations Reference citations
#'
#' @return TRUE invisibly
show_reference <- function(doi, arxiv, preprint, title, date, citations) {
    usethis::ui_line(glue::glue(
        "{usethis::ui_field('DOI:')} {usethis::ui_value(doi)}")
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
    
    invisible(TRUE)
}

#' Get dummy reference
#'
#' Create a dummy reference tibble for when reference information is missing
#'
#' @return empty reference tibble
get_dummy_reference <- function() {
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
    
    attr(dummy, "Links") <- get_dummy_link()
    
    return(dummy)
}

#' Get dummy link
#'
#' Create a dummy link tibble for when link information is missing
#'
#' @return empty link tibble
get_dummy_link <- function() {
    tibble::tibble(
        Preprint    = character(),
        Publication = character(),
        Correct     = logical()
    )
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

#' Is DOI preprint
#' 
#' Check whether a DOI is from a preprint server.
#'
#' @param doi DOI to check
#' @param container_title Whether or not there is a container title (journal
#' name) associated with the DOI
#'
#' @return Logical whether the DOI is from a preprint
is_doi_preprint <- function(doi, container_title) {

    peerj    <- stringr::str_detect(doi, "^10.7287/")
    square   <- stringr::str_detect(doi, "^10.21203/")
    biorxiv  <- stringr::str_detect(doi, "^10.1101/") && !container_title
    preprint <- biorxiv || peerj || square
    
    return(preprint)
}

#' Get linked reference
#'
#' Search for references to an existing reference in the database
#'
#' @param reference data.frame containing query reference to search for
#' @param print_query Whether to print `reference` before printing any potential
#' linked references
#'
#' @return tibble with confirmed linked references
get_linked_ref <- function(reference, print_query = FALSE) {
    
    dummy <- get_dummy_reference()
    
    if (reference$arXiv) {
        return(dummy)
    }
    
    results <- doilinker::search_doi_links(
        reference$DOI,
        preprint       = reference$Preprint,
        filter_matches = TRUE,
        verbose        = FALSE
    )
    
    if (nrow(results) == 0) {
        return(dummy)
    }
    
    if (print_query) {
        cat("\n")
        usethis::ui_done("Query reference:")
        cat("\n")
        show_reference(
            reference$DOI, reference$arXiv, reference$Preprint, reference$Title,
            reference$Date, reference$Citations
        )
    }
    
    for (idx in seq_len(nrow(results))) {
        doi <- results$doi[idx]
        title <- results$title[idx]
        
        preprint <- is_doi_preprint(
            doi,
            container_title = "container.title" %in% colnames(results) &&
                !(is.na(results$container.title[idx]))
        )
        
        date <- dplyr::if_else(preprint, NA_character_, results$issued[idx])
        citations <- rcrossref::cr_citation_count(doi)$count
        
        cat("\n")
        usethis::ui_todo("Found the following potential linked reference:")
        cat("\n")
        show_reference(doi, FALSE, preprint, title, date, citations)
        
        correct <- prompt_yn("Is this correct (y/n)?:")
        
        if (correct) {
            linked_ref <- tibble::tibble(
                DOI        = doi,
                arXiv      = FALSE,
                Preprint   = preprint,
                Date       = date,
                Title      = title,
                Citations  = citations,
                Timestamp  = lubridate::now("UTC"),
                Delay      = 0
            )
            
            attr(linked_ref, "Links") <- new_ref_link(
                doi        = reference$DOI,
                linked_doi = doi,
                preprint   = reference$Preprint
            )
            
            return(linked_ref)
        }
    }
    
    return(dummy)
}

#' New reference link
#'
#' Create a new tibble describing a link between two references
#'
#' @param doi DOI of the original reference
#' @param linked_doi DOI of the linked reference
#' @param preprint Whether `doi` is a preprint
#' @param correct Whether this link is real or not
#'
#' @return tibble with reference link
new_ref_link <- function(doi, linked_doi, preprint, correct = TRUE) {
    
    if (preprint) {
        link <- tibble::tibble(
            Preprint    = doi,
            Publication = linked_doi,
            Correct     = correct
        )
    } else {
        link <- tibble::tibble(
            Preprint    = linked_doi,
            Publication = doi,
            Correct     = correct
        )
    }
    
    return(link)
}
