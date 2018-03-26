#' Add references
#'
#' Covert references to list column and get citations
#'
#' @param swsheet Tibble containing software table
#' @param titles_cache Tibble containing titles cache
#'
#' @return swsheet with additional columns
add_refs <- function(swsheet, titles_cache) {

    `%>%` <- magrittr::`%>%`

    message("Adding references...")

    doi_list <- swsheet %>%
        dplyr::mutate(DOIs = stringr::str_split(DOIs, ";")) %>%
        dplyr::pull(DOIs) %>%
        setNames(swsheet$Name)

    date_list <- swsheet %>%
        dplyr::mutate(PubDates = stringr::str_split(PubDates, ";")) %>%
        dplyr::pull(PubDates) %>%
        setNames(swsheet$Name)

    ref_list <- pbapply::pbsapply(names(doi_list), function(x) {
        dois <- doi_list[[x]]
        dates <- date_list[[x]]
        if (!(length(dois) == length(dates))) {
            stop(x, " - length(dois) != length(dates)", call. = FALSE)
        }

        if (all(is.na(dois))) {
            return(NA)
        }

        cites <- sapply(dois, function(doi) {
            cite <- tryCatch({
                rcrossref::cr_citation_count(doi)
            }, error = function(e) {
                NA
            })

            Sys.sleep(sample(seq(0, 1, 0.1), 1))

            return(cite)
        })

        titles <- get_titles(dois, titles_cache)

        ref <- tibble::tibble(Title     = titles,
                              DOI       = dois,
                              PubDate   = ifelse(dates != "PREPRINT",
                                                 dates, NA),
                              Preprint  = dates == "PREPRINT",
                              Citations = cites)
    })

    pre_list <- purrr::map_if(ref_list, !is.na(ref_list),
                              dplyr::filter, Preprint == TRUE)
    pub_list <- purrr::map_if(ref_list, !is.na(ref_list),
                              dplyr::filter, Preprint == FALSE)

    swsheet <- swsheet %>%
        dplyr::mutate(Refs = purrr::map2(pub_list, pre_list,
                                         function(x, y) {
                                             list(Publications = x,
                                                  Preprints = y)
                                             })) %>%
        dplyr::mutate(Citations = purrr::map_if(ref_list, !is.na(ref_list),
                                                function(x) {sum(x$Citations)}),
                      Publications = purrr::map_if(pub_list, !is.na(pub_list),
                                                   nrow),
                      Preprints = purrr::map_if(pre_list, !is.na(pre_list),
                                                nrow)) %>%
        dplyr::mutate(Citations = purrr::flatten_dbl(Citations),
                      Publications = purrr::flatten_int(Publications),
                      Preprints = purrr::flatten_int(Preprints))

    return(swsheet)
}
