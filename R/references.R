#' Add references
#'
#' Covert references to list column and get citations
#'
#' @param swsheet Tibble containing software table
#' @param titles_cache Tibble containing titles cache
#'
#' @return swsheet with additional columns
add_refs <- function(swsheet, titles_cache) {

    message("Adding references...")

    doi_list <- swsheet %>%
        mutate(DOIs = str_split(DOIs, ";")) %>%
        pull(DOIs) %>%
        setNames(swsheet$Name)

    date_list <- swsheet %>%
        mutate(PubDates = str_split(PubDates, ";")) %>%
        pull(PubDates) %>%
        setNames(swsheet$Name)

    ref_list <- pbsapply(names(doi_list), function(x) {
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
                cr_citation_count(doi)
            }, error = function(e) {
                NA
            })

            Sys.sleep(sample(seq(0, 1, 0.1), 1))

            return(cite)
        })

        titles <- get_titles(dois, titles_cache)

        ref <- tibble(Title = titles,
                      DOI = dois,
                      PubDate = ifelse(dates != "PREPRINT", dates, NA),
                      Preprint = dates == "PREPRINT",
                      Citations = cites)
    })

    pre_list <- map_if(ref_list, !is.na(ref_list), filter, Preprint == TRUE)
    pub_list <- map_if(ref_list, !is.na(ref_list), filter, Preprint == FALSE)

    swsheet <- swsheet %>%
        mutate(Refs = map2(pub_list, pre_list,
                           function(x, y) {
                               list(Publications = x, Preprints = y)
                           })) %>%
        mutate(Citations = map_if(ref_list, !is.na(ref_list),
                                  function(x) {sum(x$Citations)}),
               Publications = map_if(pub_list, !is.na(pub_list), nrow),
               Preprints = map_if(pre_list, !is.na(pre_list), nrow)) %>%
        mutate(Citations = flatten_dbl(Citations),
               Publications = flatten_int(Publications),
               Preprints = flatten_int(Preprints))

    return(swsheet)
}
