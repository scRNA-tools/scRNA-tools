#' URL regular expression
#'
#' Regular expression for matching a website URL. Must include a protocol
#' (http/https).
#'
#' @return regex object
url_re <- function() {

    valid_chars <- rex::rex(except_some_of(".", "/", " ", "-"))

    rex::rex(
        start,

        # protocol identifier + //
        rex:::group(list("http", rex:::maybe("s")), "://"),

        # host name
        rex:::group(
            rex:::zero_or_more(valid_chars, rex:::zero_or_more("-")),
            rex:::one_or_more(valid_chars)
        ),

        # domain name
        rex:::zero_or_more(
            ".",
            rex:::zero_or_more(valid_chars, rex:::zero_or_more("-")),
            rex:::one_or_more(valid_chars)
        ),

        # TLD identifier
        rex:::group(".", valid_chars %>% rex:::at_least(2)),

        # resource path (optional)
        rex:::maybe("/", non_space %>% rex:::zero_or_more()),

        end
    )
}

#' DOI regular expression
#'
#' Regular expression for matching a DOI. Also matches arXiv identifiers
#' (arxiv/ID_STRING).
#'
#' @return regex object
doi_re <- function() {
    rex::rex(
        start,

        rex:::or(
            # DOI
            rex::rex(
                rex:::group("10."),
                rex:::between(digit, 4, 9),
                rex:::group("/"),
                rex:::some_of(alnum, "-", "_", ".", ":", ";", "(", ")", "/")
            ),

            # arXiv
            rex::rex(
                rex:::group("arxiv/"),
                rex:::n_times(digit, 4),
                rex:::group("."),
                rex:::between(digit, 4, 5)
            )
        ),

        end
    )
}
