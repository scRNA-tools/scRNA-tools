#' Search
#'
#' Search for a tool in the database
#'
#' @param database Database object
#' @param name Name of the tool to update
search <- function(database, name = NULL) {

    if (is.null(name)) {
        name <- prompt_string(
            "Which do you want to search for?:",
            allowed = "A-Za-z0-9-_"
        )
    }

    matches <- search_name(database, name)

    if (length(matches) == 1) {
        usethis::ui_done("Found one matching tool")
        cat("\n")
        database$Tools[[matches]]
    } else if (length(matches) > 1) {
        usethis::ui_done(glue::glue(
            "Found {usethis::ui_value(length(matches))} possible tools: ",
            "{usethis::ui_value(matches)}"
        ))
        match <- prompt_menu("Which tool would you like to display?", matches)
        cat("\n")
        database$Tools[[match]]
    } else {
        usethis::ui_done("No matches found!")
    }
}

#' Search name
#'
#' Search for matching names in the database
#'
#' @param database Database object
#' @param name Name of the tool to update
search_name <- function(database, name) {

    matches <- stringdist::stringsim(
        tolower(name),
        tolower(names(database$Tools)),
        method = "jw", p = 0.1
    ) > 0.9

    matches_names <- names(database$Tools)[matches]

    return(matches_names)
}
