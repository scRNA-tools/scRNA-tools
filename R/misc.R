#' Prompt Y/N
#'
#' Prompt the user for a yes or no response
#'
#' @param prompt Text prompt to use
#'
#' @return TRUE or FALSE
prompt_yn <- function(prompt) {

    prompt <- paste0(prompt, "? (y/n): ")

    if (interactive()) {
        response <- readline(prompt)
    } else {
        cat(prompt)
        response <- readLines("stdin", n = 1)
    }

    if (response %in% c("y", "n")) {
        value <- ifelse(response == "y", TRUE, FALSE)
    } else {
        message("Please enter 'y' or 'n'")
        value <- prompt_yn(prompt)
    }

    return(value)
}
