ui_prompt <- function(prompt) {

    prompt <- glue::glue(usethis::ui_field(prompt), " ")

    if (interactive()) {
        response <- readline(prompt)
    } else {
        cat(prompt)
        response <- readLines("stdin", n = 1)
    }

    response <- stringr::str_trim(response)

    return(response)
}
