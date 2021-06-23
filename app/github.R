#' Ping GitHub repository
#'
#' Ping a GitHub repository to check if it exists
#'
#' @param repo String giving the repository in the form "owner/repo"
#' @param newline Whether to start messages on a new line
#'
#' @return `TRUE` if the repository exists, `FALSE` otherwise
ping_gh_repo <- function(repo, newline = FALSE) {
    query <- glue::glue("GET /repos/{repo}")

    for (iter in seq(1:5)) {
        result <- try(gh::gh(query), silent = TRUE)
        if (!is(result, "try-error")) {
            return(TRUE)
        }
        Sys.sleep(1)
    }

    if (newline) {
        cat("\n\n")
    }
    usethis::ui_oops(glue::glue(
        "GitHub repository {usethis::ui_value(repo)} does not exist"
    ))
    
    return(FALSE)
}
