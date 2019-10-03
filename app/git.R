#' Check git status
#'
#' Check if there are unstaged or uncommited files and warn the user
check_status <- function() {

    status <- git2r::status()

    if (length(status$staged) > 0 || length(status$unstaged) > 0) {
        usethis::ui_warn(glue::glue(
            "There are uncommited files. ",
            "You should commit these before continuing."
        ))

        cat("\n")
        end <- prompt_yn("Do you want to quit (y/n)?:")

        if (end) {
            quit()
        }
    }
}
