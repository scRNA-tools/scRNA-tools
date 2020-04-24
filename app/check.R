#' Check repositories
#'
#' Check for new package repository matches
#'
#' @param database Database object
#' @param pkgs_cache Packages cache table
#' @param dir Database directory
#' @param all Whether to check all repositories
#'
#' @return Updated database object
check <- function(database, pkgs_cache, dir, all) {

    if (!all) {
        last_week <- lubridate::today("UTC") - 7

        all_repos <- pkgs_cache$Repository
        pkgs_cache <- dplyr::filter(pkgs_cache, Added > last_week)

        if (nrow(pkgs_cache) == 0) {
            usethis::ui_done("No new repositories to check")
            quit()
        }
    }

    usethis::ui_todo(glue::glue(
        "Checking {usethis::ui_value(nrow(pkgs_cache))} repositories..."
    ))

    pb <- progress::progress_bar$new(
        format = paste(
            "[:bar] :current/:total :percent",
            "Elapsed: :elapsedfull ETA: :eta"
        ),
        total = length(database$Tools),
        clear = FALSE
    )
    pb$tick(0)
    gh_idx <- which(names(database$Tools[[1]]$Repositories) == "GitHub")
    for (name in names(database$Tools)) {
        pb$tick()
        tool <- database$Tools[[name]]

        existing <- tool$Repositories[-gh_idx]
        existing <- existing[!is.na(existing)]

        for (type in names(existing)) {
            repo <- paste(existing[type], type, sep = "@")
            if (!(repo %in% all_repos)) {
                cat("\n\n")
                usethis::ui_todo(glue::glue(
                    "{usethis::ui_value(repo)} repository for ",
                    "{usethis::ui_value(tool$Tool)} not found in packages cache"
                ))
                cat("\n")
                remove <- prompt_yn(glue::glue(
                    "Do you want to remove {usethis::ui_value(repo)} ",
                    "from {usethis::ui_value(tool$Tool)} (y/n)?:"
                ))
                if (remove) {
                    tool$Repositories[[type]] <- NA
                    database$Tools[[name]] <- tool
                    usethis::ui_todo("Commiting removed repository...")
                    save_database(database, dir)
                    set_gitmessage_checkremove(name)
                    commit_database(dir)
                }
            }
        }

        database <- update_repositories(name, database, pkgs_cache,
                                        prompt = FALSE)
        checked_tool <- database$Tools[[name]]
        new_repo <- !identical(tool$Repositories, checked_tool$Repositories)
        if (new_repo) {
            usethis::ui_todo("Commiting new repository...")
            save_database(database, dir)
            set_gitmessage_check(name)
            commit_database(dir)
        }
    }
    usethis::ui_done("Repositories updated")

    set_gitmessage_checkdone()

    return(database)
}
