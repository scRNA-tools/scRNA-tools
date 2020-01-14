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
    for (name in names(database$Tools)) {
        pb$tick()
        tool <- database$Tools[[name]]
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
