#' Build website
#'
#' Build the scRNA-tools website from the database
#'
#' @param database Database object
#' @param pkgs_cache Packages cache object
#' @param data_dir Path to save JSON data files
#' @param plot_dir Path to save JSON plot files
#'
#' @return Updated database object
build <- function(database, pkgs_cache, data_dir, plot_dir) {

    usethis::ui_todo("Building website...")

    if (fs::file_exists(fs::path(data_dir, "build-timestamp.txt"))) {
        last_build <- readr::read_lines(
            fs::path(data_dir, "build-timestamp.txt")
        )
        last_build <- lubridate::as_datetime(last_build)

        usethis::ui_info(glue::glue(
            "Last build at {usethis::ui_value(last_build)}")
        )
    } else {
        last_build <- lubridate::ydm_hms("2000-01-01 00:00:00")
    }

    # Check repositories
    if (any(pkgs_cache$Added > last_build)) {

        new_pkgs_cache <- dplyr::filter(pkgs_cache, Added > last_build)

        usethis::ui_todo(glue::glue(
            "Checking {usethis::ui_value(nrow(new_pkgs_cache))} ",
            "new repositories..."
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
            database <- update_repositories(name, database, new_pkgs_cache,
                                            prompt = FALSE)
        }
        usethis::ui_done("Repositories updated")
    } else {
        usethis::ui_done("Repositories up to date")
    }

    # Update citations
    usethis::ui_todo("Updating citations...")
    references <- database$References
    n_checked <- 0
    n_updated <- 0
    pb <- progress::progress_bar$new(
        format = paste(
            "[:bar] :current/:total :percent",
            "Elapsed: :elapsedfull ETA: :eta"
        ),
        total = nrow(references),
        clear = FALSE
    )
    pb$tick(0)
    for (idx in seq_len(nrow(references))) {

        pb$tick()
        if (references$arXiv[idx]) {
            next
        }

        diff <- difftime(lubridate::now("UTC"), references$Timestamp[idx],
                         units = "days")
        diff <- as.numeric(diff)

        if (diff > references$Delay[idx]) {
            n_checked <- n_checked + 1

            for (i in 1:10) {
                tryCatch({
                    cites <- rcrossref::cr_citation_count(
                        references$DOI[idx]
                    )$count
                    break
                }, error = function(e) {
                    cat("\n")
                    usethis::ui_info(glue::glue(
                        "{usethis::ui_value(names(database$Tools)[idx])} ",
                        "attempt {usethis::ui_value(i)} failed"))
                })
            }
            Sys.sleep(sample(seq(0, 1, 0.1), 1))

            if (!is.na(cites) && cites > references$Citations[idx]) {
                n_updated <- n_updated + 1
                references$Citations[idx] <- cites
                # Start with random delay between 0.5 and 1.5 hours
                references$Delay[idx] <- runif(1, 0.5, 1.5) / 24
            } else {
                # Increase delay by the amount since the last update (+ noise)
                new_delay <- references$Delay[idx] + diff + rnorm(1)
                # New delay cannot be more than 30 days
                new_delay <- min(new_delay, 30)
                references$Delay[idx] <- min(
                    as.numeric(references$Delay[idx] + diff), 30
                )
            }

            # Set check timestamp
            references$Timestamp[idx] <- lubridate::now("UTC")
        }
    }
    database$References <- references
    usethis::ui_done(glue::glue(
        "Checked {usethis::ui_value(n_checked)} citations, ",
        "updated {usethis::ui_value(n_updated)}"
    ))

    # !!!!!!!!!!!! Get shields !!!!!!!!!!!!!

    # Output JSON
    usethis::ui_todo("Creating data files...")
    fs::dir_create(data_dir)
    save_table_json(database, data_dir)
    save_tools_json(database, data_dir)
    save_categories_json(database, data_dir)
    usethis::ui_done(glue::glue(
        "Data files written to {usethis::ui_path(data_dir)}"
    ))

    # Make plots
    usethis::ui_todo("Analysing database...")
    fs::dir_create(plot_dir)
    tools <- get_tools(database$Tools)
    save_number_plot(tools, plot_dir)
    save_pub_plot(database, plot_dir)
    save_platform_plot(tools, plot_dir)
    save_licenses_plot(tools, plot_dir)
    save_categories_plot(database, plot_dir)
    usethis::ui_done(glue::glue(
        "Plot files written to {usethis::ui_path(plot_dir)}"
    ))

    # !!!!!!!!!  Write footer !!!!!!!!!

    build_time <- lubridate::now("UTC")
    readr::write_lines(build_time, fs::path(data_dir, "build-timestamp.txt"))
    usethis::ui_done(glue::glue(
        "Build finished at {usethis::ui_value(build_time)}"
    ))

    return(database)
}

#' Save table JSON
#'
#' Save JSON file with data for the tools table
#'
#' @param database Database object
#' @param data_dir Path to the data directory
save_table_json <- function(database, data_dir) {

    `%>%` <- magrittr::`%>%`

    cat_idx <- get_cat_idx(database$Tools)
    cat_mat <- cat_idx %>%
        tidyr::spread(Category, -Tool) %>%
        dplyr::mutate_at(dplyr::vars(-Tool),
                         ~ dplyr::if_else(is.na(.), FALSE, TRUE))

    dois <- get_doi_idx(database$Tools) %>%
        dplyr::left_join(database$References, by = "DOI") %>%
        dplyr::group_by(Tool) %>%
        dplyr::summarise(
            DOIs      = paste(DOI, collapse = ";"),
            Dates     = paste(Date, collapse = ";"),
            Citations = sum(Citations)
        )

    tools <- purrr::map_dfr(database$Tools, function(.tool) {
        tibble::tibble(
            Tool        = .tool$Tool,
            Platform    = .tool$Platform,
            Code        = .tool$Code,
            Description = .tool$Description,
            License     = .tool$License,
            Categories  = list(.tool$Categories),
            Added       = .tool$Added,
            Updated     = .tool$Updated
        )
    }) %>%
        dplyr::left_join(dois, by = "Tool") %>%
        dplyr::left_join(cat_mat, by = "Tool")

    jsonlite::write_json(tools, fs::path(data_dir, "tools-table.json"),
                         pretty = TRUE)
}

#' Save tools JSON
#'
#' Save JSON file with data for the tools page
#'
#' @param database Database object
#' @param data_dir Path to the data directory
save_tools_json <- function(database, data_dir) {

    tools <- get_tools(database$Tools)
    pubs_list <- get_pubs_list(database)
    preprints_list <- get_preprints_list(database)

    tools <- dplyr::mutate(
        tools,
        Categories   = purrr::map(database$Tools, ~ .x$Categories),
        Publications = unname(pubs_list[Tool]),
        Preprints    = unname(preprints_list[Tool]),
        Citations    = purrr::map_dbl(Publications, ~ sum(.x$Citations)) +
            purrr::map_dbl(Preprints, ~ sum(.x$Citations, na.rm = TRUE)),
        NumPubs      = purrr::map_dbl(Publications, nrow),
        NumPreprints = purrr::map_dbl(Preprints, nrow),
        Bioc         = purrr::map_chr(database$Tools,
                                      ~ .x$Repositories["Bioc"]),
        CRAN         = purrr::map_chr(database$Tools,
                                      ~ .x$Repositories["CRAN"]),
        PyPI         = purrr::map_chr(database$Tools,
                                      ~ .x$Repositories["PyPI"]),
        Conda        = purrr::map_chr(database$Tools,
                                      ~ .x$Repositories["Conda"]),
        GitHub       = purrr::map_chr(database$Tools,
                                      ~ .x$Repositories["GitHub"])
    )

    jsonlite::write_json(tools, fs::path(data_dir, "tools.json"), pretty = TRUE)
}

#' Save categories JSON
#'
#' Save JSON file with data for the categories page
#'
#' @param database Database object
#' @param data_dir Path to the data directory
save_categories_json <- function(database, data_dir) {

    `%>%` <- magrittr::`%>%`

    cat_idx <- get_cat_idx(database$Tools)

    cat_idx <- dplyr::mutate(
        cat_idx,
        Bioc  = purrr::map_chr(Tool,
                               ~ database$Tools[[.x]]$Repositories["Bioc"]),
        CRAN  = purrr::map_chr(Tool,
                               ~ database$Tools[[.x]]$Repositories["CRAN"]),
        PyPI  = purrr::map_chr(Tool,
                               ~ database$Tools[[.x]]$Repositories["PyPI"]),
        Conda = purrr::map_chr(Tool,
                               ~ database$Tools[[.x]]$Repositories["Conda"])
    )

    categories <- tidyr::nest(
        cat_idx,
        Tools = c(Tool, Bioc, CRAN, PyPI, Conda)
    ) %>%
        # Convert from vctrs_list to list for compatibility with jsonlite
        dplyr::mutate(Tools = as.list(Tools)) %>%
        dplyr::left_join(database$Categories, by = "Category") %>%
        dplyr::select(Category, Description, Tools) %>%
        dplyr::arrange(Category)

    jsonlite::write_json(categories, fs::path(data_dir, "categories.json"),
                         pretty = TRUE)
}
