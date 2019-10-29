#' Show stats
#'
#' Show statistics about the database
#'
#' @param database Database object
#' @param show_plots Whether to show plots
show_stats <- function(database, show_plots) {

    `%>%` <- magrittr::`%>%`

    repositories <- get_repositories(database$Tools)
    tools <- get_tools(database$Tools)

    licenses_summ <- tools %>%
        tidyr::replace_na(list(License = "Unknown")) %>%
        dplyr::mutate(License = forcats::as_factor(License)) %>%
        dplyr::mutate(
            License = forcats::fct_lump(
                License,
                n = 8,
                other_level = "Other"
            ),
            License = forcats::fct_infreq(License),
            License = forcats::fct_relevel(License, "Other", after = Inf),
            License = forcats::fct_relevel(License, "Unknown", after = Inf)
        ) %>%
        dplyr::group_by(License) %>%
        dplyr::count(name = "Count") %>%
        dplyr::arrange(License)

    licenses_list <- as.list(licenses_summ$Count)
    names(licenses_list) <- as.character(licenses_summ$License)

    stats_list <- list(
        Tools        = length(database$Tools),
        References   = nrow(database$References),
        Categories   = nrow(database$Categories),
        Repositories = list(
            Bioc   = sum(!is.na(repositories$Bioc)),
            CRAN   = sum(!is.na(repositories$CRAN)),
            PyPI   = sum(!is.na(repositories$PyPI)),
            Conda  = sum(!is.na(repositories$Conda)),
            GitHub = sum(!is.na(repositories$GitHub))
        ),
        Licenses = licenses_list
    )

    cat("\n")
    usethis::ui_info("DATABASE STATISTICS")
    cat("\n")
    ui_pairs_list(stats_list)

    if (show_plots) {
        cat("\n")
        usethis::ui_info("NUMBER OF TOOLS OVER TIME")
        cat("\n")

        datecount <- get_datecount(tools)
        txtplot::txtplot(
            as.numeric(datecount$Date - min(datecount$Date)),
            datecount$Total,
            xlab = "Day",
            ylab = "Num tools"
        )

        cat("\n")
        usethis::ui_info("PUBLICATION STATUS")
        cat("\n")

        pub_data <- get_pub_data(database)
        plot_data <- factor(rep(pub_data$Type, times = pub_data$Count))
        txtplot::txtbarchart(
            plot_data,
            ylab = "Percent tools"
        )

        cat("\n")
        usethis::ui_info("PLATFORMS")
        cat("\n")

        platforms <- get_platforms_data(tools)
        plot_data <- factor(rep(platforms$Platform, times = platforms$Percent))
        txtplot::txtbarchart(
            plot_data,
            ylab = "Percent tools"
        )

        cat("\n")
        usethis::ui_info("LICENSES")
        cat("\n")

        licenses <- get_licenses_data(tools)
        plot_data <- factor(rep(licenses$License, times = licenses$Percent))
        txtplot::txtbarchart(
            plot_data,
            ylab = "Percent tools"
        )

        cat("\n")
        usethis::ui_info("CATEGORIES")
        cat("\n")

        cats <- get_cats_data(database)
        plot_data <- factor(rep(cats$Category, times = cats$Percent))
        txtplot::txtbarchart(
            plot_data,
            ylab = "Percent tools"
        )
    }
}
