#' Save number plot
#'
#' Save a JSON file with the details needed to create a Plotly plot of the
#' number of tools over time
#'
#' @param tools Tools tibble
#' @param plot_dir Path to directory to save plot JSON
save_number_plot <- function(tools, plot_dir) {

    `%>%` <- magrittr::`%>%`

    datecount <- get_datecount(tools)

    gg <- ggplot2::ggplot(datecount, ggplot2::aes(x = Date, y = Total)) +
        ggplot2::geom_line(size = 2, colour = "#7A52C7") +
        ggplot2::xlab("Date") +
        ggplot2::ylab("Number of tools") +
        ggplot2::scale_x_date(
            breaks = scales::pretty_breaks(10),
            labels = scales::date_format("%b %Y")
        ) +
        # ggplot2::ggtitle("Number of tools over time") +
        cowplot::theme_cowplot() +
        ggplot2::theme(
            plot.title   = ggplot2::element_text(size = 20, hjust = 0.5),
            axis.title.x = ggplot2::element_blank(),
            axis.text    = ggplot2::element_text(size = 12),
            axis.text.x  = ggplot2::element_text(angle = 60, vjust = 0.5)
        )

    plot <- plotly::ggplotly(gg, dynamicTicks = TRUE, height = 600) %>%
        plotly::layout(margin = list(t = 40, r = 40, b = 100, l = 100),
                       autosize = TRUE) %>%
        plotly::config(responsive = TRUE)

    json <- plotly::plotly_json(plot, jsonedit = FALSE)

    readr::write_lines(json, fs::path(plot_dir, "number.json"))

    plot <- plotly::ggplotly(gg, height = 250, width = 400) %>%
        plotly::layout(margin = list(t = 0, r = 10, b = 0, l = 55),
                       xaxis = list(title = list(font = list(size = 9)),
                                    tickfont = list(size = 7),
                                    fixedrange = TRUE),
                       yaxis = list(title = list(font = list(size = 9)),
                                    tickfont = list(size = 7),
                                    fixedrange = TRUE)) %>%
        plotly::config(displayModeBar = FALSE)

    json <- plotly::plotly_json(plot, jsonedit = FALSE)

    readr::write_lines(json, fs::path(plot_dir, "number-index.json"))
}

#' Save publications plot
#'
#' Save a JSON file with the details needed to create a Plotly plot of the
#' publication status of tools
#'
#' @param database Database object
#' @param plot_dir Path to directory to save plot JSON
save_pub_plot <- function(tools, plot_dir) {

    `%>%` <- magrittr::`%>%`

    pub_data <- get_pub_data(database)

    plot <- ggplot2::ggplot(pub_data,
                            ggplot2::aes(x = 1, y = Count, fill = Type)) +
        ggplot2::geom_col() +
        ggplot2::geom_text(ggplot2::aes(y = Midpoint, label = Label),
                           size = 6, colour = "white") +
        ggplot2::scale_fill_manual(
            values = c("#EC008C", "#00ADEF", "#8DC63F")
        ) +
        # ggplot2::ggtitle("Publication status") +
        cowplot::theme_nothing() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(size = 20, face = "bold"),
            legend.position = "none"
        )

    plot <- plotly::ggplotly(plot, tooltip = c("fill", "y", "text"),
                             height = 600) %>%
        plotly::layout(margin = list(t = 50, r = 30, b = 50, l = 30),
                       autosize = TRUE) %>%
        plotly::config(responsive = TRUE)

    json <- plotly::plotly_json(plot, jsonedit = FALSE)

    readr::write_lines(json, fs::path(plot_dir, "publication.json"))
}

#' Save platform plot
#'
#' Save a JSON file with the details needed to create a Plotly plot of the
#' platforms used by tools
#'
#' @param tools Tools tibble
#' @param plot_dir Path to directory to save plot JSON
save_platform_plot <- function(tools, plot_dir) {

    `%>%` <- magrittr::`%>%`

    platforms <- get_platforms_data(tools)

    plot <- ggplot2::ggplot(
        platforms,
        ggplot2::aes(x = Platform, y = Count, fill = Platform,
                     colour = Platform, text = paste("Percent:", Percent))
        ) +
        ggplot2::geom_col(width = 0.95) +
        ggplot2::geom_text(
            ggplot2::aes(
                label = Label,
                y = dplyr::if_else(
                    Count == max(Count),
                    Count - sum(Count) * 0.05,
                    Count + sum(Count) * 0.05)
            ),
            size = 5,
        ) +
        ggplot2::scale_fill_manual(
            values = c("#EC008C", "#00ADEF", "#8DC63F", "#00B7C6", "#F47920",
                       "#7A52C7", "#999999")
        ) +
        ggplot2::scale_colour_manual(
            values = c("white", "#00ADEF", "#8DC63F", "#00B7C6", "#F47920",
                       "#7A52C7", "#999999")
        ) +
        # ggplot2::ggtitle("Platforms") +
        cowplot::theme_nothing() +
        ggplot2::theme(
            plot.title      = ggplot2::element_text(size = 20, face = "bold"),
            legend.position = "none"
        )

    plot <- plotly::ggplotly(plot, tooltip = c("x", "y", "text"),
                             height = 600) %>%
        plotly::layout(margin = list(t = 50, r = 30, b = 50, l = 30),
                       autosize = TRUE) %>%
        plotly::config(responsive = TRUE)

    json <- plotly::plotly_json(plot, jsonedit = FALSE)

    readr::write_lines(json, fs::path(plot_dir, "platforms.json"))
}

#' Save licenses plot
#'
#' Save a JSON file with the details needed to create a Plotly plot of the
#' tool software licenses
#'
#' @param tools Tools tibble
#' @param plot_dir Path to directory to save plot JSON
save_licenses_plot <- function(tools, plot_dir) {

    `%>%` <- magrittr::`%>%`

    licenses <- get_licenses_data(tools)

    plot <- ggplot2::ggplot(
            licenses,
            ggplot2::aes(x = License, y = Count, fill = License,
                         text = paste("Percent:", Percent)
            )
        ) +
        ggplot2::geom_col(width = 0.95) +
        ggplot2::geom_text(
            ggplot2::aes(
                y = dplyr::if_else(
                    Count == max(Count),
                    Count - sum(Count) * 0.05,
                    Count + sum(Count) * 0.05
                ),
                label = Label, colour = License),
            size = 5) +
        ggplot2::scale_fill_manual(
            values = c("#EC008C", "#00ADEF", "#8DC63F", "#00B7C6", "#F47920",
                       "#7A52C7", "#999999")
        ) +
        ggplot2::scale_colour_manual(
            values = c("white", "#00ADEF", "#8DC63F", "#00B7C6", "#F47920",
                       "#7A52C7", "#999999")
        ) +
        # ggplot2::ggtitle("Software licenses") +
        cowplot::theme_nothing() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(size = 20, face = "bold"),
            legend.position = "none"
        )

    plot <- plotly::ggplotly(plot, tooltip = c("x", "y", "text"),
                             height = 600) %>%
        plotly::layout(margin = list(t = 50, r = 30, b = 50, l = 30),
                       autosize = TRUE) %>%
        plotly::config(responsive = TRUE)

    json <- plotly::plotly_json(plot, jsonedit = FALSE)

    readr::write_lines(json, fs::path(plot_dir, "licenses.json"))
}

#' Save categories plot
#'
#' Save a JSON file with the details needed to create a Plotly plot of the
#' analysis categories
#'
#' @param database Database object
#' @param plot_dir Path to directory to save plot JSON
save_categories_plot <- function(database, data_dir) {

    `%>%` <- magrittr::`%>%`

    cats <- get_cats_data(database)

    gg <- ggplot2::ggplot(
            cats,
            ggplot2::aes(
                x = Category, y = Prop,
                text = paste0("Count: ", Count, "\n", "Percent: ", Percent)
            )
        ) +
        ggplot2::geom_col(width = 0.95, fill = "#7A52C7") +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::ylab("Percentage of tools") +
        # ggplot2::ggtitle("Categories") +
        cowplot::theme_cowplot() +
        ggplot2::theme(
            plot.title      = ggplot2::element_text(size = 20, face = "bold",
                                                    hjust = 0.5),
            axis.title.x    = ggplot2::element_blank(),
            legend.position = "none",
            legend.title    = ggplot2::element_text(size = 14),
            legend.key.size = ggplot2::unit(25, "points"),
            axis.text       = ggplot2::element_text(size = 12),
            axis.text.x     = ggplot2::element_text(
                angle = 60, hjust = 1, vjust = 0.5
            )
        )

    plot <- plotly::ggplotly(gg, tooltip = c("x", "text"),
                             height = 600) %>%
        plotly::layout(margin = list(t = 50, r = 30, b = 50, l = 70),
                       autosize = TRUE) %>%
        plotly::config(responsive = TRUE)

    json <- plotly::plotly_json(plot, jsonedit = FALSE)

    readr::write_lines(json, fs::path(plot_dir, "categories.json"))

    plot <- plotly::ggplotly(gg, height = 250, width = 500) %>%
        plotly::layout(margin = list(t = 0, r = 10, b = 0, l = 55),
                       xaxis = list(title = list(font = list(size = 9)),
                                    tickfont = list(size = 7),
                                    fixedrange = TRUE),
                       yaxis = list(title = list(font = list(size = 9)),
                                    tickfont = list(size = 7),
                                    fixedrange = TRUE)) %>%
        plotly::config(displayModeBar = FALSE)

    json <- plotly::plotly_json(plot, jsonedit = FALSE)

    readr::write_lines(json, fs::path(plot_dir, "categories-index.json"))
}

#' Get date count table
#'
#' Get a table of the count of tools in the database for each date
#'
#' @param tools Tools tibble
#'
#' @return Date count tibble
get_datecount <- function(tools) {

    `%>%` <- magrittr::`%>%`

    datecount <- tools %>%
        dplyr::select(Date = Added) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(Count = dplyr::n()) %>%
        tidyr::complete(Date = tidyr::full_seq(Date, 1),
                        fill = list(Count = 0)) %>%
        dplyr::mutate(Total = cumsum(Count))

    return(datecount)
}

#' Get publication data
#'
#' Get a table of publication data for plotting
#'
#' @param database Database object
#'
#' @return Publication data tibble
get_pub_data <- function(database) {

    `%>%` <- magrittr::`%>%`

    pub_status <- get_doi_idx(database$Tools) %>%
        dplyr::left_join(database$References, by = "DOI") %>%
        dplyr::group_by(Tool) %>%
        dplyr::summarise(
            Published = any(!Preprint),
            Preprint  = any(Preprint)
        )

    pub_data <- tibble::tibble(Tool = names(database$Tools)) %>%
        dplyr::left_join(pub_status, by = "Tool") %>%
        tidyr::replace_na(list(Preprint = FALSE, Published = FALSE)) %>%
        dplyr::mutate(Preprint = Preprint & !Published) %>%
        dplyr::mutate(None = !Preprint & !Published) %>%
        dplyr::summarise_if(is.logical, sum) %>%
        tidyr::gather(Type, Count) %>%
        dplyr::mutate(
            Type = factor(
                Type,
                levels = c("Published", "Preprint", "None"),
                labels = c("Published", "Preprint", "Not published"))) %>%
        dplyr::arrange(Type) %>%
        dplyr::mutate(
            Cumulative = cumsum(Count),
            Midpoint   = max(Cumulative) - (Cumulative - (Count / 2)),
            Percent    = round(Count / sum(Count) * 100, 1),
            Label      = paste0(Type, " ", Percent, "%")
        )

    return(pub_data)
}

#' Get platforms data
#'
#' Get a table of platforms data for plotting
#'
#' @param tools Tools tibble
#'
#' @return Platforms data tibble
get_platforms_data <- function(tools) {

    `%>%` <- magrittr::`%>%`

    platforms <- tools %>%
        dplyr::mutate(
            R      = stringr::str_detect(Platform, "R"),
            Python = stringr::str_detect(Platform, "Python"),
            MATLAB = stringr::str_detect(Platform, "MATLAB"),
            CPP    = stringr::str_detect(Platform, "C++"),
            Other  = !(R | Python | MATLAB | CPP)
        ) %>%
        dplyr::summarise_if(is.logical, sum) %>%
        tidyr::gather(Platform, Count) %>%
        dplyr::mutate(
            Platform = factor(
                Platform,
                levels = c("R", "Python", "MATLAB", "CPP", "Other"),
                labels = c("R", "Python", "MATLAB", "C++", "Other")
            ),
            Platform = forcats::fct_reorder(Platform, -Count),
            Percent  = round(Count / sum(Count) * 100, 1),
            Label    = paste0(Platform, "\n", Percent, "%")
        )

    return(platforms)
}

#' Get licenses data
#'
#' Get a table of licenses data for plotting
#'
#' @param tools Tools tibble
#'
#' @return Licenses data tibble
get_licenses_data <- function(tools) {

    `%>%` <- magrittr::`%>%`

    licenses <- tools %>%
        dplyr::mutate(
            GPL      = stringr::str_detect(License, "GPL"),
            BSD      = stringr::str_detect(License, "BSD"),
            MIT      = stringr::str_detect(License, "MIT"),
            Apache   = stringr::str_detect(License, "Apache"),
            Artistic = stringr::str_detect(License, "Artistic"),
            Unknown  = is.na(License),
            Other    = !(GPL | BSD | MIT | Apache | Artistic | Unknown)
        ) %>%
        dplyr::summarise_if(is.logical, sum, na.rm = TRUE) %>%
        tidyr::gather(License, Count) %>%
        dplyr::mutate(
            License = factor(
                License,
                levels = c("Apache", "Artistic", "BSD", "GPL", "MIT", "Other",
                           "Unknown")
            ),
            License = forcats::fct_reorder(License, -Count),
            License = forcats::fct_relevel(License, "Other", after = Inf),
            License = forcats::fct_relevel(License, "Unknown", after = Inf),
            Percent = round(Count / sum(Count) * 100, 1),
            Label   = paste0(License, "\n", Percent, "%")
        )

    return(licenses)
}

#' Get categories data
#'
#' Get a table of categories data for plotting
#'
#' @param tools Tools tibble
#'
#' @return Categories data tibble
get_cats_data <- function(database) {

    `%>%` <- magrittr::`%>%`

    cats <- get_cat_idx(database$Tools) %>%
        dplyr::group_by(Category) %>%
        dplyr::tally(sort = TRUE, name = "Count") %>%
        dplyr::mutate(
            Category = stringr::str_replace_all(
                Category, "([[:upper:]])", " \\1"
            ),
            Category = stringr::str_trim(Category),
            Category = dplyr::if_else(
                Category == "U M Is", "UMIs", Category
            ),
            Category = factor(Category, levels = Category),
            Prop     = Count / length(database$Tools),
            Percent  = round(Count / length(database$Tools) * 100, 1)
        )

    return(cats)
}
