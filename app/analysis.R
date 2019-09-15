save_number_plot <- function(database, plot_dir) {

    `%>%` <- magrittr::`%>%`

    tools <- get_tools(database$Tools)

    datecount <- tools %>%
        dplyr::select(Date = Added) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(Count = dplyr::n()) %>%
        tidyr::complete(Date = tidyr::full_seq(Date, 1),
                        fill = list(Count = 0)) %>%
        dplyr::mutate(Total = cumsum(Count))

    plot <- ggplot2::ggplot(datecount, ggplot2::aes(x = Date, y = Total)) +
        ggplot2::geom_line(size = 2, colour = "#7A52C7") +
        ggplot2::xlab("Date") +
        ggplot2::ylab("Number of tools") +
        ggplot2::scale_x_date(
            breaks = scales::pretty_breaks(10),
            labels = scales::date_format("%b %Y")
        ) +
        ggplot2::ggtitle("Number of tools over time") +
        cowplot::theme_cowplot() +
        ggplot2::theme(
            plot.title   = ggplot2::element_text(size = 20, hjust = 0.5),
            axis.title.x = ggplot2::element_blank(),
            axis.text    = ggplot2::element_text(size = 12),
            axis.text.x  = ggplot2::element_text(angle = 60, vjust = 0.5)
        )

    plot <- plotly::ggplotly(plot, dynamicTicks = TRUE, height = 600) %>%
        plotly::layout(margin = list(l = 70, r = 40, b = 90, t = 50)) %>%
        plotly::config(responsive = TRUE)

    json <- plotly::plotly_json(plot, jsonedit = FALSE)

    readr::write_lines(json, fs::path(plot_dir, "number.json"))
}

save_pub_plot <- function(database, plot_dir) {

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

    plot <- ggplot2::ggplot(pub_data,
                            ggplot2::aes(x = 1, y = Count, fill = Type)) +
        ggplot2::geom_col() +
        ggplot2::geom_text(ggplot2::aes(y = Midpoint, label = Label),
                           size = 6, colour = "white") +
        ggplot2::scale_fill_manual(
            values = c("#EC008C", "#00ADEF", "#8DC63F")
        ) +
        ggplot2::ggtitle("Publication status") +
        cowplot::theme_nothing() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(size = 20, face = "bold"),
            legend.position = "none"
        )

    plot <- plotly::ggplotly(plot, tooltip = c("fill", "y", "text"),
                             height = 600) %>%
        plotly::layout(margin = list(l = 100, r = 100, b = 20, t = 50)) %>%
        plotly::config(responsive = TRUE)

    json <- plotly::plotly_json(plot, jsonedit = FALSE)

    readr::write_lines(json, fs::path(plot_dir, "publication.json"))
}

save_platform_plot <- function(database, plot_dir) {

    `%>%` <- magrittr::`%>%`

    platforms <- get_tools(database$Tools) %>%
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
        ggplot2::ggtitle("Platforms") +
        cowplot::theme_nothing() +
        ggplot2::theme(
            plot.title      = ggplot2::element_text(size = 20, face = "bold"),
            legend.position = "none"
        )

    plot <- plotly::ggplotly(plot, tooltip = c("x", "y", "text"),
                             height = 600) %>%
        plotly::layout(margin = list(l = 10, r = 10, b = 20, t = 80)) %>%
        plotly::config(responsive = TRUE)

    json <- plotly::plotly_json(plot, jsonedit = FALSE)

    readr::write_lines(json, fs::path(plot_dir, "platforms.json"))
}

save_licenses_plot <- function(database, plot_dir) {

    `%>%` <- magrittr::`%>%`

    licenses <- get_tools(database$Tools) %>%
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
        ggplot2::ggtitle("Software licenses") +
        cowplot::theme_nothing() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(size = 20, face = "bold"),
            legend.position = "none"
        )

    plot <- plotly::ggplotly(plot, tooltip = c("x", "y", "text"),
                             height = 600) %>%
        plotly::layout(margin = list(l = 10, r = 10, b = 20, t = 80)) %>%
        plotly::config(responsive = TRUE)

    json <- plotly::plotly_json(plot, jsonedit = FALSE)

    readr::write_lines(json, fs::path(plot_dir, "licenses.json"))
}

save_categories_plot <- function(database, data_dir) {

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

    plot <- ggplot2::ggplot(
            cats,
            ggplot2::aes(
                x = Category, y = Prop,
                text = paste0("Count: ", Count, "\n", "Percent: ", Percent)
            )
        ) +
        ggplot2::geom_col(width = 0.95, fill = "#7A52C7") +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::ylab("Percentage of tools") +
        ggplot2::ggtitle("Categories") +
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

    plot <- plotly::ggplotly(plot, tooltip = c("x", "text"),
                             height = 600) %>%
        plotly::layout(margin = list(l = 80, r = 10, b = 20, t = 50)) %>%
        plotly::config(responsive = TRUE)

    json <- plotly::plotly_json(plot, jsonedit = FALSE)

    readr::write_lines(json, fs::path(plot_dir, "categories.json"))
}
