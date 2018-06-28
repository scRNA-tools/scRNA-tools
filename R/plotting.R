#' Plot number of tools
#'
#' Produces a HTML page with an interactive plot of the number of tools over
#' time
#'
#' @param swsheet Tibble containing software table
plot_number <- function(swsheet) {

    `%>%` <- magrittr::`%>%`

    datecount <- swsheet %>%
        dplyr::select(Date = Added) %>%
        dplyr::group_by(Date = as.Date(Date)) %>%
        dplyr::summarise(Count = n()) %>%
        tidyr::complete(Date = tidyr::full_seq(Date, 1),
                        fill = list(Count = 0)) %>%
        dplyr::mutate(Total = cumsum(Count))

    plot <- ggplot2::ggplot(datecount, ggplot2::aes(x = Date, y = Total)) +
        ggplot2::geom_line(size = 2, colour = "#7A52C7") +
        ggplot2::xlab("Date") +
        ggplot2::ylab("Number of tools") +
        ggplot2::scale_x_date(breaks = scales::pretty_breaks(10)) +
        ggplot2::ggtitle("Number of tools over time") +
        cowplot::theme_cowplot() +
        ggplot2::theme(plot.title   = ggplot2::element_text(size = 20),
                       axis.title.x = ggplot2::element_blank(),
                       axis.text    = ggplot2::element_text(size = 12),
                       axis.text.x  = ggplot2::element_text(angle = 60,
                                                            vjust = 0.5)
        )

    plot <- plotly::ggplotly(plot, dynamicTicks = TRUE) %>%
        plotly::layout(margin = list(l = 70, r = 40, b = 90, t = 50))

    htmlwidgets::saveWidget(widgetframe::frameableWidget(plot),
                            file.path(getwd(), "docs/plots/number.html"),
                            selfcontained = FALSE, libdir = "libraries")
}


#' Plot publication status
#'
#' Produces a HTML page with an interactive plot of the publication status of
#' tools in the database
#'
#' @param swsheet Tibble containing software table
plot_publication <- function(swsheet) {

    `%>%` <- magrittr::`%>%`

    plot <- swsheet %>%
        dplyr::mutate(HasPub = purrr::map(.$Refs,
                                          function(x) {
                                              nrow(x$Publications) > 0
                                          })) %>%
        dplyr::mutate(HasPub = purrr::map(.$HasPub,
                                          function(x) {
                                              ifelse(length(x) == 0, FALSE, x)
                                          }),
                      HasPub = purrr::flatten_lgl(HasPub)) %>%
        dplyr::mutate(HasPre = purrr::map(.$Refs,
                                          function(x) {
                                              nrow(x$Preprints) > 0
                                          })) %>%
        dplyr::mutate(HasPre = purrr::map(.$HasPre,
                                          function(x) {
                                              ifelse(length(x) == 0, FALSE, x)
                                          }),
                      HasPre = purrr::flatten_lgl(HasPre),
                      HasPre = HasPre & !HasPub) %>%
        dplyr::mutate(HasNot = !HasPub & !HasPre) %>%
        dplyr::summarise(NotPublished = sum(HasNot),
                         Published = sum(HasPub),
                         Preprint = sum(HasPre)) %>%
        tidyr::gather(key = Type, value = Count) %>%
        dplyr::mutate(Type = factor(Type,
                                    levels = c("Published", "Preprint",
                                               "NotPublished"),
                                    labels = c("Published", "Preprint",
                                               "Not Published"))) %>%
        dplyr::arrange(Type) %>%
        dplyr::mutate(Cumulative = cumsum(Count),
                      Midpoint = Cumulative - (Count / 2),
                      Midpoint = max(Cumulative) - Midpoint,
                      Percent = round(Count / sum(Count) * 100, 1),
                      Label = paste0(Type, " ", Percent, "%")) %>%
        ggplot2::ggplot(ggplot2::aes(x = 1, fill = Type, weight = Count,
                                     text = paste("Percent:", Percent))) +
        ggplot2::geom_bar(width = 1, position = "stack") +
        ggplot2::geom_text(ggplot2::aes(x = 1, y = Midpoint, label = Label),
                           size = 6, colour = "white") +
        ggplot2::scale_fill_manual(values = c("#EC008C", "#00ADEF",
                                              "#8DC63F")) +
        ggplot2::scale_colour_manual(values = c("#EC008C", "#00ADEF",
                                                "#8DC63F")) +
        ggplot2::ggtitle("Publication status") +
        cowplot::theme_nothing() +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 20,
                                                          face = "bold"),
                       legend.position = "none"
        )

    plot <- plotly::ggplotly(plot, tooltip = c("fill", "weight", "text")) %>%
        plotly::layout(margin = list(l = 100, r = 100, b = 20, t = 50))

    htmlwidgets::saveWidget(widgetframe::frameableWidget(plot),
                            file.path(getwd(), "docs/plots/publication.html"),
                            selfcontained = FALSE, libdir = "libraries")
}

#' Plot licenses
#'
#' Produces a HTML page with an interactive plot of the licenses used by tools
#' in the database
#'
#' @param swsheet Tibble containing software table
plot_licenses <- function(swsheet) {

    `%>%` <- magrittr::`%>%`

    plot <- swsheet %>%
        dplyr::select(License) %>%
        dplyr::mutate(IsGPL = stringr::str_detect(License, "GPL"),
                      IsBSD = stringr::str_detect(License, "BSD"),
                      IsMIT = stringr::str_detect(License, "MIT"),
                      IsApache = stringr::str_detect(License, "Apache"),
                      IsArtistic = stringr::str_detect(License, "Artistic"),
                      IsUnknown = is.na(License),
                      IsOther = !(IsGPL | IsBSD | IsMIT | IsApache |
                                      IsArtistic | IsUnknown)) %>%
        dplyr::summarise(Apache = sum(IsApache, na.rm = TRUE),
                         Artistic = sum(IsArtistic, na.rm = TRUE),
                         BSD = sum(IsBSD, na.rm = TRUE),
                         GPL = sum(IsGPL, na.rm = TRUE),
                         MIT = sum(IsMIT, na.rm = TRUE),
                         Other = sum(IsOther),
                         Unknown = sum(IsUnknown)) %>%
        tidyr::gather(key = License, value = Count) %>%
        dplyr::mutate(License = factor(License,
                                       levels = c("Apache", "Artistic", "BSD",
                                                  "GPL", "MIT", "Other",
                                                  "Unknown")),
                      Percent = round(Count / sum(Count) * 100, 1),
                      Label = paste0(License, "\n", Percent, "%")) %>%
        ggplot2::ggplot(ggplot2::aes(x = License, weight = Count,
                                     fill = License,
                                     text = paste("Percent:", Percent))) +
        ggplot2::geom_bar(width = 0.95, position = "dodge") +
        ggplot2::geom_text(ggplot2::aes(x = License,
                                        y = Count + nrow(swsheet) * 0.05,
                                        label = Label, colour = License),
                           size = 5) +
        ggplot2::scale_fill_manual(values = c("#EC008C", "#00ADEF", "#8DC63F",
                                              "#00B7C6", "#F47920", "#7A52C7",
                                              "#999999")) +
        ggplot2::scale_colour_manual(values = c("#EC008C", "#00ADEF", "#8DC63F",
                                                "#00B7C6", "#F47920", "#7A52C7",
                                                "#999999")) +
        ggplot2::ggtitle("Software licenses") +
        cowplot::theme_nothing() +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 20,
                                                          face = "bold"),
                       legend.position = "none"
        )

    plot <- plotly::ggplotly(plot, tooltip = c("x", "weight", "text")) %>%
        plotly::layout(margin = list(l = 10, r = 10, b = 20, t = 80))

    htmlwidgets::saveWidget(widgetframe::frameableWidget(plot),
                            file.path(getwd(), "docs/plots/licenses.html"),
                            selfcontained = FALSE, libdir = "libraries")
}


#' Plot platforms
#'
#' Produces a HTML page with an interactive plot of the platforms used by tools
#' in the database
#'
#' @param swsheet Tibble containing software table
plot_platforms <- function(swsheet) {

    plot <- swsheet %>%
        dplyr::select(Platform) %>%
        dplyr::mutate(IsR = stringr::str_detect(Platform, "R"),
                      IsPython = stringr::str_detect(Platform, "Python"),
                      IsMATLAB = stringr::str_detect(Platform, "MATLAB"),
                      IsCPP = stringr::str_detect(Platform, "C++"),
                      IsOther = !(IsR | IsPython | IsMATLAB | IsCPP)) %>%
        dplyr::summarise(R = sum(IsR),
                         Python = sum(IsPython),
                         MATLAB = sum(IsMATLAB),
                         CPP = sum(IsCPP),
                  Other = sum(IsOther)) %>%
        tidyr::gather(key = Platform, value = Count) %>%
        dplyr::mutate(Platform = factor(Platform,
                                        levels = c("R", "Python", "MATLAB",
                                                   "CPP", "Other"),
                                        labels = c("R", "Python", "MATLAB",
                                                   "C++", "Other")),
                      Percent = round(Count / sum(Count) * 100, 1),
                      Label = paste0(Platform, "\n", Percent, "%")) %>%
        ggplot2::ggplot(ggplot2::aes(x = Platform, weight = Count,
                                     fill = Platform,
                                     text = paste("Percent:", Percent))) +
        ggplot2::geom_bar(width = 0.95, position = "dodge") +
        ggplot2::geom_text(ggplot2::aes(x = Platform,
                                        y = Count + nrow(swsheet) * 0.05,
                                        label = Label, colour = Platform),
                           size = 5) +
        ggplot2::scale_fill_manual(values = c("#EC008C", "#00ADEF", "#8DC63F",
                                              "#00B7C6", "#F47920", "#7A52C7",
                                              "#999999")) +
        ggplot2::scale_colour_manual(values = c("#EC008C", "#00ADEF", "#8DC63F",
                                                "#00B7C6", "#F47920", "#7A52C7",
                                                "#999999")) +
        ggplot2::ggtitle("Platforms") +
        cowplot::theme_nothing() +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 20,
                                                          face = "bold"),
                       legend.position = "none"
        )

    plot <- plotly::ggplotly(plot, tooltip = c("x", "weight", "text")) %>%
        plotly::layout(margin = list(l = 10, r = 10, b = 20, t = 80))

    htmlwidgets::saveWidget(widgetframe::frameableWidget(plot),
                            file.path(getwd(), "docs/plots/platforms.html"),
                            selfcontained = FALSE, libdir = "libraries")
}


#' Plot categories
#'
#' Produces a HTML page with an interactive plot showing the percentage of tools
#' in each category
#'
#' @param swsheet Tibble containing software table
plot_categories <- function(swsheet) {

    `%>%` <- magrittr::`%>%`

    catcounts <- swsheet %>%
        dplyr::summarise_at(8:39, sum) %>%
        tidyr::gather(key = Category, value = Count) %>%
        dplyr::arrange(-Count, Category) %>%
        dplyr::mutate(Prop = Count / nrow(swsheet)) %>%
        dplyr::mutate(Category = stringr::str_replace_all(Category,
                                                          "([[:upper:]])",
                                                          " \\1")) %>%
        dplyr::mutate(Category = stringr::str_trim(Category)) %>%
        dplyr::mutate(Category = ifelse(Category == "U M Is",
                                        "UMIs", Category)) %>%
        dplyr::mutate(Category = factor(Category, levels = Category)) %>%
        dplyr::mutate(Percent = round(Prop * 100, 1))

    plot <- ggplot2::ggplot(catcounts,
                            ggplot2::aes(x = Category, weight = Prop,
                                         text = paste("Count:", Count, "\n",
                                                      "Percent:", Percent))) +
        ggplot2::geom_bar(fill = "#7A52C7") +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::ylab("Percentage of tools") +
        ggplot2::ggtitle("Categories") +
        cowplot::theme_cowplot() +
        ggplot2::theme(axis.title.x = ggplot2::element_blank(),
              legend.position = "none",
              legend.title    = ggplot2::element_text(size = 14),
              legend.text     = ggplot2::element_text(size = 12),
              legend.key.size = ggplot2::unit(25, "points"),
              plot.title      = ggplot2::element_text(size = 20),
              axis.text       = ggplot2::element_text(size = 12),
              axis.text.x     = ggplot2::element_text(angle = 60, hjust = 1,
                                                      vjust = 0.5)
        )

    plot <- plotly::ggplotly(plot, tooltip = c("x", "text")) %>%
        plotly::layout(margin = list(l = 80, r = 10, b = 200, t = 50))

    htmlwidgets::saveWidget(widgetframe::frameableWidget(plot),
                            file.path(getwd(), "docs/plots/categories.html"),
                            selfcontained = FALSE, libdir = "libraries")
}
