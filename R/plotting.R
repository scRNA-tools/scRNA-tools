#' Plot number of tools
#'
#' Produces a HTML page with an interactive plot of the number of tools over
#' time
#'
#' @param swsheet Tibble containing software table
plot_number <- function(swsheet) {

    datecount <- swsheet %>%
        select(Date = Added) %>%
        group_by(Date = as.Date(Date)) %>%
        summarise(Count = n()) %>%
        complete(Date = full_seq(Date, 1), fill = list(Count = 0)) %>%
        mutate(Total = cumsum(Count))

    plot <- ggplot(datecount, aes(x = Date, y = Total)) +
        geom_line(size = 2, colour = "#7A52C7") +
        xlab("Date") +
        ylab("Number of tools") +
        scale_x_date(breaks = scales::pretty_breaks(10)) +
        ggtitle("Number of tools over time") +
        theme_cowplot() +
        theme(plot.title = element_text(size = 20),
              axis.title.x = element_blank(),
              axis.text = element_text(size = 12),
              axis.text.x = element_text(angle = 60, vjust = 0.5)
        )

    plot <- ggplotly(plot, dynamicTicks = TRUE) %>%
        layout(margin = list(l = 70, r = 40, b = 90, t = 50))

    saveWidget(frameableWidget(plot),
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
    plot <- swsheet %>%
        mutate(HasPub = map(.$Refs, function(x) {nrow(x$Publications) > 0})) %>%
        mutate(HasPub = map(.$HasPub,
                            function(x) {ifelse(length(x) == 0, FALSE, x)}),
               HasPub = flatten_lgl(HasPub)) %>%
        mutate(HasPre = map(.$Refs, function(x) {nrow(x$Preprints) > 0})) %>%
        mutate(HasPre = map(.$HasPre,
                            function(x) {ifelse(length(x) == 0, FALSE, x)}),
               HasPre = flatten_lgl(HasPre),
               HasPre = HasPre & !HasPub) %>%
        mutate(HasNot = !HasPub & !HasPre) %>%
        summarise(NotPublished = sum(HasNot),
                  Published = sum(HasPub),
                  Preprint = sum(HasPre)) %>%
        gather(key = Type, value = Count) %>%
        mutate(Type = factor(Type,
                             levels = c("Published", "Preprint",
                                        "NotPublished"),
                             labels = c("Published", "Preprint",
                                        "Not Published"))) %>%
        arrange(Type) %>%
        mutate(Cumulative = cumsum(Count),
               Midpoint = Cumulative - (Count / 2),
               Midpoint = max(Cumulative) - Midpoint,
               Percent = round(Count / sum(Count) * 100, 1),
               Label = paste0(Type, " ", Percent, "%")) %>%
        ggplot(aes(x = 1, fill = Type, weight = Count,
                   text = paste("Percent:", Percent))) +
        geom_bar(width = 1, position = "stack") +
        geom_text(aes(x = 1, y = Midpoint, label = Label),
                  size = 6, colour = "white") +
        scale_fill_manual(values = c("#EC008C", "#00ADEF", "#8DC63F")) +
        scale_colour_manual(values = c("#EC008C", "#00ADEF", "#8DC63F")) +
        ggtitle("Publication status") +
        theme_nothing() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              legend.position = "none"
        )

    plot <- ggplotly(plot, tooltip = c("fill", "weight", "text")) %>%
        layout(margin = list(l = 100, r = 100, b = 20, t = 50))

    saveWidget(frameableWidget(plot),
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
    plot <- swsheet %>%
        select(License) %>%
        mutate(IsGPL = str_detect(License, "GPL"),
               IsBSD = str_detect(License, "BSD"),
               IsMIT = str_detect(License, "MIT"),
               IsApache = str_detect(License, "Apache"),
               IsArtistic = str_detect(License, "Artistic"),
               IsUnknown = is.na(License),
               IsOther = !(IsGPL | IsBSD | IsMIT | IsApache | IsArtistic |
                               IsUnknown)) %>%
        summarise(Apache = sum(IsApache, na.rm = TRUE),
                  Artistic = sum(IsArtistic, na.rm = TRUE),
                  BSD = sum(IsBSD, na.rm = TRUE),
                  GPL = sum(IsGPL, na.rm = TRUE),
                  MIT = sum(IsMIT, na.rm = TRUE),
                  Other = sum(IsOther),
                  Unknown = sum(IsUnknown)) %>%
        gather(key = License, value = Count) %>%
        mutate(License = factor(License,
                                levels = c("Apache", "Artistic", "BSD", "GPL",
                                           "MIT", "Other", "Unknown")),
               Percent = round(Count / sum(Count) * 100, 1),
               Label = paste0(License, "\n", Percent, "%")) %>%
        ggplot(aes(x = License, weight = Count, fill = License,
                   text = paste("Percent:", Percent))) +
        geom_bar(width = 0.95, position = "dodge") +
        geom_text(aes(x = License, y = Count + 4, label = Label,
                      colour = License), size = 5) +
        scale_fill_manual(values = c("#EC008C", "#00ADEF", "#8DC63F", "#00B7C6",
                                     "#F47920", "#7A52C7", "#999999")) +
        scale_colour_manual(values = c("#EC008C", "#00ADEF", "#8DC63F",
                                       "#00B7C6", "#F47920", "#7A52C7",
                                       "#999999")) +
        ggtitle("Software licenses") +
        theme_nothing() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              legend.position = "none"
        )

    plot <- ggplotly(plot, tooltip = c("x", "weight", "text")) %>%
        layout(margin = list(l = 10, r = 10, b = 20, t = 80))

    saveWidget(frameableWidget(plot),
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
        select(Platform) %>%
        mutate(IsR = str_detect(Platform, "R"),
               IsPython = str_detect(Platform, "Python"),
               IsMATLAB = str_detect(Platform, "MATLAB"),
               IsCPP = str_detect(Platform, "C++"),
               IsOther = !(IsR | IsPython | IsMATLAB | IsCPP)) %>%
        summarise(R = sum(IsR),
                  Python = sum(IsPython),
                  MATLAB = sum(IsMATLAB),
                  CPP = sum(IsCPP),
                  Other = sum(IsOther)) %>%
        gather(key = Platform, value = Count) %>%
        mutate(Platform = factor(Platform,
                                 levels = c("R", "Python", "MATLAB", "CPP",
                                            "Other"),
                                 labels = c("R", "Python", "MATLAB", "C++",
                                            "Other")),
               Percent = round(Count / sum(Count) * 100, 1),
               Label = paste0(Platform, "\n", Percent, "%")) %>%
        ggplot(aes(x = Platform, weight = Count, fill = Platform,
                   text = paste("Percent:", Percent))) +
        geom_bar(width = 0.95, position = "dodge") +
        geom_text(aes(x = Platform, y = Count + 4, label = Label,
                      colour = Platform), size = 5) +
        scale_fill_manual(values = c("#EC008C", "#00ADEF", "#8DC63F", "#00B7C6",
                                     "#F47920", "#7A52C7", "#999999")) +
        scale_colour_manual(values = c("#EC008C", "#00ADEF", "#8DC63F",
                                       "#00B7C6", "#F47920", "#7A52C7",
                                       "#999999")) +
        ggtitle("Platforms") +
        theme_nothing() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              legend.position = "none"
        )

    plot <- ggplotly(plot, tooltip = c("x", "weight", "text")) %>%
        layout(margin = list(l = 10, r = 10, b = 20, t = 80))

    saveWidget(frameableWidget(plot),
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

    catcounts <- swsheet %>%
        summarise_at(8:38, sum) %>%
        gather(key = Category, value = Count) %>%
        arrange(-Count, Category) %>%
        mutate(Prop = Count / nrow(swsheet)) %>%
        mutate(Category = str_replace_all(Category, "([[:upper:]])", " \\1")) %>%
        mutate(Category = str_trim(Category)) %>%
        mutate(Category = ifelse(Category == "U M Is", "UMIs", Category)) %>%
        mutate(Category = factor(Category, levels = Category)) %>%
        mutate(Percent = round(Prop * 100, 1))

    plot <- ggplot(catcounts,
                   aes(x = Category, weight = Prop,
                       text = paste("Count:", Count, "\n",
                                    "Percent:", Percent))) +
        geom_bar(fill = "#7A52C7") +
        scale_y_continuous(labels = scales::percent) +
        ylab("Percentage of tools") +
        ggtitle("Categories") +
        theme_cowplot() +
        theme(axis.title.x = element_blank(),
              legend.position = "none",
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12),
              legend.key.size = unit(25, "points"),
              plot.title = element_text(size = 20),
              axis.text = element_text(size = 12),
              axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5)
        )

    plot <- ggplotly(plot, tooltip = c("x", "text")) %>%
        layout(margin = list(l = 80, r = 10, b = 200, t = 50))

    saveWidget(frameableWidget(plot),
               file.path(getwd(), "docs/plots/categories.html"),
               selfcontained = FALSE, libdir = "libraries")
}
