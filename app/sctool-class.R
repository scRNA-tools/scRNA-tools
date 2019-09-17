#' New sctool
#'
#' Create a new `sctool` object for storing a tool in the scRNA-tools database
#'
#' @param name Name of the tool
#' @param platform Platforms used by the tool (e.g. "R/Python")
#' @param code URL to where the tool code can be found
#' @param license Software license for the tool
#' @param description Description of the tool
#' @param dois Vector of DOIs associated with the tool
#' @param categories Analysis categories for the tool
#' @param bioc Repository for the tool on Bioconductor
#' @param cran Repository for the tool on CRAN
#' @param pypi Repository for the tool on PyPI
#' @param conda Repository for the tool on Conda
#' @param github Repository for the tool on GitHub
#' @param ignored Vector of ignored repositories for the tool
#' @param added Date the tool was added to the database
#' @param updated Date the tool entry was last updated
#'
#' @return `sctool` S3 object
new_sctool <- function(name, platform, code, license, description, dois,
                       categories,  bioc = NA, cran = NA, pypi = NA, conda = NA,
                       github = NA, ignored = NA,
                       added = lubridate::today("UTC"),
                       updated = lubridate::today("UTC")) {

    tool <- list(
        Tool         = name,
        Platform     = platform,
        Code         = code,
        License      = license,
        Description  = description,
        DOIs         = dois,
        Repositories = c(
            Bioc   = bioc,
            CRAN   = cran,
            PyPI   = pypi,
            Conda  = conda,
            GitHub = github
        ),
        Ignored      = ignored,
        Categories   = categories,
        Added        = added,
        Updated      = updated
    )

    structure(tool, class = "sctool")
}

print.sctool <- function(x) {

    repos <- x$Repositories[!is.na(x$Repositories)]
    print(repos)

    cat(usethis::ui_field(x$Tool), "\n\n")
    cat(usethis::ui_value(x$Description), "\n\n")
    cat(usethis::ui_field("Platform:"), usethis::ui_value(x$Platform), "\n")
    cat(usethis::ui_field("Code:"), usethis::ui_value(x$Code), "\n")
    cat(usethis::ui_field("License:"), usethis::ui_value(x$License), "\n")
    cat(usethis::ui_field("DOIs:"), usethis::ui_value(x$DOIs), "\n")
    cat(usethis::ui_field("Repositories:"), "\n")
    for (idx in seq_along(repos)) {
        cat("\t")
        cat(glue::glue(
                "{usethis::ui_field(names(repos[idx]))}: ",
                "{usethis::ui_value(repos[idx])}"
        ))
        cat("\n")
    }
    if (length(x$Ignored) > 0) {
        cat("\t")
        cat(usethis::ui_field("Ignored:"), usethis::ui_value(x$Ignored), "\n")
    }
    cat(usethis::ui_field("Categories:"), usethis::ui_value(x$Categories), "\n")
    cat(usethis::ui_field("Added:"), usethis::ui_value(x$Added),
        "\t",
        usethis::ui_field("Updated:"), usethis::ui_value(x$Updated), "\n")

}
