#' Arrange string
#'
#' Order a tbl by a string column.
#'
#' @param .data A tbl to order.
#' @param col Unquoted name of the column to sort by.
#' @param ... Unquoted names of other variables passed to `dplyr::arrange`
#'
#' @details
#' 
#' Uses `stringr::str_sort()` to do the sort to make it independent of the
#' system locale.
#'
#' @return Arranged tbl
arrange_str <- function(.data, col, ...) {
    
    col_data <- dplyr::pull(.data, {{ col }})
    arrangement <- unique(stringr::str_sort(col_data))
    
    dplyr::arrange(.data, factor({{ col }}, levels = arrangement), ...)
}
