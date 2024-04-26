#' Build xlsx table output
#'
#' Function to output Excel tables - one sheet per question within a survey block. The function gets called in the Shiny app within the download handler.
#'
#' @param data List of dataframes that have been processed via `summarize_block()`
#' @param file File name of outputted Excel file
#'
#' @return Formatted tables in Excel
#' @export
#'
#' @examples
#' \dontrun{
#' xlsx_build(list, "filename.xlsx")
#' }
xlsx_build <- function(data, file) {
  data <- unlist(data,recursive=FALSE)
  wb <- openxlsx::createWorkbook()
  Map(function(data, nameofsheet){

    openxlsx::addWorksheet(wb, nameofsheet)
    openxlsx::writeData(wb, nameofsheet, data)
    wrap_format <- openxlsx::createStyle(wrapText = TRUE)
    openxlsx::addStyle(wb, sheet = nameofsheet, style = wrap_format, rows = 1:(nrow(data)+1), cols = 1:(ncol(data)-1), gridExpand = TRUE)

  }, data, names(data))

  ## Save workbook to excel file
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}
