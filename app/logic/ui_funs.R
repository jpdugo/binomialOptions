box::use(
  shiny[div],
  shiny.fluent[...],
  dplyr[...],
  glue[glue],
  purrr,
  rlang,
  tibble,
  stringr
)

#' Create a Shiny card with specified title, content, size, and style
#'
#' This function creates a Shiny card using the shiny.fluent library with the specified title,
#' content, size, and style. It can be used to create visually appealing cards for a Shiny app.
#' The function was adapted from the example at:
#' https://appsilon.github.io/shiny.fluent/articles/shiny-fluent.html
#'
#' @param title \code{character} The title of the card
#' @param content \code{list} The content of the card (e.g., text, images, or other UI elements)
#' @param size \code{numeric} (optional) The size of the card, default is 12
#' @param style \code{character} (optional) Additional CSS styles for the card
#'
#' @return \code{shiny.tag} A Shiny card with the specified title, content, size, and style
#'
#' @family Shiny Card Functions
#'
#' @export
#'
#' @examples
#' library(shiny)
#' library(shiny.fluent)
#'
#' ui <- fluidPage(
#'   make_card(
#'     "Sample Card",
#'     "This is a sample card created using the make_card function.",
#'     size = 6
#'   )
#' )
#'
#' server <- function(input, output, session) {
#' }
#'
#' shinyApp(ui, server)
make_card <- function(title, content, size = 12, style = "") {
  div(
    class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", title, block = FALSE),
      content
    )
  )
}

#' @export
card_6 <- purrr$partial(
  .f = make_card,
  size = 6,
  style = "overflow-y: auto;height: 500px;"
)

#' Create a formatted list of details for a given dataset
#'
#' This function creates a formatted list of details for a given dataset with adjustable
#' column widths and customizable column names using the DetailsList component.
#' It can be used to display data in a user-friendly and customizable format.
#'
#' @param data \code{data.frame} The dataset for which the details list should be created
#'
#' @return \code{shiny.tag} A DetailsList component with a formatted list of details for
#' the given dataset
#'
#' @export
#'
#' @examples
#' sample_data <- data.frame(
#'   rowname = c("A", "B", "C"),
#'   col1    = c(1, 2, 3),
#'   col2    = c(4, 5, 6)
#' )
#'
#' details_list(sample_data)
details_list <- function(data) {
  data <- data |> tibble$rownames_to_column()
  columns <- rlang$list2(
    list(
      key         = "rowname",
      name        = "n_succeses",
      fieldName   = "rowname",
      minWidth    = 50,
      maxWidth    = NULL,
      isResizable = TRUE
    ),
    !!!purrr$imap(select(data, -rowname), \(x, y) {
      list(
        key         = y,
        name        = stringr::str_to_title(y),
        fieldName   = y,
        minWidth    = 50,
        maxWidth    = 100,
        isResizable = TRUE
      )
    })
  )

  DetailsList(
    items              = data,
    columns            = purrr$set_names(columns, NULL),
    checkboxVisibility = 2
  )
}
