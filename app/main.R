box::use(
  shiny[moduleServer, NS],
  shiny.fluent[...],
  app/view/inputMenu,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluentPage(
    inputMenu$ui(id = ns("values"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    inputMenu$server(
      id = "values"
    )
  })
}
