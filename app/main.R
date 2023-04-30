box::use(
  shiny[moduleServer, NS],
  shiny.fluent[...],
  app / view / inputMenu,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluentPage(
    CommandBar(
      items = list(),
      farItems = list(
        list(
          id        = "icon_settings",
          key       = "settings",
          text      = "Settings",
          ariaLabel = "Settings",
          iconOnly  = TRUE,
          iconProps = list(iconName = "Settings")
        )
      )
    ),
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
