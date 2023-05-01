box::use(
  shiny[moduleServer, NS, eventReactive, renderPlot, plotOutput, validate, need],
  shiny.fluent[...],
  app / view / inputMenu,
  app / logic / options
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
    inputMenu$ui(id = ns("values")),
    plotOutput(ns("tree"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    params <- inputMenu$server(
      id = "values"
    )

    u <- eventReactive(params(), {
      with(params(), {
        (exp(sigma * sqrt(t / steps)))
      })
    })

    d <- eventReactive(u(), {
      1 / u()
    })

    p <- eventReactive(d(), {
      with(params(), {
        (exp(risk_free * (t / steps)) - d()) / (u() - d())
      })
    })

    tree_data <- eventReactive(p(), {
      with(
        params(),
        options$underlying_asset_value_matrix(
          s_0 = underlying,
          u   = u(),
          d   = d(),
          n   = steps
        )
      )
    })

    output$tree <- renderPlot({
      validate(
        need(
          expr    = p() <= 1,
          message = "WARNING - parameters must satisfy the following condition: d < e ^ r < u"
        )
      )
      
      options$plot_tree(
        data = tree_data(),
        n    = params()$steps
      )
      
    })

    shiny::observe({
      print(tree_data())
    })
  })
}
