box::use(
  shiny[moduleServer, NS, eventReactive, renderPlot, plotOutput, validate, need, div, tableOutput],
  shiny.fluent[...],
  app / view / inputMenu,
  app / logic / options,
  shinyjs,
  glue[glue]
)

makeCard <- function(title, content, size = 12, style = "") {
  div(
    class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", title, block = TRUE),
      content
    )
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluentPage(
    shinyjs$useShinyjs(),
    inputMenu$ui(id = ns("values")),
    plotOutput(ns("tree")),
    Stack(
      tokens = list(childrenGap = 10), horizontal = TRUE,
      makeCard("Call", div(style="max-height: 500px; overflow: auto")),
      makeCard("Put", tableOutput(ns("call")))
    )
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

    tree_plot <- eventReactive(tree_data(), {
      options$plot_tree(
        data = tree_data(),
        n    = params()$steps
      )
    })

    output$tree <- renderPlot({
      validate(
        need(
          expr    = p() <= 1,
          message = "WARNING - parameters must satisfy the following condition: d < e ^ r < u"
        )
      )
      tree_plot()
    })
  })
}