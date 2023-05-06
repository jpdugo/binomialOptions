box::use(
  shiny[
    moduleServer, NS, eventReactive, renderPlot, plotOutput, validate, need, div, uiOutput,
    renderUI, br, reactiveValues, req
  ],
  dplyr[...],
  shiny.fluent[...],
  app / view / inputMenu,
  app / logic / options,
  app / logic / ui_funs[make_card, card_6, details_list],
  shinyjs,
  glue[glue]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluentPage(
    shinyjs$useShinyjs(),
    inputMenu$ui(id = ns("values")),
    Stack(
      make_card(
        title   = "Tree",
        content = uiOutput(ns("tree_container")),
        style   = "overflow-y: auto;height: auto;"
      )
    ),
    br(),
    Stack(
      tokens = list(childrenGap = 10),
      horizontal = TRUE,
      card_6(title = "Call", content = uiOutput(ns("call"))),
      card_6(title = "Put", content = uiOutput(ns("put")))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    plt_size <- reactiveValues(height = NULL, activate = NULL)

    ns <- session$ns
    params <- inputMenu$server(
      id = "values",
      reactive_values = plt_size
    )

    # 1 Data --------------------------------------------------------------------------------------

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

    # 2 Tree Plot ---------------------------------------------------------------------------------

    tree_plot <- eventReactive(tree_data(), {
      options$plot_tree(
        data = tree_data(),
        n    = params()$steps
      )
    })

    output$tree_container <- renderUI({
      req(plt_size$activate)
      plotOutput(
        outputId = ns("tree"),
        height   = as.numeric(plt_size$height)
      )
    })
    plotOutput(ns("tree"), height = 500)

    output$tree <- renderPlot({
      validate(
        need(
          expr    = p() <= 1,
          message = "WARNING - parameters must satisfy the following condition: d < e ^ r < u"
        )
      )
      tree_plot()
    })

    # 3 Options -----------------------------------------------------------------------------------

    # > 1 European  ---------------------------------------------------------------------------

    price_call <- eventReactive(tree_data(), {
      with(params(), {
        options$intermediate_nodes(
          data  = tree_data(),
          rf    = risk_free,
          time  = t,
          steps = steps,
          d     = d(),
          u     = u(),
          p     = p(),
          k     = strike,
          type  = "Call"
        )
      })
    })

    price_put <- eventReactive(tree_data(), {
      with(params(), {
        options$intermediate_nodes(
          data  = tree_data(),
          rf    = risk_free,
          time  = t,
          steps = steps,
          d     = d(),
          u     = u(),
          p     = p(),
          k     = strike,
          type  = "Put"
        )
      })
    })

    output$call <- renderUI({
      price_call() |> details_list()
    })

    output$put <- renderUI({
      price_put() |> details_list()
    })
  })
}
