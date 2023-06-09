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
        content = uiOutput(ns("tree_container"))
      )
    ),
    br(),
    Pivot(
      PivotItem(
        headerText = "European",
        Stack(
          tokens = list(childrenGap = 10),
          horizontal = TRUE,
          card_6(title = "Call", content = uiOutput(ns("call"))),
          card_6(title = "Put", content = uiOutput(ns("put")))
        )
      ),
      PivotItem(
        headerText = "American",
        Stack(
          tokens = list(childrenGap = 10),
          horizontal = TRUE,
          card_6(title = "Call", content = uiOutput(ns("call_american"))),
          card_6(title = "Put", content = uiOutput(ns("put_american")))
        )
      )
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

    output$tree <- renderPlot({
      validate(
        need( # change later the color to red of this validate message
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

    # > 2 American --------------------------------------------------------------------------------

    exercise_call <- eventReactive(tree_data(), {
      options$exercise_option(
        data = tree_data(),
        k    = params()$strike,
        type = "Call"
      )
    })

    exercise_put <- eventReactive(tree_data(), {
      options$exercise_option(
        data = tree_data(),
        k    = params()$strike,
        type = "Put"
      )
    })

    price_american_call <- eventReactive(exercise_call(), {
      with(params(), {
        options$price_american_option(
          data = exercise_call(),
          p    = p(),
          r    = risk_free,
          n    = steps,
          time = t
        )
      })
    })

    price_american_put <- eventReactive(exercise_put(), {
      with(params(), {
        options$price_american_option(
          data = exercise_put(),
          p    = p(),
          r    = risk_free,
          n    = steps,
          time = t
        )
      })
    })

    output$call_american <- renderUI({
      price_american_call() |> details_list()
    })

    output$put_american <- renderUI({
      price_american_put() |> details_list()
    })
  })
}
