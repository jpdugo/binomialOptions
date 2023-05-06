box::use(
  shiny[
    moduleServer, NS, eventReactive, renderPlot, plotOutput, validate, need, div, uiOutput,
    renderUI, br, reactiveValues
  ],
  dplyr[...],
  shiny.fluent[...],
  app / view / inputMenu,
  app / logic / options,
  shinyjs,
  tibble,
  glue[glue],
  rlang,
  purrr,
  stringr
)

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

card_6 <- purrr$partial(
  .f = make_card,
  size = 6,
  style = "overflow-y: auto;height: 500px;"
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluentPage(
    shinyjs$useShinyjs(),
    inputMenu$ui(id = ns("values")),
    Stack(
      make_card(
        title = "Tree",
        content = uiOutput(ns("tree_container"))
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
    plt_size <- reactiveValues(height = NULL)

    ns <- session$ns
    params <- inputMenu$server(
      id = "values",
      reactive_values = plt_size
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

    output$tree_container <- renderUI({
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
      data <- price_call() |> tibble$rownames_to_column()
      columns <- rlang$list2(
        list(
          key         = "rowname",
          name        = "n_succeses",
          fieldName   = "rowname",
          minWidth    = 50,
          maxWidth    = 100,
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
    })

    output$put <- renderUI({
      data <- price_put() |> tibble$rownames_to_column()
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
    })
  })
}
