box::use(
  shiny[
    NS, tagList, moduleServer, eventReactive, is.reactive, selectInput,
    observeEvent, req, renderText, textOutput, reactiveVal, tags
  ],
  shiny.fluent[...],
  glue[glue]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script(
      glue(
        "App.addClick('icon_settings', '{ns('show_panel')}')"
      )
    ),
    reactOutput(ns("react_panel"))
  )
}

#' @export
server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      is_panel_open <- reactiveVal(FALSE)
      output$react_panel <- renderReact({
        Panel(
          headerText = "Configuration Panel",
          isOpen = is_panel_open(),
          onDismiss = JS(paste0(
            "function() {",
            "  Shiny.setInputValue('", ns("hidePanel"), "', Math.random());",
            "}"
          )),
          list(
            ChoiceGroup.shinyInput(
              inputId = ns("opt_class"),
              label = "Option Class",
              value = 1,
              options = list(
                list(key = 1, text = "American"),
                list(key = 2, text = "European")
              )
            ),
            ChoiceGroup.shinyInput(
              inputId = ns("opt_type"),
              label = "Option Type",
              value = 1,
              options = list(
                list(key = 1, text = "Call"),
                list(key = 2, text = "Put")
              )
            ),
            Slider.shinyInput(
              inputId = ns("opt_underlying"),
              label   = "Underlying Asset Value",
              min     = 1,
              max     = 500,
              step    = 1,
              value   = 100
            ),
            Slider.shinyInput(
              inputId = ns("opt_strike"),
              label   = "Strike Price",
              min     = 1,
              max     = 500,
              step    = 1,
              value   = 100
            ),
            Slider.shinyInput(
              inputId = ns("opt_sigma"),
              label   = "Annual Volatility",
              min     = 0.01,
              max     = 1.5,
              step    = 0.01,
              value   = 0.15
            ),
            Slider.shinyInput(
              inputId = ns("opt_risk_free"),
              label   = "Risk Free Rate",
              min     = 0.01,
              max     = 0.5,
              step    = 0.01,
              value   = 0.03
            ),
            Slider.shinyInput(
              inputId = ns("opt_time"),
              label   = "T",
              min     = 0.1,
              max     = 5,
              step    = 0.1,
              value   = 0.6
            ),
            Slider.shinyInput(
              inputId = ns("opt_steps"),
              label   = "Number of Steps",
              min     = 1,
              max     = 100,
              step    = 1,
              value   = 3
            ),
            Toggle.shinyInput(
              inputId = ns("plt_deactivate"),
              label   = "Deactivate Plot",
              value   = FALSE
            ),
            TextField.shinyInput(
              inputId = ns("plt_width"),
              label   = "Plot Width (px)",
              value   = "500"
            ),
            TextField.shinyInput(
              inputId = ns("plt_height"),
              label   = "Plot Height (px)",
              value   = "500"
            )
          )
        )
      })
      observeEvent(input$show_panel, is_panel_open(TRUE))
      observeEvent(input$hidePanel, is_panel_open(FALSE))
    }
  )
}
