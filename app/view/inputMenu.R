box::use(
  shiny[
    NS, tagList, moduleServer, eventReactive, is.reactive, selectInput,
    observeEvent, req, renderText, textOutput, reactiveVal, tags, br, observe
  ],
  shiny.fluent[...],
  glue[glue],
  shinyjs
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    CommandBar(
      items = list(),
      farItems = list(
        list(
          id        = ns("icon_settings"),
          key       = "settings",
          text      = "Settings",
          ariaLabel = "Settings",
          iconOnly  = TRUE,
          iconProps = list(iconName = "Settings")
        )
      )
    ),
    Panel(
      headerText = "Configuration Panel",
      isOpen = TRUE,
      onDismiss = JS(
        "function hideElement() {
           $('.ms-Panel').css('visibility', 'hidden'); // improve this later
         }"
      ),
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
          max     = 25,
          step    = 1,
          value   = 5
        ),
        Toggle.shinyInput(
          inputId = ns("plt_deactivate"),
          label   = "Deactivate Plot",
          value   = FALSE
        ),
        Slider.shinyInput(
          inputId = ns("plt_height"),
          label   = "Plot Height (px)",
          min     = 200,
          max     = 1000,
          step    = 20,
          value   = 500
        ),
        br(),
        PrimaryButton.shinyInput(
          inputId = ns("run"),
          text    = "Calculate"
        )
      )
    )
  )
}

#' @export
server <- function(id, reactive_values) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observe(
        {
          shinyjs$runjs(
            glue(
              "App.addClick('{ns('icon_settings')}', '{ns('show_panel')}')"
            )
          )
        },
        autoDestroy = TRUE
      )

      observeEvent(input$show_panel, {
        shinyjs$runjs(
          "$('.ms-Panel').css('visibility', 'visible');"
        )
      })

      observeEvent(input$plt_height, {
        reactive_values$height <- input$plt_height
      })

      eventReactive(input$run,
        {
          list(
            class      = input$opt_class,
            underlying = input$opt_underlying,
            strike     = input$opt_strike,
            sigma      = input$opt_sigma,
            risk_free  = input$opt_risk_free,
            t          = input$opt_time,
            steps      = input$opt_steps
          )
        },
        ignoreNULL = FALSE
      )
    }
  )
}
